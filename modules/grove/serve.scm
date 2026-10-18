;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove serve)
  #:use-module (grove output)
  #:use-module (grove private cli progress)
  #:use-module (grove private build-lock)
  #:use-module (grove project diagnostic)
  #:use-module (grove private preview http)
  #:use-module (grove private preview watch)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (serve-project sweep-orphan-workspaces!))

(define (milliseconds)
  (quotient (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(define (remove-workspace directory)
  (when (file-exists? directory)
    (for-each
      (lambda (name)
        (let* ((path (string-append directory "/" name))
               (info (lstat path)))
          (if (eq? (stat:type info) 'directory)
              (remove-workspace path)
              (delete-file path))))
      (scandir directory (lambda (name) (not (member name '("." ".."))))))
    (rmdir directory)))

(define (sweep-orphan-workspaces!)
  ;;     A preview workspace whose .live lock can be taken has no live
  ;;     serve behind it - a hard-killed session's leftovers.  A
  ;;     missing .live marks a workspace from before the convention:
  ;;     a day untouched means nothing is using it.
  (let ((parent (or (getenv "TMPDIR") "/tmp")))
    (for-each
      (lambda (name)
        (let* ((path (string-append parent "/" name))
               (port (false-if-exception
                       (open-file (string-append path "/.live") "a"))))
          (if port
              (let ((orphan? (try-exclusive-lock port)))
                (release-lock port)
                (close-port port)
                (when orphan?
                  (format (current-error-port)
                          "Removing stale preview workspace ~a\n" path)
                  (remove-workspace path)))
              (let ((info (false-if-exception (lstat path))))
                (when (and info
                           (> (- (current-time) (stat:mtime info)) 86400))
                  (format (current-error-port)
                          "Removing stale preview workspace ~a\n" path)
                  (remove-workspace path))))))
      (scandir parent (lambda (name) (string-prefix? ".grove-preview-" name))))))

(define worker-code
  ;;     The worker must own its process group before it loads anything:
  ;;     teardown kills the group, and the group is where project
  ;;     subprocesses stay. Failing here is fatal rather than serving in
  ;;     the serve's group, where a group kill would be impossible.
  "(setpgid 0 0)
(unless (= (getpgrp) (getpid)) (exit 70))
(use-modules (grove private preview worker))
(exit (worker-main (cdr (command-line))))")

(define (join-load-path paths)
  (string-join (map absolute-path paths) ":"))

(define (worker-environment)
  ;;     Hand the worker the serve process's own resolved load paths
  ;;     instead of replaying them through -L/-C: the worker must
  ;;     resolve every module to the same file a manual build in this
  ;;     checkout resolves it to, because the shared build state
  ;;     records those resolved paths, and any difference in resolution
  ;;     turns into a full re-render. -L/-C replay does not survive
  ;;     Guile's path composition; the environment does.
  (cons (format #f "GUILE_LOAD_PATH=~a" (join-load-path %load-path))
        (cons (format #f "GUILE_LOAD_COMPILED_PATH=~a"
                      (join-load-path %load-compiled-path))
              (filter (lambda (entry)
                        (not (or (string-prefix? "GUILE_LOAD_PATH=" entry)
                                 (string-prefix? "GUILE_LOAD_COMPILED_PATH=" entry))))
                      (environ)))))

(define* (serve-project filename #:key (output-directory #f)
                                      (port 8080) (watch? #t) (debug? #f)
                                      (profile? #f))
  "Build in a resident worker process and preview the last successful
artifact set on localhost.  The worker stays alive across rebuilds and
retains its analysis forest; requests and results travel through files
in the workspace, numbered so both sides can tell stale from current.
Only an edit to a Scheme module or the project file restarts the
worker - its loaded modules and forest are only safe in the process
that loaded them."
  (let* ((filename (call-with-project-stage 'load filename
                     (lambda () (canonicalize-path filename))))
         (override (and output-directory (absolute-path output-directory)))
         (output (or override (string-append (dirname filename) "/build")))
         (roots (list (dirname filename)))
         (files (list filename))
         (modules '())
         (snapshot #f)
         (pid #f)
         (worker-group #f)
         (workspace #f)
         (live-port #f)
         (listener #f)
         (server-started? #f)
         (idle? #f)
         (serial 0)
         (active-serial #f)
         (sigint #f) (sigterm #f) (sigpipe #f))
    (define (request-path)
      (string-append workspace "/request.scm"))
    (define (result-path)
      (string-append workspace "/result.scm"))
    (define (kill-worker-group! group signal)
      (catch 'system-error (lambda () (kill (- group) signal)) (lambda _ #f)))
    (define (stop-worker!)
      ;; The group is the worker and everything it spawned.  Kill it
      ;; before reaping: the worker may already be gone while project
      ;; subprocesses still run in the group it led, and only the
      ;; retained group id can reach them then.
      (when worker-group
        (kill-worker-group! worker-group SIGKILL))
      (when pid
        (catch 'system-error (lambda () (kill pid SIGKILL)) (lambda _ #f))
        (waitpid pid)
        (set! pid #f))
      (set! worker-group #f))
    (define (reap-worker!)
      ;; A worker that died between builds must not receive new requests,
      ;; and its group dies with it: leftovers must not outlive the
      ;; worker that spawned them.  The group id is the worker's pid and
      ;; stays valid while any member remains, leader or not.
      (when pid
        (let ((finished (call-with-blocked-asyncs
                          (lambda () (waitpid pid WNOHANG)))))
          (unless (zero? (car finished))
            (kill-worker-group! pid SIGKILL)
            (set! pid #f)
            (set! worker-group #f)))))
    (define (spawn-worker!)
      (call-with-blocked-asyncs
        (lambda ()
          (let ((child
                  (call-with-input-file "/dev/null"
                    (lambda (input)
                      (spawn (string-append (assq-ref %guile-build-info 'bindir) "/guile")
                        (list "guile" "--no-auto-compile"
                              "-c" worker-code (request-path) (result-path))
                        #:input input
                        ;;     The worker inherits this environment, so it
                        ;;     reads and writes the same per-project cache
                        ;;     as a manual build: sharing the build state
                        ;;     and artifact manifest is what keeps a manual
                        ;;     build over a serve-written output
                        ;;     incremental instead of fully re-rendering.
                        ;;     The build lock serializes the writers, and
                        ;;     cache writes land through temp-file renames.
                        #:environment (worker-environment))))))
            ;;     worker-code sets the worker's group to its own pid
            ;;     before anything else; record it now so teardown can
            ;;     reach the group even after the leader is gone.  A
            ;;     group kill in the window before the worker creates
            ;;     its group fails harmlessly and falls back to the
            ;;     leader, which cannot have spawned anything yet.
            (set! pid child)
            (set! worker-group child)))))
    (define (start-build!)
      (set! serial (+ serial 1))
      (reap-worker!)
      (when (file-exists? (result-path)) (delete-file (result-path)))
      (let ((request-tmp (string-append workspace "/request.scm.tmp")))
        (call-with-output-file request-tmp
          (lambda (port)
            (write (list serial filename override modules roots files
                         debug? profile?)
                   port)))
        (rename-file request-tmp (request-path)))
      (set! active-serial serial)
      (set! idle? #f)
      (force-output)
      (unless pid (spawn-worker!)))
    (define (worker-stuck-on-module? pending)
      ;;     Module and project-file edits cannot be applied inside
      ;;     the resident worker: Guile will not reload a module into
      ;;     a running process, and the retained forest was built by
      ;;     the modules the process loaded.  Such an edit restarts
      ;;     the worker instead of becoming a request.
      (any (lambda (path)
             (or (string=? path filename)
                 (member path modules string=?)))
           pending))
    (define (start-http!)
      (listen listener 128)
      (set! server-started? #t)
      (print-cli-status "Serving" (format #f "http://127.0.0.1:~a/" port))
      (force-output))
    (define (accept-result! payload)
      (match payload
        (('preview echoed new-output new-roots new-files new-modules before data)
         (let ((table
                 (artifact-table
                   (map (lambda (entry)
                          (match entry
                            ((path (? string? text)) (make-artifact path text))
                            ((path bytes) (make-byte-artifact path bytes))))
                        data))))
           (set! output new-output)
           (set! roots new-roots)
           (set! files new-files)
           (set! modules new-modules)
           (set! snapshot table)
           (unless server-started? (start-http!))
           before))
        (_ (error "invalid preview worker result"))))
    (define (build-result-matches? payload)
      ;;     A build is in flight from start-build! until its result
      ;;     appears or the worker dies.  The result file only ever
      ;;     holds a complete generation - the worker renames it into
      ;;     place - so the serial match tells stale from current.
      (match payload
        (('preview (? (lambda (n) (and (integer? n) (= n active-serial)))) rest ...) #t)
        (_ #f)))
    (define (reap-failed-build!)
      (when pid
        (let ((finished
                (call-with-blocked-asyncs
                  (lambda () (waitpid pid WNOHANG)))))
          (unless (zero? (car finished))
            ;;     A build that dies with its worker leaves project
            ;;     subprocesses in the group the worker led; teardown
            ;;     owns that group, not just the leader.
            (kill-worker-group! pid SIGKILL)
            (set! pid #f)
            (set! worker-group #f)
            (set! active-serial #f)
            (cond
              ((not snapshot) (error "initial preview build failed"))
              (else
               (display "Build failed; serving the last successful outputs.\n"
                        (current-error-port))
               (force-output (current-error-port))))))))
    (define (cleanup!)
      (stop-worker!)
      (when (and listener (not (port-closed? listener))) (close-port listener))
      (when workspace (remove-workspace workspace))
      (when live-port (close-port live-port))
      (when sigint (sigaction SIGINT (car sigint) (cdr sigint)))
      (when sigterm (sigaction SIGTERM (car sigterm) (cdr sigterm)))
      (when sigpipe (sigaction SIGPIPE SIG_IGN)))
    (catch 'preview-stop
      (lambda ()
        (dynamic-wind
          (lambda ()
            (set! sigint (sigaction SIGINT (lambda (signal) (throw 'preview-stop signal))))
            (set! sigterm (sigaction SIGTERM (lambda (signal) (throw 'preview-stop signal))))
            (set! sigpipe (sigaction SIGPIPE SIG_IGN)))
          (lambda ()
            (sweep-orphan-workspaces!)
            (set! workspace (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                                                   "/.grove-preview-XXXXXX")))
            ;;     The .live lock says this workspace has a live
            ;;     serve: the startup sweep takes it without blocking
            ;;     to tell leftovers from sessions like this one, and
            ;;     the resident worker polls it to learn that its
            ;;     serve is gone.
            (set! live-port
              (open-file (string-append workspace "/.live") "a"))
            (lock-port-exclusive live-port)
            ;; Bind before building so an occupied port fails without rebuilding.
            (call-with-project-stage 'serve #f
              (lambda ()
                (set! listener (socket PF_INET SOCK_STREAM 0))
                (setsockopt listener SOL_SOCKET SO_REUSEADDR 1)
                (bind listener AF_INET INADDR_LOOPBACK port)))
            (start-build!)
            (let loop ((baseline (watch-snapshot roots files output))
                       (pending '()) (deadline 0) (next-scan 0))
              (let* ((now (milliseconds))
                     (scan? (and watch? (>= now next-scan)))
                     (current (if scan? (watch-snapshot roots files output) baseline))
                     (changes (snapshot-changes baseline current))
                     (pending (delete-duplicates (append pending changes) string=?))
                     (deadline (if (null? changes) deadline (+ now 200))))
                (when active-serial
                  (let ((payload (false-if-exception
                                   (and (file-exists? (result-path))
                                        (call-with-input-file (result-path) read)))))
                    (if (build-result-matches? payload)
                        (let* ((before (accept-result! payload))
                               (after (watch-snapshot roots files output))
                               (valid-paths (map car (append before after))))
                          ;;     Re-evaluate with the worker's actual
                          ;;     output directory and watch set: newly
                          ;;     created outputs must not trigger
                          ;;     rebuilds, and additions to the watch
                          ;;     set (modules and store objects first
                          ;;     recorded by this build) are not edits
                          ;;     either.  Stamp changes on paths both
                          ;;     snapshots knew are the periodic scan's
                          ;;     business - its baseline is fresher than
                          ;;     the request-time snapshot, and spanning
                          ;;     the build window here requeued a rebuild
                          ;;     for every file the build itself wrote.
                          ;;     Watch-set removals still count: a
                          ;;     source this build dropped from its set
                          ;;     is invisible to the periodic scan.
                          (set! pending
                            (delete-duplicates
                              (append (filter (lambda (path)
                                                (member path valid-paths string=?))
                                              pending)
                                      (filter-map (lambda (entry)
                                                    (and (assoc (car entry) before)
                                                         (not (assoc (car entry) after))
                                                         (car entry)))
                                                  before))
                              string=?))
                          (set! current after)
                          (set! active-serial #f))
                        (reap-failed-build!))))
                (when (and watch? (not active-serial) (pair? pending) (>= now deadline))
                  ;;     Name the triggers: chained rebuilds - a build's
                  ;;     own writes re-triggering the watcher - are
                  ;;     indistinguishable from real edits without this.
                  (format (current-error-port)
                          "Rebuilding (~a changed path~a):\n"
                          (length pending)
                          (if (= (length pending) 1) "" "s"))
                  (for-each
                    (lambda (path)
                      (format (current-error-port) "  ~a\n" path))
                    (take (sort pending string<?)
                          (min 12 (length pending))))
                  (force-output (current-error-port))
                  (when (worker-stuck-on-module? pending) (stop-worker!))
                  (start-build!)
                  (set! pending '()))
                (when (and watch? server-started? (not active-serial)
                           (null? pending) (not idle?))
                  (print-cli-status "Watching" "for changes (Ctrl-C to stop)" (current-error-port))
                  (set! idle? #t))
                (if server-started?
                    (let ((ready (select (list listener) '() '() 0 50000)))
                      (when (pair? (car ready))
                        (serve-ready-client listener
                          (make-preview-handler (lambda () snapshot)))))
                    (usleep 50000))
                (loop current pending deadline (if scan? (+ now 500) next-scan)))))
          cleanup!))
      (lambda (_ signal)
        (display "\nPreview stopped.\n")
        (+ 128 signal)))))
