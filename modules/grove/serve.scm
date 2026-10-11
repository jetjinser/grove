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
  "(setpgid 0 0) (use-modules (grove private preview worker)) (exit (worker-main (cdr (command-line))))")

(define (worker-arguments request-path result-path)
  (append (list "guile" "--no-auto-compile")
          (append-map (lambda (path) (list "-L" (absolute-path path)))
                      (reverse %load-path))
          (append-map (lambda (path) (list "-C" (absolute-path path)))
                      (reverse %load-compiled-path))
          (list "-c" worker-code request-path result-path)))

(define* (serve-project filename #:key (output-directory #f)
                                      (port 8080) (watch? #t) (debug? #f))
  "Build in fresh processes and preview the last successful artifact set on localhost."
  (let* ((filename (call-with-project-stage 'load filename
                     (lambda () (canonicalize-path filename))))
         (override (and output-directory (absolute-path output-directory)))
         (output (or override (string-append (dirname filename) "/build")))
         (roots (list (dirname filename)))
         (files (list filename))
         (modules '())
         (snapshot #f)
         (pid #f)
         (workspace #f)
         (live-port #f)
         (listener #f)
         (server-started? #f)
         (idle? #f)
         (sigint #f) (sigterm #f) (sigpipe #f))
    (define (start-build!)
      (let ((request (string-append workspace "/request.scm"))
            (result (string-append workspace "/result.scm")))
        (when (file-exists? result) (delete-file result))
        (call-with-output-file request
          (lambda (port)
            (write (list filename override modules roots files debug?) port)))
        (set! idle? #f)
        (force-output)
        (call-with-blocked-asyncs
          (lambda ()
            (set! pid
              (call-with-input-file "/dev/null"
                (lambda (input)
                  (spawn (string-append (assq-ref %guile-build-info 'bindir) "/guile")
                     (worker-arguments request result)
                     #:input input
                     #:environment
                     ;;     One stable cache for the workspace's
                     ;;     lifetime: entries are content-fingerprint
                     ;;     addressed, so rebuilds reuse everything
                     ;;     that has not changed instead of starting
                     ;;     from an empty generation.
                     (cons (format #f "XDG_CACHE_HOME=~a/cache" workspace)
                           (filter (lambda (entry)
                                     (not (string-prefix? "XDG_CACHE_HOME=" entry)))
                                   (environ)))))))))))
    (define (start-http!)
      (listen listener 128)
      (set! server-started? #t)
      (print-cli-status "Serving" (format #f "http://127.0.0.1:~a/" port))
      (force-output))
    (define (accept-result!)
      (match (call-with-input-file (string-append workspace "/result.scm") read)
        (('preview new-output new-roots new-files new-modules before data)
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
    (define (cleanup!)
      (when pid
        ;; The worker owns its process group, including project subprocesses.
        (catch 'system-error (lambda () (kill (- pid) SIGKILL))
          (lambda _ (catch 'system-error (lambda () (kill pid SIGKILL)) (lambda _ #f))))
        (waitpid pid)
        (set! pid #f))
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
            ;;     to tell leftovers from sessions like this one.
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
                (when pid
                  (let ((finished
                          (call-with-blocked-asyncs
                            (lambda ()
                              (let ((status (waitpid pid WNOHANG)))
                                (unless (zero? (car status)) (set! pid #f))
                                status)))))
                    (unless (zero? (car finished))
                      (cond
                        ((equal? (status:exit-val (cdr finished)) 0)
                         (let* ((before (accept-result!))
                                (after (watch-snapshot roots files output))
                                (valid-paths (map car (append before after))))
                           ;; Re-evaluate with the worker's actual output directory:
                           ;; newly created output files must not trigger rebuilds.
                           ;; Additions to the watch set itself (modules and store
                           ;; objects first recorded by this build) are not edits
                           ;; either - counting them made every serve start spend
                           ;; one full rebuild on nothing.
                           (set! pending
                             (delete-duplicates
                               (append (filter (lambda (path) (member path valid-paths string=?)) pending)
                                       (snapshot-changes before after
                                                         #:ignore-additions? #t))
                               string=?))
                           (set! current after)))
                        ((not snapshot) (error "initial preview build failed"))
                        (else
                         (display "Build failed; serving the last successful outputs.\n"
                                  (current-error-port))
                         (force-output (current-error-port)))))))
                (when (and watch? (not pid) (pair? pending) (>= now deadline))
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
                  (start-build!)
                  (set! pending '()))
                (when (and watch? server-started? (not pid) (null? pending) (not idle?))
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
