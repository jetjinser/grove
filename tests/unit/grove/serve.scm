;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove serve)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit grove subprocess)
  #:use-module (web client)
  #:use-module (web response))

(define root
  (format #f "~a/grove-serve-test-~a" (or (getenv "TMPDIR") "/tmp") (getpid)))
(define external (string-append root "-external"))
(define inputs (string-append root "-inputs"))
(define config (string-append root "/project.scm"))
(define article (string-append root "/sources/one.txt"))
(define helper (string-append root "/modules/probe/helper.scm"))
(define control (string-append root "/control"))
(define sources (getenv "GROVE_SRCDIR"))
(define processes '())
(define port #f)
(define (write-file path text)
  (call-with-output-file path (lambda (output) (display text output))))
(define (read-file path) (call-with-input-file path get-string-all))
(define (helper-text label)
  (format #f "(define-module (probe helper) #:export (label))\n(define label ~s)\n" label))
(define (rebuild-count)
  ;;     Serve names the changed paths that trigger each rebuild on
  ;;     stderr; the churn assertions below assert on the count.
  (let ((text (read-file (string-append root "/.stderr"))))
    (let loop ((start 0) (count 0))
      (let ((found (string-contains text "Rebuilding" start)))
        (if found (loop (+ found 1) (+ count 1)) count)))))
(define (touch-rewrite path)
  ;;     Same bytes through a fresh file: identity stamps change,
  ;;     contents do not, so neither caching nor compilation may care.
  ;;     Byte ports only - compiled files are binary.
  (let ((bytes (call-with-input-file path get-bytevector-all)))
    (call-with-output-file (string-append path ".churn")
      (lambda (output) (put-bytevector output bytes)))
    (rename-file (string-append path ".churn") path)))
(define (wait-for thunk)
  ;; Instruction tracing also runs inside rebuild workers. Keep the ordinary
  ;; responsiveness bound, but allow its instrumentation overhead in coverage.
  (let ((deadline (+ (get-internal-real-time)
                     (* (if (equal? (getenv "GROVE_TESTS_ENABLE_COVERAGE") "1")
                            60 10)
                        internal-time-units-per-second))))
    (let loop ()
      (let ((value (thunk)))
        (cond (value value)
              ((>= (get-internal-real-time) deadline)
               (write-file (string-append root "-failure.log")
                 (string-append (read-file (string-append root "/.stdout"))
                                (read-file (string-append root "/.stderr"))))
               (error "preview test timed out"))
              (else (usleep 50000) (loop)))))))
(define (request path)
  (parameterize ((current-http-proxy #f))
    (catch 'system-error
      (lambda ()
        (call-with-values
          (lambda () (http-get (format #f "http://127.0.0.1:~a~a" port path)
                              #:decode-body? #f))
          list))
      (lambda _ #f))))
(define (page)
  (let ((result (request "/")))
    (and result (= (response-code (car result)) 200) (utf8->string (cadr result)))))
(define (await-text text)
  (wait-for (lambda () (let ((body (page))) (and body (string-contains body text) body)))))
(define (free-port)
  (let ((socket (socket PF_INET SOCK_STREAM 0)))
    (bind socket AF_INET INADDR_LOOPBACK 0)
    (let ((port (sockaddr:port (getsockname socket))))
      (close-port socket)
      port)))
(define (blue-load-path)
  ;;     Coverage wraps the worker's -c code in a module that imports
  ;;     (blue debugging coverage).  The test process finds blue through
  ;;     the launcher's path entries, which the spawned serve does not
  ;;     inherit, so pass the directory explicitly.
  (let ((path (search-path %load-path "blue/subprocess.scm")))
    (unless path (error "blue is not on the load path"))
    (list "-L" (dirname (dirname path)))))

(define (entry-arguments)
  ;;     Search paths shared by the serve process and a synchronous CLI
  ;;     run; both use the per-project cache, so a manual build over a
  ;;     served output stays incremental.
  (append (list "guile" "--no-auto-compile"
                "-L" (dirname sources) "-L" sources
                "-C" (string-append (dirname sources) "/build")
                "-C" (string-append root "/.cache"))
          (blue-load-path)))

(define (entry-environment)
  (cons (string-append "XDG_CACHE_HOME=" root "/.cache")
        (filter (lambda (entry) (not (string-prefix? "XDG_CACHE_HOME=" entry)))
                (environ))))

(define (start-server . options)
  (let ((output (open-output-file (string-append root "/.stdout")))
        (error (open-output-file (string-append root "/.stderr")))
        (input (open-input-file "/dev/null")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let ((pid
                (spawn "guile"
                  (append (entry-arguments)
                          (list (string-append root "/.entry.scm") "serve"
                                "--project" config "--port" (number->string port))
                          options)
                  #:input input #:output output #:error error
                  #:environment (entry-environment))))
          (set! processes (cons pid processes))
          pid))
      (lambda () (close-port input) (close-port output) (close-port error)))))
(define (await-exit pid)
  (let ((status
          (wait-for (lambda () (let ((status (waitpid pid WNOHANG)))
                                (and (not (zero? (car status))) (cdr status)))))))
    (set! processes (delv pid processes))
    status))
(define (run-entry . arguments)
  (let ((stdout (open-output-file (string-append root "/.entry.out")))
        (input (open-input-file "/dev/null")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (waitpid
          (spawn "guile"
            (append (entry-arguments)
                    (list (string-append root "/.entry.scm"))
                    arguments)
            #:input input #:output stdout #:error stdout
            #:environment (entry-environment)))
        (read-file (string-append root "/.entry.out")))
      (lambda () (close-port input) (close-port stdout)))))
(define (stop-server pid signal)
  (kill pid signal)
  (status:exit-val (await-exit pid)))
(define (running? pid)
  (let ((path (format #f "/proc/~a/stat" pid)))
    (and (file-exists? path)
         (not (string-contains (read-file path) ") Z ")))))

(test-begin "serve lifecycle")
(dynamic-wind
  (lambda ()
    (mkdir root)
    (prepare-subprocess root)
    (mkdir external)
    (mkdir inputs)
    (mkdir (string-append inputs "/nested"))
    (symlink inputs (string-append root "/sources"))
    (mkdir (string-append root "/modules"))
    (mkdir (string-append root "/modules/probe"))
    (write-file article "one")
    (write-file control "fast")
    (write-file (string-append root "/asset.txt") "asset-1")
    (write-file (string-append external "/theme.txt") "css-1")
    (write-file helper (helper-text "module-1"))
    (write-file (string-append root "/.entry.scm")
      "(use-modules (tests unit grove subprocess))\n(exit (cli-entry (cdr (command-line))))\n")
    (write-file config
      (format #f
        "(add-to-load-path \"modules\")
(use-modules (grove project) (grove build) (grove output) (probe helper)
             (ice-9 ftw) (ice-9 textual-ports) (srfi srfi-13) (rnrs bytevectors))
(unless (file-exists? \".cache\") (mkdir \".cache\"))
;;     Test state lives under the cache directory, outside the watched
;;     project tree: rewriting a project file on every load would
;;     requeue one rebuild per cycle.
(call-with-output-file \".cache/worker.pid\" (lambda (p) (write (getpid) p)))
(define external ~s)
(define asset ~s)
(make-project
  #:source-directory \"sources\"
  #:extra-sources (list external \"control\")
  #:read-source (lambda (path)
                 (let ((text (call-with-input-file path get-string-all)))
                   (when (string-contains text \"FAIL\") (error \"article failure\"))
                   text))
  #:analyze (lambda (context)
             ;;     Analysis counter: the retained-model contract is
             ;;     observable here - asset edits re-render without
             ;;     analysis, document edits re-analyze.
             (let ((port (open-file \".cache/analyze.count\" \"a\")))
               (display \"x\" port)
               (close-port port))
             context)
  #:renderers
  (list (make-cached-renderer
          (lambda (context)
            (let ((mode (call-with-input-file \"control\" get-string-all)))
              (cond
                ((string=? mode \"slow\")
                 (call-with-output-file \".building\" (lambda (p) (write (getpid) p)))
                 (sleep 2))
                ((string=? mode \"block\")
                 (let ((pid (spawn \"sleep\" (list \"sleep\" \"30\"))))
                   (call-with-output-file \".helper.pid\" (lambda (p) (write pid p)))
                   (waitpid pid)))
                ((string=? mode \"orphan\")
                 (let ((pid (spawn \"sleep\" (list \"sleep\" \"30\"))))
                   (call-with-output-file \".orphan.pid\" (lambda (p) (write pid p)))
                   (error \"renderer failure after spawning\"))))
              (append
                (list (make-artifact \"index.html\"
                        (string-append
                          (string-join (map project-input-value (project-context-inputs context)) \",\")
                          \"|\" label \"|\" (call-with-input-file external get-string-all)
                          \"|\" (call-with-input-file asset get-string-all)))
                      (make-byte-artifact \"blob.bin\" (u8-list->bytevector '(0 255 128))))
                (map (lambda (input)
                       (make-artifact (string-append (basename (project-input-path input)) \".html\")
                                      (project-input-value input)))
                     (project-context-inputs context)))))))
  #:output-directory \"output\")\n"
        (string-append external "/theme.txt")
        (string-append root "/asset.txt"))))
  (lambda ()
    (set! port (free-port))
    (let ((pid (start-server)))
      (test-assert "initial build serves its complete result" (await-text "one|module-1|css-1"))
      (test-assert "idle preview reports watching"
        (wait-for (lambda ()
                    (string-contains (read-file (string-append root "/.stderr")) "Watching"))))
      (test-equal "binary HTTP response preserves bytes" '(0 255 128)
        (bytevector->u8-list (cadr (request "/blob.bin"))))
      (test-equal "cache metadata is not an HTTP resource" 404
        (response-code (car (request "/.grove-build-state"))))
      (test-equal "unmanaged project files are not HTTP resources" 404
        (response-code (car (request "/project.scm"))))
      (write-file article "two")
      (test-assert "article edits rebuild automatically" (await-text "two|module-1"))
      (write-file helper (helper-text "module-2"))
      (test-assert "named Scheme modules reload and invalidate renderer cache" (await-text "two|module-2"))
      (write-file (string-append external "/theme.txt") "css-2")
      (test-assert "declared external file edits rebuild" (await-text "css-2"))
      (write-file (string-append root "/sources/nested/new.txt") "new")
      (test-assert "new nested sources are discovered through a source-directory symlink" (await-text "new,two|"))
      (delete-file (string-append root "/sources/nested/new.txt"))
      (wait-for (lambda () (let ((body (page))) (and body (string-prefix? "two|" body)))))
      (test-equal "deleted source artifacts disappear from HTTP snapshot" 404
        (response-code (car (request "/new.txt.html"))))
      ;;     An asset analysis never read must re-render without
      ;;     re-running analysis; a document edit must re-analyze.  The
      ;;     counter stands at five: the initial build, the article
      ;;     edit, the worker restart, and the added and deleted source.
      (write-file (string-append root "/asset.txt") "asset-2")
      (test-assert "undeclared asset edits rebuild without analysis"
        (await-text "asset-2"))
      (test-equal "asset edits skip analysis" 5
        (string-length (read-file (string-append root "/.cache/analyze.count"))))
      (write-file article "three")
      (test-assert "document edits re-analyze" (await-text "three|"))
      (test-equal "document edits re-run analysis" 6
        (string-length (read-file (string-append root "/.cache/analyze.count"))))
      (write-file article "two")
      (wait-for (lambda () (let ((body (page))) (and body (string-prefix? "two|" body)))))
      (let ((before (page)))
        (write-file article "FAIL")
        (wait-for (lambda () (string-contains (read-file (string-append root "/.stderr"))
                                             "last successful outputs")))
        (test-equal "failed rebuild keeps serving last successful snapshot" before (page)))
      (write-file article "three")
      (test-assert "fixing a failed article restores automatic rebuilding" (await-text "three|"))
      (write-file helper "(broken syntax\n")
      (wait-for (lambda () (string-contains (read-file (string-append root "/.stderr")) "load failed")))
      (test-assert "module-load failures retain the successful article" (string-contains (page) "three|module-2"))
      (write-file helper (helper-text "module-3"))
      (test-assert "fixing a module syntax error recovers in a fresh process" (await-text "module-3"))
      ;;     The serve worker inherited this XDG cache, so the state and
      ;;     manifest its builds wrote are exactly what a manual build
      ;;     reads: no source changed since the module-3 rebuild, so
      ;;     nothing may re-render.
      (test-assert "a manual build over a served output is a cache hit"
        (let ((summary (run-entry "build" "--project" config)))
          (or (string-contains summary "(0 affected)")
              (begin (display summary (current-error-port)) #f))))
      ;;     Churn regression: stamps outside project content - store
      ;;     items and grove's own compiled tree - must not requeue
      ;;     rebuilds. The worker's load paths resolve into trees whose
      ;;     stamps differ between resolutions; each flip used to chain
      ;;     another full rebuild with no project edit behind it.
      (let ((baseline-churn (rebuild-count)))
        (for-each touch-rewrite
                  (map (lambda (name)
                         (string-append (dirname sources) "/build/grove/" name))
                       '("build.go" "cli.go" "collect.go")))
        (usleep 3000000)
        (test-assert "rewriting grove's compiled tree does not rebuild"
          (= baseline-churn (rebuild-count))))
      (write-file (string-append external "/theme.txt") "css-3")
      (test-assert "the watcher still answers real edits" (await-text "css-3"))
      (let ((settled (rebuild-count)))
        (usleep 3000000)
        (test-assert "idle serve does not rebuild on its own" (= settled (rebuild-count))))
      (write-file control "slow")
      (wait-for (lambda () (file-exists? (string-append root "/.building"))))
      (let ((building (call-with-input-file (string-append root "/.building") read)))
        (write-file article "rapid-one")
        (usleep 100000)
        (write-file article "rapid-two")
        (test-assert "HTTP remains available while a rebuild runs" (string-contains (page) "three|"))
        (test-assert "changes during a build do not start overlapping workers" (running? building)))
      (test-assert "changes during a rebuild are picked up by the next build" (await-text "rapid-two|"))
      (write-file control "block")
      (wait-for (lambda () (file-exists? (string-append root "/.helper.pid"))))
      (let ((worker (call-with-input-file
                      (string-append root "/.cache/worker.pid") read))
            (child (call-with-input-file (string-append root "/.helper.pid") read)))
        (test-equal "Ctrl-C exits with status 130" 130 (stop-server pid SIGINT))
        (test-assert "Ctrl-C stops the active build worker" (not (running? worker)))
        (wait-for (lambda () (not (running? child))))
        (test-assert "Ctrl-C also stops project subprocesses" (not (running? child)))))
    (write-file control "fast")
    (let* ((port-socket (socket PF_INET SOCK_STREAM 0))
           (_ (bind port-socket AF_INET INADDR_LOOPBACK 0)))
      (set! port (sockaddr:port (getsockname port-socket)))
      (listen port-socket 1)
      (let ((pid (start-server "--output" (string-append root "/occupied"))))
        (test-equal "an occupied port exits with a build error" 1 (status:exit-val (await-exit pid)))
        (test-assert "port conflicts are reported explicitly"
          (string-contains (read-file (string-append root "/.stderr")) "serve failed"))
        (test-assert "port conflicts do not start a build"
          (not (file-exists? (string-append root "/occupied")))))
      (close-port port-socket))
    (set! port (free-port))
    (let ((pid (start-server "--no-watch" "--output" (string-append root "/fixed"))))
      (await-text "rapid-two|")
      (write-file article "fixed-new")
      (usleep 1200000)
      (test-assert "--no-watch does not report watching"
        (not (string-contains (read-file (string-append root "/.stderr")) "Watching")))
      (test-assert "--no-watch keeps its initial snapshot" (string-contains (page) "rapid-two|"))
      (test-assert "serve honors the output override"
        (file-exists? (string-append root "/fixed/index.html")))
      (test-equal "SIGTERM shuts down with status 143" 143 (stop-server pid SIGTERM)))
    ;;     A serve killed outright cannot run its teardown, and the
    ;;     worker is blocked in a renderer child wait at this point, so
    ;;     only its watchdog can react: worker and spawned subprocess
    ;;     must both disappear.
    (write-file control "block")
    (when (file-exists? (string-append root "/.helper.pid"))
      (delete-file (string-append root "/.helper.pid")))
    (set! port (free-port))
    (let ((pid (start-server)))
      (wait-for (lambda () (file-exists? (string-append root "/.helper.pid"))))
      (let ((worker (call-with-input-file
                      (string-append root "/.cache/worker.pid") read))
            (child (call-with-input-file
                     (string-append root "/.helper.pid") read)))
        (kill pid SIGKILL)
        (await-exit pid)
        (wait-for (lambda () (not (running? worker))))
        (test-assert "a hard-killed serve cannot leave its worker behind"
          (not (running? worker)))
        (wait-for (lambda () (not (running? child))))
        (test-assert "a hard-killed serve cannot leave project subprocesses"
          (not (running? child)))))
    ;;     A worker that dies mid-build still owns its group: the
    ;;     project subprocess it spawned must be reaped with it, and
    ;;     the serve keeps answering from its last snapshot.
    (write-file control "fast")
    (set! port (free-port))
    (let ((pid (start-server)))
      (test-assert "serve recovers between hard kills" (await-text "fixed-new|"))
      (write-file control "orphan")
      (wait-for (lambda () (file-exists? (string-append root "/.orphan.pid"))))
      (let ((child (call-with-input-file
                     (string-append root "/.orphan.pid") read)))
        (wait-for (lambda () (not (running? child))))
        (test-assert "a crashed worker's subprocesses are reaped with it"
          (not (running? child))))
      (wait-for (lambda ()
                  (string-contains (read-file (string-append root "/.stderr"))
                                   "Build failed")))
      (write-file control "fast"))
    (write-file article "FAIL")
    (set! port (free-port))
    (let ((pid (start-server)))
      (test-equal "initial build failure exits instead of serving stale outputs" 1
        (status:exit-val (await-exit pid))))
    (let ((socket (socket PF_INET SOCK_STREAM 0)))
      (bind socket AF_INET INADDR_LOOPBACK port)
      (test-assert "initial failure releases its listening socket" #t)
      (close-port socket)))
  (lambda ()
    (for-each
      (lambda (pid)
        (catch 'system-error
          (lambda () (kill pid SIGTERM) (await-exit pid))
          (lambda _ #f)))
      processes)
    (for-each
      (lambda (name)
        (let ((path (string-append root "/" name)))
          (when (file-exists? path)
            (let ((pid (call-with-input-file path read)))
              (when (running? pid)
                (catch 'system-error (lambda () (kill (- pid) SIGKILL)) (lambda _ #f)))))))
      '(".cache/worker.pid" ".helper.pid" ".orphan.pid"))
    (system* "rm" "-rf" root external inputs)))
(test-subprocess-coverage '("grove/private/preview/worker.scm"))
(test-end "serve lifecycle")
