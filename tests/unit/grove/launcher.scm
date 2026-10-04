;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove launcher)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define sources (getenv "GROVE_SRCDIR"))
(define repository (dirname sources))
(set! %load-path (cons repository %load-path))
(resolve-interface '(blueprint buildables))
(define runtime-library-paths
  (module-ref (resolve-module '(blueprint buildables)) 'runtime-library-paths))
(define root (format #f "/tmp/grove-launcher-test-~a" (getpid)))
(define launcher (string-append root "/grove"))
(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))
(define (read-file path) (call-with-input-file path get-string-all))
(define (run command)
  (let* ((stdout (string-append root "/stdout"))
         (stderr (string-append root "/stderr"))
         (status
           (system* "sh" "-c"
             "out=$1; err=$2; shift 2; exec \"$@\" >\"$out\" 2>\"$err\""
             "launcher-test" stdout stderr "env" "-i"
             (string-append "HOME=" root)
             (string-append "XDG_CACHE_HOME=" root "/cache")
             launcher command "--project" (string-append root "/project.scm"))))
    (list (status:exit-val status) (read-file stdout) (read-file stderr))))

(test-begin "standalone launcher")
(dynamic-wind
  (lambda ()
    (mkdir root)
    (mkdir (string-append root "/sources"))
    (write-file (string-append root "/sources/input.txt") "input")
    (let ((template (read-file (string-append repository "/bin/grove.in"))))
      ;; Use the production template and dependency resolver. The library source
      ;; directory stands in for an installed prefix; child processes have no
      ;; inherited load paths, development tools, or user compilation cache.
      (for-each
        (lambda (entry)
          (set! template
            (string-replace-substring template (car entry) (cdr entry))))
        (list
          (cons "@GUILE@" (string-append (assq-ref %guile-build-info 'bindir) "/guile"))
          (cons "@GUILE_MODULE_DIR@" sources)
          (cons "@GUILE_OBJECT_DIR@" (string-append root "/no-compiled-modules"))
          (cons "@GUILE_RUNTIME_LOAD_PATH@"
                (format #f "~s" (runtime-library-paths %load-path ".scm" #t)))
          (cons "@GUILE_RUNTIME_COMPILED_PATH@"
                (format #f "~s" (runtime-library-paths %load-compiled-path ".go" #f)))))
      (write-file launcher template)
      (chmod launcher #o755))
    (write-file (string-append root "/project.scm")
      "(use-modules (grove project) (grove output) (grove relation) (ice-9 textual-ports))
(make-project
  #:source-directory \"sources\"
  #:read-source (lambda (path) (call-with-input-file path get-string-all))
  #:analyze (lambda (_) (run* (q) (== q 'ready)))
  #:renderers (list (lambda (answers)
                     (list (make-artifact \"result.scm\" (format #f \"~s\" answers)))))
  #:output-directory \"output\")\n"))
  (lambda ()
    (test-equal "launcher starts without devshell variables" 0 (car (run "--help")))
    (let ((result (run "check")))
      (test-equal "standalone check resolves hashing and relational dependencies" 0 (car result))
      (test-assert "check completes rendering" (string-contains (cadr result) "1 artifact checked"))
      (test-assert "check leaves output absent" (not (file-exists? (string-append root "/output")))))
    (test-equal "standalone build succeeds" 0 (car (run "build")))
    (test-equal "standalone build executes miniKanren" "(ready)"
      (read-file (string-append root "/output/result.scm"))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "standalone launcher")
