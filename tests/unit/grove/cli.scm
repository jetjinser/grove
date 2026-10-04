;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove cli)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define root (format #f "/tmp/grove-cli-test-~a" (getpid)))
(define script (string-append root "/entry.scm"))
(define config (string-append root "/project.scm"))
(define out (string-append root "/stdout"))
(define err (string-append root "/stderr"))
(define sources (getenv "GROVE_SRCDIR"))
(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))
(define (run . arguments)
  (let ((status
          (apply system* "sh" "-c"
            "out=$1; err=$2; shift 2; exec \"$@\" >\"$out\" 2>\"$err\""
            "grove-test" out err "env"
            (string-append "XDG_CACHE_HOME=" root "/cache") "guile" "--no-auto-compile" "-L" sources
            script arguments)))
    (list (status:exit-val status)
          (call-with-input-file out get-string-all)
          (call-with-input-file err get-string-all))))

(test-begin "grove CLI")
(dynamic-wind
  (lambda ()
    (mkdir root)
    (mkdir (string-append root "/sources"))
    (write-file (string-append root "/sources/input.txt") "input")
    (write-file script "(use-modules (grove cli))\n(exit (main (cdr (command-line))))\n")
    (write-file config
      "(use-modules (grove project) (grove build) (grove output) (ice-9 textual-ports))
(make-project
  #:source-directory \"sources\"
  #:read-source (lambda (path) (call-with-input-file path get-string-all))
  #:renderers (list (make-cached-renderer
                     (lambda (_) (list (make-artifact \"page.txt\" \"page\")))
                     #:cache-key \"renderer\"))
  #:cache-key \"project\"
  #:output-directory \"output\")\n"))
  (lambda ()
    (let ((result (run "--help")))
      (test-equal "help succeeds without loading a project" 0 (car result))
      (test-assert "help describes the command" (string-contains (cadr result) "build"))
      (test-equal "help writes no diagnostics" "" (caddr result)))
    (for-each
      (lambda (arguments)
        (let ((result (apply run arguments)))
          (test-equal (format #f "invalid arguments exit with status 2: ~s" arguments)
            2 (car result))
          (test-equal "usage errors do not write success output" "" (cadr result))
          (test-assert "usage errors provide diagnostics" (not (string-null? (caddr result))))))
      '(() ("unknown") ("build" "--project") ("build" "--output=")
        ("build" "--unknown") ("build" "--project" "a" "--project" "b")
        ("build" "build")))
    (let ((result (run "build" "--project" config)))
      (test-equal "build succeeds outside the project directory" 0 (car result))
      (test-assert "build summary reports artifacts" (string-contains (cadr result) "Built 1 artifact"))
      (test-equal "successful build has no diagnostics" "" (caddr result))
      (test-assert "build uses project-relative output directory"
        (file-exists? (string-append root "/output/page.txt"))))
    (test-assert "subsequent builds report renderer cache reuse"
      (string-contains (cadr (run "build" "--project" config)) "cache hit"))
    (let ((target (string-append root "/override")))
      (test-equal "equals-style options support output overrides" 0
        (car (run "build" (string-append "--project=" config)
                  (string-append "--output=" target))))
      (test-assert "output override is actually used" (file-exists? (string-append target "/page.txt"))))
    (let ((result (run "build" "--project" (string-append root "/missing.scm"))))
      (test-equal "missing project exits with status 1" 1 (car result))
      (test-assert "missing project identifies loading stage"
        (string-contains (caddr result) "load failed")))
    (write-file config "(throw 'misc-error #f \"~A\" (list (make-string 2000 #\\x)) #f)\n")
    (let ((short (run "build" "--project" config))
          (debug (run "build" "--project" config "--debug")))
      (test-equal "project evaluation errors exit with status 1" 1 (car short))
      (test-assert "default diagnostics remain bounded" (< (string-length (caddr short)) 1000))
      (test-assert "debug includes the full diagnostic" (> (string-length (caddr debug)) 2000))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "grove CLI")
