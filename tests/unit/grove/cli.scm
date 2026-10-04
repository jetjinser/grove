;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove cli)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (grove build state)
  #:use-module (tests unit grove subprocess)
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
            (string-append "XDG_CACHE_HOME=" root "/cache") "guile" "--no-auto-compile"
            "-L" (dirname sources) "-L" sources
            "-C" (string-append (dirname sources) "/build")
            "-C" root
            script arguments)))
    (list (status:exit-val status)
          (call-with-input-file out get-string-all)
          (call-with-input-file err get-string-all))))

(test-begin "grove CLI")
(dynamic-wind
  (lambda ()
    (mkdir root)
    (prepare-subprocess root)
    (mkdir (string-append root "/sources"))
    (write-file (string-append root "/sources/input.txt") "input")
    (write-file script "(use-modules (tests unit grove subprocess))\n(exit (cli-entry (cdr (command-line))))\n")
    (write-file config
      "(use-modules (grove project) (grove build) (grove output) (ice-9 textual-ports))
(make-project
  #:source-directory \"sources\"
  #:read-source (lambda (path) (call-with-input-file path get-string-all))
  #:renderers (list (make-cached-renderer
                     (lambda (_) (list (make-artifact \"page.txt\" \"page\")))))
  #:output-directory \"output\")\n"))
  (lambda ()
    (define original-config (call-with-input-file config get-string-all))
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
        ("build" "build") ("check" "--output" "preview")
        ("build" "--no-watch") ("check" "--port" "8080")
        ("serve" "--port" "0") ("serve" "--port" "65536")
        ("serve" "--port" "abc") ("serve" "--port" "1.5")
        ("serve" "--port")))
    (test-equal "check help describes validation without build-only options" #t
      (let ((result (run "check" "--help")))
        (and (= (car result) 0)
             (string-contains (cadr result) "without writing")
             (not (string-contains (cadr result) "--output"))
             #t)))
    (let ((result (run "check" "--project" config)))
      (test-equal "check succeeds before the first build" 0 (car result))
      (test-assert "check summary reports validated artifacts"
        (string-contains (cadr result) "1 artifact checked in "))
      (test-assert "check progress identifies the input on stderr"
        (string-contains (caddr result) "Reading sources/input.txt"))
      (test-assert "check progress never reports writing"
        (not (string-contains (caddr result) "Writing")))
      (test-assert "check creates no output directory"
        (not (file-exists? (string-append root "/output")))))
    (write-file (string-append root "/sources/second.txt") "second")
    (let ((result (run "check" "--project" config)))
      (test-assert "progress preserves a status line for each input"
        (string-contains (caddr result) "Reading sources/second.txt")))
    (delete-file (string-append root "/sources/second.txt"))
    (let ((result (run "build" "--project" config)))
      (test-equal "build succeeds outside the project directory" 0 (car result))
      (test-assert "build summary reports artifacts" (string-contains (cadr result) "1 artifact (1 affected) in "))
      (test-assert "finished summary includes elapsed time"
        (and (string-contains (cadr result) "Finished")
             (string-contains (cadr result) " in ")))
      (test-assert "build progress identifies the input on stderr"
        (string-contains (caddr result) "Reading sources/input.txt"))
      (test-assert "redirected progress contains no terminal control sequences"
        (and (not (string-index (caddr result) #\return))
             (not (string-index (caddr result) #\esc))))
      (test-assert "progress leaves stdout for the summary"
        (not (string-contains (cadr result) "Reading sources/input.txt")))
      (test-assert "build uses project-relative output directory"
        (file-exists? (string-append root "/output/page.txt"))))
    (let ((result (run "build" "--project" config)))
      (test-assert "subsequent builds report renderer cache reuse"
        (string-contains (cadr result) "cache hit"))
      (test-assert "cache hits do not claim to render"
        (not (string-contains (caddr result) "Rendering"))))
    (let ((target (string-append root "/override")))
      (test-equal "equals-style options support output overrides" 0
        (car (run "build" (string-append "--project=" config)
                  (string-append "--output=" target))))
      (test-assert "output override is actually used" (file-exists? (string-append target "/page.txt"))))
    (let ((result (run "build" "--project" (string-append root "/missing.scm"))))
      (test-equal "missing project exits with status 1" 1 (car result))
      (test-assert "missing project identifies loading stage"
        (string-contains (caddr result) "load failed")))
    (let* ((paths (map (lambda (name) (string-append root "/output/" name))
                       '("page.txt" ".grove-artifacts" ".grove-build-state")))
           (snapshot (lambda ()
                       (map (lambda (path)
                              (list (file-fingerprint path)
                                    (stat:ino (stat path))
                                    (stat:mtime (stat path)))) paths)))
           (before (snapshot)))
      (test-equal "check succeeds with existing cached outputs" 0
        (car (run "check" "--project" config)))
      (test-equal "CLI check leaves output contents and state untouched" before (snapshot))
      (for-each
        (lambda (failure)
          (let ((stage (car failure)) (from (cadr failure)) (to (caddr failure)))
            (write-file config (string-replace-substring original-config from to))
            (let ((result (run "check" "--project" config)))
              (test-equal (string-append stage " failures exit with status 1") 1 (car result))
              (test-assert (string-append stage " failures retain phase context")
                (string-contains (caddr result) (string-append stage " failed")))
              (test-equal "failed checks write no success summary" "" (cadr result))
              (test-equal "failed checks preserve outputs and cache" before (snapshot)))))
        '(("collect" "#:source-directory \"sources\""
                     "#:collect-sources (lambda () (error \"collector failure\"))")
          ("prepare" "#:source-directory" "#:prepare (lambda () (error \"prepare failure\")) #:source-directory")
          ("read" "(call-with-input-file path get-string-all)" "(error \"reader failure\")")
          ("analyze" "#:renderers" "#:analyze (lambda (_) (error \"analysis failure\")) #:renderers")
          ("render" "(list (make-artifact \"page.txt\" \"page\"))" "(error \"renderer failure\")")
          ("validate" "(list (make-artifact \"page.txt\" \"page\"))"
                      "(list (make-artifact \"a\" \"file\") (make-artifact \"a/b\" \"nested\"))")))
      (write-file config original-config)
      (write-file (string-append root "/output/.grove-build-state") "malformed state")
      (let ((before (snapshot)))
        (test-equal "CLI check ignores malformed published cache state" 0
          (car (run "check" "--project" config)))
        (test-equal "CLI check does not repair or overwrite that state" before (snapshot))))
    (write-file config "(throw 'misc-error #f \"~A\" (list (make-string 2000 #\\x)) #f)\n")
    (let ((short (run "build" "--project" config))
          (debug (run "build" "--project" config "--debug")))
      (test-equal "project evaluation errors exit with status 1" 1 (car short))
      (test-assert "default diagnostics remain bounded" (< (string-length (caddr short)) 1000))
      (test-assert "debug includes the full diagnostic" (> (string-length (caddr debug)) 2000))))
  (lambda () (system* "rm" "-rf" root)))
(test-subprocess-coverage '("grove/cli.scm" "grove/private/cli/progress.scm"))
(test-end "grove CLI")
