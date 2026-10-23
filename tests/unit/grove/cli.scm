;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove cli)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (grove build state)
  #:use-module (grove private cache-dir)
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
;;     A build tree that lags the sources makes Guile log
;;     source-vs-compiled notes on the subprocess's stderr; those are
;;     loader chatter, not program diagnostics, so the stderr
;;     assertions below ignore them.
(define (without-loader-notes text)
  (string-join
    (filter (lambda (line) (not (string-prefix? ";;;" line)))
            (string-split text #\newline))
    "\n"))
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
    ;;     The same cache root the subprocesses receive below, so
    ;;     snapshots computed here read the files they write.
    (setenv "XDG_CACHE_HOME" (string-append root "/cache"))
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
      (test-equal "help writes no diagnostics" ""
        (without-loader-notes (caddr result))))
    (let ((result (run "--version")))
      (test-equal "version succeeds without loading a project" 0 (car result))
      (test-assert "version identifies the program" (string-contains (cadr result) "grove"))
      (test-equal "version writes no diagnostics" ""
        (without-loader-notes (caddr result))))
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
        ("serve" "--port") ("new" "--output" "x")
        ("build" "--dest" "x") ("new" "a" "b")
        ("browse" "--ssh-generate-host-key")
        ("browse" "--ssh-workers" "0")
        ("browse" "--ssh-memory-limit" "0")
        ("build" "--ssh-workers" "2")))
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
    (let ((result (run "build" "--profile" "--project" config)))
      (test-equal "profiled build succeeds" 0 (car result))
      (test-assert "profile reports the cache cost split"
        (string-contains (caddr result) "input cache:"))
      (test-assert "profile reports peak RSS"
        (string-contains (caddr result) "peak RSS")))
    ;;     Without --project, commands search upward from the
    ;;     invocation directory and stop at a repository boundary.
    (let ((invocation (getcwd)))
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          (mkdir (string-append root "/nested"))
          (mkdir (string-append root "/nested/deeper"))
          (chdir (string-append root "/nested/deeper"))
          (let ((result (run "check")))
            (test-equal "commands discover the project file upward" 0 (car result))
            (test-assert "discovered check reads the discovered project's inputs"
              (string-contains (caddr result) "Reading sources/input.txt")))
          (mkdir (string-append root "/boundary"))
          (mkdir (string-append root "/boundary/.git"))
          (mkdir (string-append root "/boundary/work"))
          (chdir (string-append root "/boundary/work"))
          (let ((result (run "check")))
            (test-equal "a repository boundary stops the upward search" 1 (car result))
            (test-assert "the boundary failure names the invocation directory"
              (string-contains (caddr result)
                               (string-append "no project file at "
                                              root "/boundary/work/project.scm"))))
          (test-equal "an explicit --project overrides the search" 0
            (car (run "check" "--project" config))))
        (lambda () (chdir invocation))))
    (let ((target (string-append root "/override")))
      (test-equal "equals-style options support output overrides" 0
        (car (run "build" (string-append "--project=" config)
                  (string-append "--output=" target))))
      (test-assert "output override is actually used" (file-exists? (string-append target "/page.txt"))))
    (let ((result (run "build" "--project" (string-append root "/missing.scm"))))
      (test-equal "missing project exits with status 1" 1 (car result))
      (test-assert "missing project identifies loading stage"
        (string-contains (caddr result) "load failed")))
    (let* ((cache-directory (grove-cache-directory
                              (string-append root "/output")))
           (paths (list (string-append root "/output/page.txt")
                        (string-append cache-directory "/.grove-artifacts")
                        (string-append cache-directory "/.grove-build-state")))
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
      (write-file (string-append cache-directory "/.grove-build-state")
                  "malformed state")
      (let ((before (snapshot)))
        (test-equal "CLI check ignores malformed published cache state" 0
          (car (run "check" "--project" config)))
        (test-equal "CLI check does not repair or overwrite that state" before (snapshot))))
    ;; Creation needs only filenames, even when all execution hooks fail.
    (write-file config
      "(use-modules (grove project site))\n(make-site-project #:options (make-site-options #:source-directory \"sources\" #:default-prefix \"jsk\" #:prepare (lambda () (error \"must not prepare\")) #:read-page (lambda (_) (error \"must not read\")) #:analyze (lambda (_) (error \"must not analyze\")) #:output-directory \"output\"))\n")
    (mkdir (string-append root "/sources/nested"))
    (write-file (string-append root "/sources/nested/jsk-000z.plant") "invalid source")
    (write-file (string-append root "/sources/jsk-invalid!.plant") "ignored suffix")
    (write-file (string-append root "/sources/other-zzzz.plant") "other namespace")
    (let ((result (run "new" "--project" config)))
      (test-equal "new uses the configured prefix and scans nested sources" 0 (car result))
      (test-equal "new prints only the absolute created path"
        (string-append root "/sources/jsk-0010.plant\n") (cadr result))
      (test-assert "new includes a title and publication date"
        (let ((body (call-with-input-file (string-append root "/sources/jsk-0010.plant") get-string-all)))
          (and (string-prefix? "@use-modules[(forester commands)]\n\n@title{}\n@published{" body)
               (string-suffix? "}\n" body)))))
    (test-equal "new advances past the previously created file"
      (string-append root "/sources/jsk-0011.plant\n")
      (cadr (run "new" "--project" config)))
    (write-file (string-append root "/sources/math-zzzz.plant") "overflow")
    (test-equal "explicit prefix overrides the default and grows beyond four digits"
      (string-append root "/sources/math-10000.plant\n")
      (cadr (run "new" "math" "--project" config)))
    (mkdir (string-append root "/new-destination"))
    (let ((caller (getcwd)))
      (dynamic-wind
        (lambda () (chdir root))
        (lambda ()
          (test-equal "dest is relative to the invocation directory"
            (string-append root "/new-destination/jsk-0012.plant\n")
            (cadr (run "new" "--project" config "--dest" "new-destination"))))
        (lambda () (chdir caller))))
    (for-each
      (lambda (prefix)
        (test-equal "unsafe or noncanonical prefixes fail" 1
          (car (run "new" prefix "--project" config))))
      '("../escape" "UPPER" "bad-"))
    (test-equal "missing destination fails without creating it" 1
      (car (run "new" "--project" config "--dest" (string-append root "/absent"))))
    ;; Exercise generated source through the real document/page reader.
    (mkdir (string-append root "/template-sources"))
    (write-file config
      "(use-modules (grove project site) (grove input scribble) (forester input scribble))\n(make-site-project #:options (make-site-options #:source-directory \"template-sources\" #:default-prefix \"tutorial\" #:tree-template (lambda (id date) (string-append \"@use-modules[(forester commands) (language scribble commands)]\\n@title{教程：\" id \"}\\n@published{\" date \"}\\nHello.\\n\")) #:read-page (make-page-reader (make-scribble-document-reader)) #:output-directory \"output\"))\n")
    (test-equal "site projects preserve custom templates through CLI loading" 0
      (car (run "new" "--project" config)))
    (test-assert "custom templates receive the allocated ID and preserve Unicode"
      (string-contains
        (call-with-input-file (string-append root "/template-sources/tutorial-0001.plant") get-string-all)
        "@title{教程：tutorial-0001}"))
    (test-equal "custom generated article passes real reading and analysis" 0
      (car (run "check" "--project" config)))
    (write-file config
      "(use-modules (grove project site) (grove input scribble) (forester input scribble))\n(make-site-project #:options (make-site-options #:source-directory \"template-sources\" #:default-prefix \"plain\" #:read-page (make-page-reader (make-scribble-document-reader)) #:output-directory \"output\"))\n")
    (test-equal "default template creates an article with explicit imports" 0
      (car (run "new" "--project" config)))
    (let ((path (string-append root "/template-sources/plain-0001.plant")))
      (write-file path
        (string-replace-substring (call-with-input-file path get-string-all)
                                  "@title{}" "@title{Plain}")))
    (test-equal "default generated article builds after filling its title" 0
      (car (run "check" "--project" config)))
    (let ((valid-config (call-with-input-file config get-string-all)))
      (for-each
        (lambda (template)
          (write-file config
            (string-replace-substring valid-config "#:default-prefix"
              (string-append "#:tree-template " template " #:default-prefix")))
          (test-equal "invalid templates fail before creating a file" 1
            (car (run "new" "--project" config)))
          (test-assert "failed template leaves no empty article"
            (not (file-exists? (string-append root "/template-sources/plain-0002.plant")))))
        '("(lambda (id date) 42)" "(lambda (id date) (error \"template failure\"))" "42")))
    (write-file config original-config)
    (test-equal "missing default prefix requires an explicit prefix" 1
      (car (run "new" "--project" config)))
    (test-equal "explicit prefix works without a default" 0
      (car (run "new" "note" "--project" config)))
    (test-equal "command names can also be prefixes" 0
      (car (run "new" "build" "--project" config)))
    (mkdir (string-append root "/sources/note-0002.plant"))
    (test-equal "exclusive creation skips occupied paths without overwriting"
      (string-append root "/sources/note-0003.plant\n")
      (cadr (run "new" "note" "--project" config)))
    (test-assert "occupied directory remains intact"
      (file-is-directory? (string-append root "/sources/note-0002.plant")))
    (write-file config
      "(use-modules (grove project))\n(make-project #:collect-sources (lambda () '()) #:default-prefix \"jsk\" #:read-source (lambda (_) (error \"must not read\")) #:output-directory \"output\")\n")
    (test-equal "collector-only projects require an explicit destination" 1
      (car (run "new" "--project" config)))
    (test-equal "collector-only projects scan existing destination IDs"
      (string-append root "/new-destination/jsk-0013.plant\n")
      (cadr (run "new" "--project" config "--dest" (string-append root "/new-destination"))))
    (write-file config "(throw 'misc-error #f \"~A\" (list (make-string 2000 #\\x)) #f)\n")
    (let ((short (run "build" "--project" config))
          (debug (run "build" "--project" config "--debug")))
      (test-equal "project evaluation errors exit with status 1" 1 (car short))
      (test-assert "default diagnostics remain bounded"
        (< (string-length (without-loader-notes (caddr short))) 1000))
      (test-assert "debug includes the full diagnostic" (> (string-length (caddr debug)) 2000))))
  (lambda () (system* "rm" "-rf" root)))
(test-subprocess-coverage '("grove/cli.scm" "grove/project/new.scm" "grove/private/cli/progress.scm"))
(test-end "grove CLI")
