;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove input-cache)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove private build-state)
  #:use-module (grove private html-style)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove style html element)
  #:use-module (grove input scribble)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove private module-load)
  #:use-module (grove project)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove private cache-dir)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (system base compile)
  #:use-module (srfi srfi-64))

(define root (format #f "/tmp/grove-input-cache-~a" (getpid)))
(define output (string-append root "/output"))
(define source (string-append root "/page.plant"))
(define dependency (string-append root "/dependency"))
(define evaluations 0)
(define analyses 0)
(define partial-renders 0)
(define statuses '())
(define renderer-events '())
(define (put path value)
  (call-with-output-file path (lambda (port) (display value port))))
(define reader
  (make-scribble-document-reader
    #:cache-dependencies (lambda () (list dependency))
    #:timings (lambda (path phase seconds)
                (when (eq? phase 'evaluate)
                  (set! evaluations (+ evaluations 1))))))
(define (project cache?)
  (make-project
    #:collect-sources (lambda () (list source))
    #:read-source reader
    #:analyze (lambda (context)
                (set! analyses (+ analyses 1))
                (project-input-value (car (project-context-inputs context))))
    #:renderers
    (let ((render (lambda (document)
                    (list (make-artifact "page.html"
                            (sxml->html (content->sxml document))
                            #:sources (list source))))))
      (list (make-cached-renderer render #:configuration 'html
              #:render-changed
              (lambda (document changed dirty old)
                (set! partial-renders (+ partial-renders 1))
                (render document)))))
    #:cache? cache? #:output-directory output))
(define rich-document
  (make-content-document
    (list (make-content-element 'paragraph
            (list (make-content-text "λ" #:source source
                    #:location `((file . ,source) (line . 3) (column . 2)))
                  (make-content-layout 'newline "\n" #:source source))
            #:source source #:flow-kind 'block
            #:location `((file . ,source) (line . 4) (column . 1))
            #:style (make-content-style 'sample
                     (list (make-html-attributes '((class . "extra")))
                           (make-html-tag "aside")))
            #:properties '((open? . #f) (arbitrary . (text "literal")))))
    #:source source))

(test-begin "document result cache")
(test-error "module observers must accept a loaded file path" #t
  (call-with-module-load-observer (lambda () #t) (lambda () #t)))
(test-error "module observation requires a zero-argument thunk" #t
  (call-with-module-load-observer (lambda (_) #t) (lambda (_) #t)))
(let ((observed '()) (nested '()) (initial-hook %load-hook))
  (dynamic-wind
    (lambda () (set! %load-hook #f))
    (lambda ()
      (call-with-module-load-observer
        (lambda (file) (set! observed (cons file observed)))
        (lambda ()
          (%load-hook "outer-before")
          (call-with-module-load-observer
            (lambda (file) (set! nested (cons file nested)))
            (lambda () (%load-hook "nested")))
          (%load-hook "outer-after")))
      (test-equal "nested module observers preserve the outer observer"
        '("outer-before" "nested" "outer-after") (reverse observed))
      (test-equal "inner module observers see only their own scope"
        '("nested") (reverse nested))
      (test-equal "module load hook is restored after observation" #f %load-hook)
      (test-error "module load hook is restored after an observer error" #t
        (call-with-module-load-observer
          (lambda (_) #t)
          (lambda () (error "observer scope probe"))))
      (test-equal "module load hook is restored after a raised exception"
        #f %load-hook))
    (lambda () (set! %load-hook initial-hook))))

(test-equal "disabled input caching still has a dependency context"
  '() (input-cache-dependencies (make-input-cache-context #f '())))
(test-error "input cache option must be boolean" #t
  (make-input-cache-context 'enabled '()))
(test-error "input cache dependencies must be non-empty paths" #t
  (make-input-cache-context #t (list "")))
(test-error "input dependency path must be a non-empty string" #t
  (depend-on! ""))
(test-error "input dependency path must be a non-empty string" #t
  (depend-on! #f))
(test-error "Scribble reader cache option is checked at construction" #t
  (make-scribble-document-reader #:cache? 'enabled))
(test-error "Scribble cache predicates must accept a source path" #t
  (make-scribble-document-reader #:cache? (lambda () #t)))
(test-error "Scribble dependency collector is checked at construction" #t
  (make-scribble-document-reader #:cache-dependencies 'invalid))
(test-error "Scribble dependency collectors must accept no arguments" #t
  (make-scribble-document-reader #:cache-dependencies (lambda (_) '())))
(test-error "Scribble timing callback is checked at construction" #t
  (make-scribble-document-reader #:timings 'invalid))
(test-error "Scribble timing callbacks must accept three arguments" #t
  (make-scribble-document-reader #:timings (lambda (_) #t)))
(test-error "Scribble environment factory is checked at construction" #t
  (make-scribble-document-reader #:environment-factory 'invalid))
(test-error "Scribble environment factories must accept no arguments" #t
  (make-scribble-document-reader #:environment-factory (lambda (_) #f)))
(test-error "Scribble environment is checked at construction" #t
  (make-scribble-document-reader #:environment 'invalid))
(test-assert "Scribble reader accepts an explicit shared environment"
  (procedure? (make-scribble-document-reader
                #:environment (resolve-module '(tests unit grove input-cache)))))
(dynamic-wind
  (lambda ()
    (setenv "XDG_CACHE_HOME" (string-append root "/.cache"))
    (mkdir root)
    (put source "First") (put dependency "one"))
  (lambda ()
    (let ((invalid-reader
            (make-scribble-document-reader
              #:cache-dependencies (lambda () (list "")))))
      (test-error "Scribble dependency collector validates returned paths"
        #t
        (parameterize ((current-input-cache
                         (make-input-cache-context #t '())))
          (invalid-reader source))))
    (parameterize ((current-input-fingerprints (make-hash-table)))
      (let* ((before (input-fingerprint dependency))
             (info (stat dependency)))
        (put dependency "two")
        (utime dependency (stat:atime info) (stat:mtime info))
        (test-assert "same-size edits with restored mtime invalidate the build memo"
          (not (equal? before (input-fingerprint dependency))))
        (delete-file dependency)
        (test-equal "deletions invalidate the build memo" #f (input-fingerprint dependency))
        (put dependency "one")
        (test-equal "restoring an absent input invalidates the build memo"
          before (input-fingerprint dependency))))
    (let ((alias (string-append root "/dependency-alias")))
      (symlink dependency alias)
      (parameterize ((current-input-fingerprints (make-hash-table)))
        (let ((before (input-fingerprint alias))
              (info (stat dependency)))
          (put dependency "two")
          (utime dependency (stat:atime info) (stat:mtime info))
          (test-assert "input memo notices same-size edits through file symlinks"
            (not (equal? before (input-fingerprint alias))))))
      (delete-file alias)
      (put dependency "one"))
    (build-project (project #t) #:progress
      (lambda (stage detail)
        (when (eq? stage 'input-cache) (set! statuses (cons detail statuses)))
        (when (eq? stage 'render-renderer)
          (set! renderer-events (cons detail renderer-events)))))
    (test-assert "renderer progress identifies its index, duration, and artifact count"
      (and (= (length renderer-events) 1)
           (= (caar renderer-events) 0)
           (number? (cadar renderer-events))
           (= (caddar renderer-events) 1)))
    (set! renderer-events '())
    (build-project (project #t) #:progress
      (lambda (stage detail)
        (when (eq? stage 'input-cache) (set! statuses (cons detail statuses)))
        (when (eq? stage 'render-renderer)
          (set! renderer-events (cons detail renderer-events)))))
    (test-equal "complete build-cache hits skip renderer progress" '()
      renderer-events)
    (test-equal "unchanged source evaluates once" 1 evaluations)
    (test-equal "whole model is analyzed even on an input hit" 2 analyses)
    (test-equal "progress reports miss then hit" '((1 . 1) (0 . 1)) statuses)
    (let* ((renders 0)
           (external-project
            (make-project
              #:collect-sources (lambda () (list source))
              #:read-source (lambda (_) 'model)
              #:renderers
              (list
                (make-cached-renderer
                  (lambda (_)
                    (set! renders (+ renders 1))
                    (list
                      (make-artifact "external.txt"
                        (call-with-input-file dependency get-string-all))))
                  #:configuration 'external
                  #:dependencies (list dependency)))
              #:output-directory (string-append root "/renderer-inputs"))))
      (build-project external-project)
      (build-project external-project)
      (test-equal "fixed renderer dependencies participate in cache identity" 1 renders)
      (put dependency "three")
      (build-project external-project)
      (test-equal "changing a fixed renderer dependency reruns its renderer" 2 renders)
      (put dependency "one"))
    (test-error "renderer dependency collectors reject duplicate paths" #t
      (build-project
        (make-project
          #:collect-sources (lambda () (list source))
          #:read-source (lambda (_) 'model)
          #:renderers
          (list (make-cached-renderer (lambda (_) '())
                  #:dependencies (lambda () (list dependency dependency))))
          #:output-directory (string-append root "/duplicate-renderer-inputs"))))
    (put source "Second")
    (build-project (project #t))
    (test-equal "source change invalidates its result" 2 evaluations)
    (test-equal "ordinary source changes retain incremental rendering" 1 partial-renders)
    (put dependency "two")
    (test-assert "dependency changes also invalidate renderer cache"
      (not (build-report-render-cache-hit?
             (build-project/report (project #t)))))
    (test-equal "declared dependency change invalidates result" 3 evaluations)
    (test-equal "reader dependencies conservatively force a full render" 1 partial-renders)
    (build-project (project #f))
    (test-equal "disabling project cache forces evaluation" 4 evaluations)
    (let* ((directory (grove-store-directory))
           (cache-directory (string-append directory "/input-cache"))
           (entry (string-append cache-directory "/"
                    (car (scandir cache-directory
                                  (lambda (name)
                                    (not (member name '("." "..")))))))))
      (define (snapshot)
        (let ((info (stat entry)))
          (list (file-fingerprint entry) (stat:ino info) (stat:mtime info))))
      (let ((before (snapshot)))
        (check-project (project #t))
        (test-equal "check preserves the cache file without rewriting it"
          before (snapshot))))
    (test-equal "check always evaluates the source" 5 evaluations)
    (build-project (project #t))
    (test-equal "check does not replace the stored entry" 5 evaluations)
    (let* ((directory (grove-store-directory))
           (cache-directory (string-append directory "/input-cache"))
           (entry (string-append cache-directory "/"
                    (car (scandir cache-directory
                                  (lambda (name)
                                    (not (member name '("." "..")))))))))
      (put entry "malformed")
      (build-project (project #t))
      (test-equal "corrupt cache falls back to source evaluation" 6 evaluations))
    (reader source)
    (test-equal "standalone reader evaluates normally" 7 evaluations)
    (parameterize ((current-input-cache
                    (make-input-cache-context #t (list dependency))))
      (cached-document source "codec" '() (lambda () rich-document) (lambda _ #t))
      (let ((decoded (cached-document source "codec" '()
                       (lambda () (error "expected a cache hit")) (lambda _ #t))))
        (let* ((element (car (content-document-children decoded)))
               (text (car (content-element-children element)))
               (layout (cadr (content-element-children element)))
               (style (content-element-style element)))
          (test-equal "cache preserves text and source provenance"
            (list source "λ" source `((file . ,source) (line . 3) (column . 2)))
            (list (content-document-source decoded) (content-text-value text)
                  (content-text-source text) (content-source-location text)))
          (test-equal "cache preserves element identity and arbitrary data properties"
            (list 'paragraph 'block source
                  `((file . ,source) (line . 4) (column . 1))
                  '((open? . #f) (arbitrary . (text "literal"))))
            (list (content-element-kind element) (content-element-flow-kind element)
                  (content-element-source element) (content-source-location element)
                  (content-element-properties element)))
          (test-equal "cache preserves presentation annotations"
            '(sample ((class . "extra")) "aside")
            (list (content-style-name style)
                  (html-attributes-assoc (car (content-style-properties style)))
                  (html-tag-name (cadr (content-style-properties style)))))
          (test-equal "cache preserves layout fragments"
            (list 'newline "\n" source)
            (list (content-layout-kind layout) (content-layout-raw layout)
                  (content-layout-source layout))))))
    (parameterize ((current-input-cache
                    (make-input-cache-context #t '())))
      (let ((document
              (make-content-document
                (list (make-content-element 'custom-void '()
                        #:style (make-content-style #f
                                 (list (make-html-void))))))))
        (cached-document source "void-codec" '() (lambda () document)
                         (lambda _ #t))
        (let* ((decoded
                 (cached-document source "void-codec" '()
                   (lambda () (error "expected a cached void marker"))
                   (lambda _ #t)))
               (element (car (content-document-children decoded)))
               (marker (car (content-style-properties
                              (content-element-style element)))))
          (test-assert "cache preserves the HTML void marker"
            (html-void? marker)))))
    (let ((reads 0)
          (document (make-content-document
                      (list (make-content-element 'forest-query '()
                              #:properties `((query . ,(lambda _ #t))))))))
      (parameterize ((current-input-cache
                      (make-input-cache-context #t '())))
        (do ((index 0 (+ index 1))) ((= index 2))
          (cached-document source "closure" '()
            (lambda () (set! reads (+ reads 1)) document) (lambda _ #t))))
      (test-equal "closure-bearing content is re-evaluated" 2 reads))
    (let ((cycle (list 'cycle)))
      (set-cdr! cycle cycle)
      (test-assert "cyclic data is rejected without looping"
        (catch 'uncacheable-content
          (lambda () ((@@ (grove private input cache) pack) cycle) #f)
          (lambda _ #t))))
    (parameterize ((current-input-fingerprints (make-hash-table))
                  (current-input-cache
                    (make-input-cache-context #t '())))
      (let ((reads 0))
        (define (read-changing-source)
          (set! reads (+ reads 1))
          (put source "changed while evaluating")
          rich-document)
        (cached-document source "unstable" '() read-changing-source (lambda _ #t))
        (cached-document source "unstable" '() read-changing-source (lambda _ #t))
        (test-equal "an input changed during evaluation is not cached" 2 reads)))

    (let ((provider (string-append root "/provider.scm")))
      (define (load-provider value)
        (put provider
          (format #f "(define-module (tests cache-provider) #:export (cached-value))\n(define (cached-value) ~s)\n" value))
        (save-module-excursion (lambda () (primitive-load provider))))
      (load-provider "one")
      (put source "@use-modules[(tests cache-provider)]\n@(cached-value)")
      (build-project (project #t))
      (let ((before evaluations))
        (build-project (project #t))
        (test-equal "module-backed documents reuse results by default" before evaluations)
        (load-provider "two")
        (build-project (project #t))
        (test-equal "imported code changes invalidate documents without declarations"
          (+ before 1) evaluations)
        (test-assert "module changes reach renderer outputs too"
          (string-contains
            (call-with-input-file (string-append output "/page.html") get-string-all)
            "two"))))
    (let* ((provider (string-append root "/provider.scm"))
           (compiled (string-append root "/provider.go"))
           (original (call-with-input-file provider get-string-all))
           (old-source-path %load-path)
           (old-compiled-path %load-compiled-path))
      (dynamic-wind
        (lambda ()
          (set! %load-path (cons root %load-path))
          (set! %load-compiled-path (cons root %load-compiled-path)))
        (lambda ()
          (build-project (project #t))
          (let ((before evaluations))
            (build-project (project #t))
            (test-equal "absent compiled inputs permit reuse" before evaluations)
            (compile-file provider #:output-file compiled)
            (save-module-excursion (lambda () (load-compiled compiled)))
            (build-project (project #t))
            (test-equal "adding a preferred compiled module invalidates a cached source"
              (+ before 1) evaluations))
          (let ((before evaluations))
            (put provider "(define-module (tests cache-provider) #:export (cached-value))\n(define (cached-value) \"three\")\n")
            (compile-file provider #:output-file compiled)
            (put provider original)
            (save-module-excursion (lambda () (load-compiled compiled)))
            (build-project (project #t))
            (test-equal "compiled module changes invalidate an unchanged source"
              (+ before 1) evaluations)
            (test-assert "new compiled code reaches rendered output"
              (string-contains
                (call-with-input-file (string-append output "/page.html") get-string-all)
                "three"))))
        (lambda ()
          (set! %load-path old-source-path)
          (set! %load-compiled-path old-compiled-path))))
    (let ((data (string-append root "/data.txt")))
      (put data "alpha")
      (put source (format #f
        "@use-modules[(grove input) (ice-9 textual-ports)]\n@(call-with-input-file (depend-on! ~s) get-string-all)" data))
      (build-project (project #t))
      (let ((before evaluations))
        (build-project (project #t))
        (test-equal "declared external data is reusable" before evaluations)
        (put data "beta")
        (let ((report (build-project/report (project #t))))
          (test-equal "file inputs registered during evaluation invalidate the document"
            (+ before 1) evaluations)
          (test-assert "registered data participates in watching"
            (member data (build-report-sources report)))
          (test-assert "registered data updates rendered output"
            (string-contains
              (call-with-input-file (string-append output "/page.html") get-string-all)
              "beta"))))
      (let* ((uncached-project (project #f))
             (before evaluations)
             (report (build-project/report uncached-project)))
        (test-assert "disabling cache preserves document dependency tracking"
          (member data (build-report-sources report)))
        (test-equal "disabled cache still evaluates the document"
          (+ before 1) evaluations)
        (put data "gamma")
        (let ((before evaluations)
              (report (build-project/report uncached-project)))
          (test-assert "disabled cache reports changed document dependencies"
            (member data (build-report-sources report)))
          (test-equal "disabled cache reevaluates after dependency changes"
            (+ before 1) evaluations)
          (test-assert "disabled cache renders changed dependency contents"
            (string-contains
              (call-with-input-file (string-append output "/page.html") get-string-all)
              "gamma")))))
    (let* ((first-target (string-append root "/target-first.txt"))
           (second-target (string-append root "/target-second.txt"))
           (alias (string-append root "/data-alias.txt")))
      (put first-target "same target")
      (put second-target "same target")
      (symlink first-target alias)
      (put source (format #f
        "@use-modules[(grove input) (ice-9 textual-ports)]\n@(let ((resolved (depend-on! ~s))) (string-append (call-with-input-file resolved get-string-all) \" \" resolved))"
        alias))
      (build-project (project #t))
      (test-assert "registered symlink input reads its original target"
        (let ((html (call-with-input-file
                      (string-append output "/page.html") get-string-all)))
          (and (string-contains html "same target")
               (string-contains html first-target))))
      (let ((before evaluations))
        (delete-file alias)
        (symlink second-target alias)
        (build-project (project #t))
        (test-equal "retargeting a registered symlink invalidates the document"
          (+ before 1) evaluations)
        (test-assert "retargeting a registered symlink updates path-sensitive output"
          (string-contains
            (call-with-input-file (string-append output "/page.html") get-string-all)
            second-target)))
      (delete-file alias))
    (put source "@use-modules[(grove input)]\n@(disable-cache!)\nAlways execute")
    (let ((before evaluations))
      (build-project (project #t))
      (test-assert "document opt-out also prevents renderer reuse"
        (not (build-report-render-cache-hit?
               (build-project/report (project #t)))))
      (test-equal "document opt-out evaluates on every build" (+ before 2) evaluations))
    (put source "@(define-module (tests cached-article-module))\nModule body")
    (let ((before evaluations))
      (build-project (project #t))
      (build-project (project #t))
      (test-equal "named module definitions are never skipped" (+ before 2) evaluations))
    (let* ((renders 0)
           (render (lambda (_)
                     (set! renders (+ renders 1))
                     (list (make-artifact "plain.txt" "constant"
                                          #:sources '()))))
           (plain (make-project
                    #:collect-sources (lambda () (list source))
                    #:read-source (lambda (path) (call-with-input-file path get-string-all))
                    #:renderers (list render)
                    #:output-directory (string-append root "/plain"))))
      (let ((first (build-project/report plain)))
        (test-equal "plain project renderers rerun without a cache declaration"
          1 renders)
        (test-equal "a plain renderer's first output is affected"
          '("plain.txt") (build-report-affected-artifacts first)))
      (let ((second (build-project/report plain)))
        (test-equal "plain project renderers rerun without a cache declaration"
          2 renders)
        (test-assert "a rerender is not a complete renderer cache hit"
          (not (build-report-render-cache-hit? second)))
        (test-equal "identical declared-independent output is not affected"
          '() (build-report-affected-artifacts second)))
      (let* ((replacement-renders 0)
             (replacement
              (make-project
                #:collect-sources (lambda () (list source))
                #:read-source (lambda (path)
                                (call-with-input-file path get-string-all))
                #:renderers
                (list (lambda (_)
                        (set! replacement-renders (+ replacement-renders 1))
                        (list (make-artifact "plain.txt" "replacement"))))
                #:output-directory (string-append root "/plain"))))
        (build-project replacement)
        (test-equal "a replacement renderer runs against an existing output cache"
          1 replacement-renders)
        (test-equal "replacement renderer output replaces the prior project's bytes"
          "replacement"
          (call-with-input-file (string-append root "/plain/plain.txt")
            get-string-all)))
      (build-project (project-with-options plain #:cache? #f))
      (test-equal "project opt-out forces rendering" 3 renders)
      (let ((uncached (make-project
                       #:collect-sources (lambda () (list source))
                       #:read-source (lambda (path) (call-with-input-file path get-string-all))
                       #:renderers (list render)
                       #:output-directory (string-append root "/plain"))))
        (build-project uncached)
        (build-project uncached)
        (test-equal "a plain renderer runs on every build" 5 renders)))
      (let* ((cached-runs 0)
             (plain-runs 0)
             (mixed
              (make-project
                #:collect-sources (lambda () (list source))
                #:read-source (lambda (path)
                                (call-with-input-file path get-string-all))
                #:renderers
                (list
                  (make-cached-renderer
                    (lambda (_)
                      (set! cached-runs (+ cached-runs 1))
                      (list (make-artifact "cached.txt" "stable")))
                    #:configuration 'stable)
                  (lambda (_)
                    (set! plain-runs (+ plain-runs 1))
                    (list (make-artifact "plain.txt"
                                         (number->string plain-runs)))))
                #:output-directory (string-append root "/mixed-cache"))))
        (build-project mixed)
        (build-project mixed)
        (test-equal "a plain renderer does not invalidate its cached peer"
          1 cached-runs)
        (test-equal "a plain renderer reruns independently"
          2 plain-runs)
        (test-equal "the plain renderer publishes its latest bytes"
          "2"
          (call-with-input-file (string-append root "/mixed-cache/plain.txt")
            get-string-all)))
      (let* ((cached-runs 0)
             (uncached-runs 0)
             (mixed
              (make-project
                #:collect-sources (lambda () (list source))
                #:read-source (lambda (path)
                                (call-with-input-file path get-string-all))
                #:renderers
                (list
                  (make-cached-renderer
                    (lambda (_)
                      (set! cached-runs (+ cached-runs 1))
                      (list (make-artifact "cached.txt" "cached"))))
                  (lambda (_)
                    (set! uncached-runs (+ uncached-runs 1))
                    (list (make-artifact "uncached.txt" "uncached"))))
                #:output-directory (string-append root "/mixed"))))
        (build-project mixed)
        (build-project mixed)
        (test-equal "an uncached renderer does not disable its cached peer"
          '(1 2) (list cached-runs uncached-runs)))
      (let* ((stable-runs 0)
             (changing-runs 0)
             (make-mixed
              (lambda (configuration)
                (make-project
                  #:collect-sources (lambda () (list source))
                  #:read-source (lambda (path)
                                  (call-with-input-file path get-string-all))
                  #:renderers
                  (list
                    (make-cached-renderer
                      (lambda (_)
                        (set! stable-runs (+ stable-runs 1))
                        (list (make-artifact "stable.txt" "stable")))
                      #:configuration 'stable)
                    (make-cached-renderer
                      (lambda (_)
                        (set! changing-runs (+ changing-runs 1))
                        (list (make-artifact "changing.txt" "changing")))
                      #:configuration configuration))
                  #:output-directory (string-append root "/mixed-config"))))
             (_ (build-project (make-mixed 'first)))
             (changed (build-project/report (make-mixed 'second))))
        (test-equal "a changed renderer configuration leaves its peer cached"
          '(1 2) (list stable-runs changing-runs))
        (test-equal "only artifacts owned by the changed renderer are affected"
          '("changing.txt") (build-report-affected-artifacts changed)))
    (prune-input-cache! (make-input-cache-context #t '()))
    ;;     The prune owns the input-cache subtree: entries outside the
    ;;   current selection go, while sibling subtrees (artifacts) are
    ;;   the store GC's business.
    (test-equal "entries outside the current selection are removed"
      '("." "..") (scandir (string-append (grove-store-directory)
                                          "/input-cache")))
    (test-equal "input cache path is an ordinary artifact path"
      '(".grove-input-cache/user")
      (map artifact-path
        (validate-artifacts
          (list (make-artifact ".grove-input-cache/user" "x"))))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "document result cache")
