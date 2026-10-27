;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove input-cache)
  #:use-module (grove ir)
  #:use-module (grove build)
  #:use-module (grove private build-state)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove input scribble)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove project)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove private cache-dir)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (system base compile)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define root (format #f "/tmp/grove-input-cache-~a" (getpid)))
(define output (string-append root "/output"))
(define source (string-append root "/page.plant"))
(define dependency (string-append root "/dependency"))
(define evaluations 0)
(define analyses 0)
(define partial-renders 0)
(define statuses '())
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
                            (sxml->html (content->sxml document)) (list source))))))
      (list (make-cached-renderer render #:configuration 'html
              #:render-changed
              (lambda (document changed dirty old)
                (set! partial-renders (+ partial-renders 1))
                (render document)))))
    #:cache? cache? #:output-directory output))
(define rich-document
  (make-content-document
    (list (make-content-element 'paragraph
            (list (make-content-text "λ" source
                    #:location `((file . ,source) (line . 3) (column . 2)))
                  (make-content-layout 'newline "\n" source))
            source #:flow-kind 'block
            #:style (make-content-style 'sample
                     (list (make-html-attributes '((class . "extra")))
                           (make-html-tag "aside")))
            #:properties '((open? . #f) (arbitrary . (text "literal")))))
    source))

(test-begin "document result cache")
(dynamic-wind
  (lambda ()
    (setenv "XDG_CACHE_HOME" (string-append root "/.cache"))
    (mkdir root)
    (put source "First") (put dependency "one"))
  (lambda ()
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
    (build-project (project #t) #:progress
      (lambda (stage detail)
        (when (eq? stage 'input-cache) (set! statuses (cons detail statuses)))))
    (build-project (project #t) #:progress
      (lambda (stage detail)
        (when (eq? stage 'input-cache) (set! statuses (cons detail statuses)))))
    (test-equal "unchanged source evaluates once" 1 evaluations)
    (test-equal "whole model is analyzed even on an input hit" 2 analyses)
    (test-equal "progress reports miss then hit" '((1 . 1) (0 . 1)) statuses)
    (put source "Second")
    (build-project (project #t))
    (test-equal "source change invalidates its result" 2 evaluations)
    (test-equal "ordinary source changes retain incremental rendering" 1 partial-renders)
    (put dependency "two")
    (test-assert "dependency changes also invalidate renderer cache"
      (build-report-rendered? (build-project/report (project #t))))
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
                    (make-input-cache-context output #t (list dependency))))
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
                  (content-text-source text) (content-text-location text)))
          (test-equal "cache preserves element identity and arbitrary data properties"
            (list 'paragraph 'block source
                  '((open? . #f) (arbitrary . (text "literal"))))
            (list (content-element-kind element) (content-element-flow-kind element)
                  (content-element-source element) (content-element-properties element)))
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
                    (make-input-cache-context output #t '())))
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
                      (make-input-cache-context output #t '())))
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
                    (make-input-cache-context output #t '())))
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
        "@use-modules[(grove input scribble) (ice-9 textual-ports)]\n@(call-with-input-file (depend-on! ~s) get-string-all)" data))
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
              "beta")))))
    (put source "@use-modules[(grove input scribble)]\n@(disable-cache!)\nAlways execute")
    (let ((before evaluations))
      (build-project (project #t))
      (test-assert "document opt-out also prevents renderer reuse"
        (build-report-rendered? (build-project/report (project #t))))
      (test-equal "document opt-out evaluates on every build" (+ before 2) evaluations))
    (put source "@(define-module (tests cached-article-module))\nModule body")
    (let ((before evaluations))
      (build-project (project #t))
      (build-project (project #t))
      (test-equal "named module definitions are never skipped" (+ before 2) evaluations))
    (let* ((renders 0)
           (render (lambda (_)
                     (set! renders (+ renders 1))
                     (list (make-artifact "plain.txt" "constant"))))
           (plain (make-project
                    #:collect-sources (lambda () (list source))
                    #:read-source (lambda (path) (call-with-input-file path get-string-all))
                    #:renderers (list render)
                    #:output-directory (string-append root "/plain"))))
      (build-project plain)
      (build-project plain)
      (test-equal "plain project renderers cache by default without keys" 1 renders)
      (build-project (project-with-options plain #:cache? #f))
      (test-equal "project opt-out forces rendering" 2 renders)
      (let ((uncached (make-project
                       #:collect-sources (lambda () (list source))
                       #:read-source (lambda (path) (call-with-input-file path get-string-all))
                       #:renderers (list (make-cached-renderer render #:cache? #f))
                       #:output-directory (string-append root "/plain"))))
        (build-project uncached)
        (build-project uncached)
        (test-equal "renderer opt-out forces rendering on every build" 4 renders)))
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
                  (make-cached-renderer
                    (lambda (_)
                      (set! uncached-runs (+ uncached-runs 1))
                      (list (make-artifact "uncached.txt" "uncached")))
                    #:cache? #f))
                #:output-directory (string-append root "/mixed"))))
        (build-project mixed)
        (build-project mixed)
        (test-equal "one uncached renderer disables the shared artifact cache"
          '(2 2) (list cached-runs uncached-runs)))
    (prune-input-cache! (make-input-cache-context output #t '()))
    ;;     The prune owns the input-cache subtree: entries outside the
    ;;   current selection go, while sibling subtrees (artifacts) are
    ;;   the store GC's business.
    (test-equal "entries outside the current selection are removed"
      '("." "..") (scandir (string-append (grove-store-directory)
                                          "/input-cache")))
    (test-error "input cache directory is reserved from artifacts" #t
      (validate-artifacts (list (make-artifact ".grove-input-cache/user" "x")))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "document result cache")
