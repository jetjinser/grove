;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove input-cache)
  #:use-module (grove ir)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove input scribble)
  #:use-module (grove private input cache)
  #:use-module (grove project)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (ice-9 ftw)
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
    #:cacheable? (const #t)
    #:cache-dependencies (lambda () (list dependency))
    #:timings (lambda (path phase seconds)
                (when (eq? phase 'evaluate)
                  (set! evaluations (+ evaluations 1))))))
(define (project key)
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
      (list (make-cached-renderer render #:cache-key "renderer"
              #:render-changed
              (lambda (document changed dirty old)
                (set! partial-renders (+ partial-renders 1))
                (render document)))))
    #:cache-key key #:output-directory output))
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
  (lambda () (mkdir root) (put source "First") (put dependency "one"))
  (lambda ()
    (build-project (project "key") #:progress
      (lambda (stage detail)
        (when (eq? stage 'input-cache) (set! statuses (cons detail statuses)))))
    (build-project (project "key") #:progress
      (lambda (stage detail)
        (when (eq? stage 'input-cache) (set! statuses (cons detail statuses)))))
    (test-equal "unchanged source evaluates once" 1 evaluations)
    (test-equal "whole model is analyzed even on an input hit" 2 analyses)
    (test-equal "progress reports miss then hit" '((1 . 1) (0 . 1)) statuses)
    (put source "Second")
    (build-project (project "key"))
    (test-equal "source change invalidates its result" 2 evaluations)
    (test-equal "ordinary source changes retain incremental rendering" 1 partial-renders)
    (put dependency "two")
    (test-assert "dependency changes also invalidate renderer cache"
      (build-report-rendered? (build-project/report (project "key"))))
    (test-equal "declared dependency change invalidates result" 3 evaluations)
    (test-equal "reader dependencies conservatively force a full render" 1 partial-renders)
    (build-project (project "new-key"))
    (test-equal "project key change invalidates result" 4 evaluations)
    (let* ((directory (string-append output "/.grove-input-cache"))
           (entry (string-append directory "/"
                    (car (scandir directory (lambda (name)
                                             (not (member name '("." "..")))))))))
      (define (snapshot)
        (let ((info (stat entry)))
          (list (file-fingerprint entry) (stat:ino info) (stat:mtime info))))
      (let ((before (snapshot)))
        (check-project (project "new-key"))
        (test-equal "check preserves the cache file without rewriting it"
          before (snapshot))))
    (test-equal "check always evaluates the source" 5 evaluations)
    (build-project (project "new-key"))
    (test-equal "check does not replace the stored entry" 5 evaluations)
    (let* ((directory (string-append output "/.grove-input-cache"))
           (entry (string-append directory "/"
                    (car (scandir directory (lambda (name)
                                             (not (member name '("." "..")))))))))
      (put entry "malformed")
      (build-project (project "new-key"))
      (test-equal "corrupt cache falls back to source evaluation" 6 evaluations))
    (reader source)
    (test-equal "standalone reader evaluates normally" 7 evaluations)
    (parameterize ((current-input-cache
                    (make-input-cache-context output "codec" (list dependency))))
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
    (let ((reads 0)
          (document (make-content-document
                      (list (make-content-element 'forest-query '()
                              #:properties `((query . ,(lambda _ #t))))))))
      (parameterize ((current-input-cache
                      (make-input-cache-context output "closure" '())))
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
    (parameterize ((current-input-cache
                    (make-input-cache-context output "changed-during-read" '())))
      (let ((reads 0))
        (define (read-changing-source)
          (set! reads (+ reads 1))
          (put source "changed while evaluating")
          rich-document)
        (cached-document source "unstable" '() read-changing-source (lambda _ #t))
        (cached-document source "unstable" '() read-changing-source (lambda _ #t))
        (test-equal "an input changed during evaluation is not cached" 2 reads)))
    (prune-input-cache! (make-input-cache-context output "none" '()))
    (test-equal "entries outside the current selection are removed"
      '("." "..") (scandir (string-append output "/.grove-input-cache")))
    (test-error "input cache directory is reserved from artifacts" #t
      (validate-artifacts (list (make-artifact ".grove-input-cache/user" "x")))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "document result cache")
