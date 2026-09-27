;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove platform)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove project))

(define platform-root
  (format #f "/tmp/grove-platform-test-~a" (getpid)))
(define source-directory (string-append platform-root "/source"))
(define alpha-source (string-append source-directory "/alpha.data"))
(define beta-source (string-append source-directory "/beta.data"))
(define theme-source (string-append platform-root "/theme.css"))
(define output-directory (string-append platform-root "/output"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(test-begin "grove platform contracts")

(dynamic-wind
  (lambda ()
    (mkdir platform-root)
    (mkdir source-directory)
    (write-test-file alpha-source "alpha")
    (write-test-file beta-source "beta")
    (write-test-file theme-source "body { color: black; }"))
  (lambda ()
    (let* ((rendered-models '())
           (analyzed-context #f)
           (collected-sources (list beta-source alpha-source))
           (project
            (make-project
              #:collect-sources (lambda () collected-sources)
              #:extra-sources (list theme-source)
              #:read-source
              (lambda (path)
                (call-with-input-file path get-string-all))
              #:analyze
              (lambda (context)
                (set! analyzed-context context)
                (project-context-inputs context))
              #:renderers
              (list
                (lambda (inputs)
                  (let ((values (map project-input-value inputs))
                        (sources (map project-input-path inputs)))
                    (set! rendered-models
                          (append rendered-models (list values)))
                    (list (make-artifact "pages.list"
                                         (string-join values "\n")
                                         sources))))
                (lambda (inputs)
                  (let ((values (map project-input-value inputs))
                        (sources (map project-input-path inputs)))
                    (set! rendered-models
                          (append rendered-models (list values)))
                    (list (make-artifact "page-count"
                                         (number->string (length values))
                                         sources))))
              )
              #:output-directory output-directory))
           (first-report (build-project/report project))
           (artifacts (build-report-artifacts first-report)))
      (test-assert "analyzer model is opaque to Grove"
        (and (project-context? analyzed-context)
             (equal? (map project-input-path
                          (project-context-inputs analyzed-context))
                     (list beta-source alpha-source))
             (equal? (project-context-extra-sources analyzed-context)
                     (list theme-source))))
      (test-equal "build reports track generic and auxiliary inputs"
        (list beta-source alpha-source theme-source)
        (build-report-added-sources first-report))
      (test-equal "readers can return non-page project values"
        '("beta" "alpha")
        (car rendered-models))
      (test-equal "multiple independent renderers receive the same model"
        '(("beta" "alpha") ("beta" "alpha"))
        rendered-models)
      (test-equal "independent renderers contribute their own artifact sets"
        '("pages.list" "page-count")
        (map artifact-path artifacts))
      (test-equal "first output format renders the custom model"
        "beta\nalpha"
        (call-with-input-file
          (string-append output-directory "/pages.list") get-string-all))
      (test-equal "second output format renders the same custom model"
        "2"
        (call-with-input-file
          (string-append output-directory "/page-count") get-string-all))
      (write-test-file alpha-source "alpha updated")
      (write-test-file theme-source "body { color: blue; }")
      (let ((updated (build-project/report project)))
        (test-equal "build reports identify changed generic inputs"
          (list alpha-source theme-source)
          (build-report-modified-sources updated))
        (test-equal "artifact provenance limits affected outputs to dependencies"
          '("pages.list" "page-count")
          (build-report-affected-artifacts updated))
        (set! collected-sources (list alpha-source))
        (delete-file beta-source)
        (let ((removed (build-project/report project)))
          (test-equal "build reports identify removed custom-collected inputs"
            (list beta-source)
            (build-report-removed-sources removed))
          (test-equal "renderer artifact sets update after input removal"
            '("alpha updated")
            (last rendered-models))))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list (string-append output-directory "/pages.list")
            (string-append output-directory "/page-count")
            (string-append output-directory "/.grove-artifacts")
            (string-append output-directory "/.grove-artifacts.tmp")
            (string-append output-directory "/.grove-build-state")
            (string-append output-directory "/.grove-build-state.tmp")
            alpha-source
            beta-source
            theme-source))
    (when (file-exists? output-directory)
      (rmdir output-directory))
    (when (file-exists? source-directory)
      (rmdir source-directory))
    (when (file-exists? platform-root)
      (rmdir platform-root))))

(test-end "grove platform contracts")
