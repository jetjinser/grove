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
  #:use-module (grove project)
  #:use-module (grove site))

(define platform-root
  (format #f "/tmp/grove-platform-test-~a" (getpid)))
(define source-directory (string-append platform-root "/source"))
(define output-directory (string-append platform-root "/output"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(test-begin "grove platform contracts")

(dynamic-wind
  (lambda ()
    (mkdir platform-root)
    (mkdir source-directory)
    (write-test-file (string-append source-directory "/alpha.grv") "alpha")
    (write-test-file (string-append source-directory "/beta.grv") "beta"))
  (lambda ()
    (let* ((rendered-models '())
           (project
            (make-project
              #:source-directory source-directory
              #:read-page
              (lambda (path)
                (let ((id (string-drop-right (basename path) 4)))
                  (make-site-page id id (make-content-document '()) path)))
              #:analyze
              (lambda (site)
                (map site-page-id (site-pages site)))
              #:renderers
              (list
                (lambda (page-ids)
                  (set! rendered-models (append rendered-models (list page-ids)))
                  (list (make-artifact "pages.list"
                                       (string-join page-ids "\n"))))
                (lambda (page-ids)
                  (set! rendered-models (append rendered-models (list page-ids)))
                  (list (make-artifact "page-count"
                                       (number->string (length page-ids)))))
              )
              #:output-directory output-directory))
           (artifacts (build-project project)))
      (test-equal "analyzer model is opaque to Grove"
        '("alpha" "beta")
        (car rendered-models))
      (test-equal "multiple independent renderers receive the same model"
        '(("alpha" "beta") ("alpha" "beta"))
        rendered-models)
      (test-equal "independent renderers contribute their own artifact sets"
        '("pages.list" "page-count")
        (map artifact-path artifacts))
      (test-equal "first output format renders the custom model"
        "alpha\nbeta"
        (call-with-input-file
          (string-append output-directory "/pages.list") get-string-all))
      (test-equal "second output format renders the same custom model"
        "2"
        (call-with-input-file
          (string-append output-directory "/page-count") get-string-all))))
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
            (string-append source-directory "/alpha.grv")
            (string-append source-directory "/beta.grv")))
    (when (file-exists? output-directory)
      (rmdir output-directory))
    (when (file-exists? source-directory)
      (rmdir source-directory))
    (when (file-exists? platform-root)
      (rmdir platform-root))))

(test-end "grove platform contracts")
