;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester derived)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (forester render derived)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (grove output)
  #:use-module (grove render context)
  #:use-module (grove render derived)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (tests unit forester fixtures))

(define page
  (make-site-page
    "root" "Root"
    (evaluate-document "@paragraph{Hello.}")
    #:metadata '((published . "2026-01-02"))))

(define forest
  (make-forest
    (make-site (list page) #:title "Test" #:home "root")))

(define context
  (make-render-context
    #:site-url "https://example.org"
    #:default-locale "en"
    #:locale "en"))

(define routes (make-html-routes #:home-page "root"))

(define artifacts
  (list (sitemap-derived #:routes routes)
        (rss-derived #:routes routes)
        (graph-derived)))

(test-begin "forester derived artifacts")

(let ((first (sitemap-derived #:routes routes #:sitemap-path "first.xml"))
      (second (sitemap-derived #:routes routes #:sitemap-path "second.xml")))
  (test-assert "derived configuration tracks the artifact path"
    (not (equal? ((derived-artifact-configuration first) context forest)
                 ((derived-artifact-configuration second) context forest)))))

(let ((rendered (render-derived-artifacts artifacts context forest)))
  (test-equal "derived artifacts keep their paths"
    '("sitemap.xml" "feed.xml" "forest-graph.scm")
    (map artifact-path rendered))
  (test-assert "sitemap uses the context site URL"
    (string-contains
      (artifact-content (car rendered))
      "https://example.org"))
  (test-assert "rss uses the context language"
    (string-contains
      (artifact-content (cadr rendered))
      "<language>en</language>")))

(test-error "derived artifacts require a configuration" #t
  (make-derived-artifact
    #:name "unconfigured"
    #:procedure (lambda (context forest) '())))

(test-end "forester derived artifacts")
