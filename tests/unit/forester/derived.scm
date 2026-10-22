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

(test-equal "derived composition has an identity"
  (compose-derived-artifacts (car artifacts))
  (compose-derived-artifacts (car artifacts) (compose-derived-artifacts)))

(test-equal "derived composition is associative"
  (compose-derived-artifacts
    (car artifacts)
    (compose-derived-artifacts (cadr artifacts) (caddr artifacts)))
  (compose-derived-artifacts
    (compose-derived-artifacts (car artifacts) (cadr artifacts))
    (caddr artifacts)))

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

(test-end "forester derived artifacts")
