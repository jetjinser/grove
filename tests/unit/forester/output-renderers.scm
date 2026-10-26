;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester output-renderers)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-13)
  #:use-module (forester render graph)
  #:use-module (forester render rss)
  #:use-module (forester render site)
  #:use-module (forester render sitemap)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (tests unit forester fixtures))

(define forest
  (make-forest
    (make-site
      (list (make-site-page
              "guide-en" "Guide"
              (evaluate-document "@paragraph{English.}")
              #:metadata '((locale . "en") (translation . "guide")
                           (published . "2026-01-02")))
            (make-site-page
              "guide-zh" "指南"
              (evaluate-document "@paragraph{中文。}")
              #:metadata '((locale . "zh-Hans") (translation . "guide"))))
      #:title "Test"
      #:home "guide-en")))

(test-begin "Forester output renderers")

(let* ((settings (make-site-settings
                   #:site-url "https://example.org"
                   #:default-locale "en"))
       (routes (make-html-routes #:home-page "guide-en"))
       (renderers
        (list (make-sitemap-renderer #:settings settings #:routes routes)
              (make-rss-renderer #:settings settings #:routes routes)
              (make-graph-renderer)))
       (artifacts (render-model forest renderers)))
  (test-equal "site projections use the ordinary renderer list"
    '("sitemap.xml" "feed.xml" "forest-graph.scm")
    (map artifact-path artifacts))
  (test-assert "sitemap renderer uses shared site settings"
    (let ((content (artifact-content (car artifacts))))
      (and (string-contains content "https://example.org")
           (string-contains content "hreflang=\"x-default\""))))
  (test-assert "RSS renderer uses shared site settings"
    (string-contains (artifact-content (cadr artifacts))
                     "<language>en</language>")))

(test-error "sitemap renderer rejects invalid settings at construction" #t
  (make-sitemap-renderer #:settings 'not-site-settings))
(test-error "RSS renderer rejects invalid settings at construction" #t
  (make-rss-renderer #:settings 'not-site-settings))

(test-end "Forester output renderers")
