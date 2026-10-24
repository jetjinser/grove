;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester derived)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (forester render derived)
  #:use-module (forester render context)
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

(define translated-forest
  (make-forest
    (make-site
      (list (make-site-page
              "guide-en" "Guide"
              (evaluate-document "@paragraph{English.}")
              #:metadata '((locale . "en") (translation . "guide")))
            (make-site-page
              "guide-zh" "指南"
              (evaluate-document "@paragraph{中文。}")
              #:metadata '((locale . "zh-Hans") (translation . "guide")))))))

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

(let* ((settings (make-site-settings
                   #:site-url "https://settings.example"
                   #:default-locale "zh-Hans"))
       (context-with-conflicting-defaults
        (make-render-context
          #:site-url "https://context.example"
          #:default-locale "en"
          #:locale "en"))
       (translated-routes (make-html-routes))
       (sitemap (sitemap-derived #:settings settings
                                 #:routes translated-routes))
       (rss (rss-derived #:settings settings #:routes translated-routes))
       (sitemap-fallback
        (sitemap-derived #:settings settings #:routes translated-routes
                         #:default-locale #f))
       (rss-fallback
        (rss-derived #:settings settings #:routes translated-routes
                     #:language #f))
       (sitemap-configuration
        ((derived-artifact-configuration sitemap)
         context-with-conflicting-defaults translated-forest))
       (rss-configuration
        ((derived-artifact-configuration rss)
         context-with-conflicting-defaults translated-forest))
       (sitemap-fallback-configuration
        ((derived-artifact-configuration sitemap-fallback)
         context-with-conflicting-defaults translated-forest))
       (rss-fallback-configuration
        ((derived-artifact-configuration rss-fallback)
         context-with-conflicting-defaults translated-forest))
       (rendered
        (render-derived-artifacts
         (list sitemap rss)
         context-with-conflicting-defaults translated-forest))
       (sitemap-content (artifact-content (car rendered)))
       (rss-content (artifact-content (cadr rendered))))
  (test-equal "derived sitemap settings override context URL and locale"
    '("https://settings.example" "zh-Hans")
    (list (list-ref sitemap-configuration 2)
          (list-ref sitemap-configuration 3)))
  (test-equal "derived RSS settings override context URL and language"
    '("https://settings.example" "zh-Hans")
    (list (list-ref rss-configuration 2)
          (list-ref rss-configuration 5)))
  (test-equal "false locale overrides still inherit shared settings"
    '("zh-Hans" "zh-Hans")
    (list (list-ref sitemap-fallback-configuration 3)
          (list-ref rss-fallback-configuration 5)))
  (test-assert "sitemap settings select the x-default translation"
    (and (string-contains sitemap-content "hreflang=\"x-default\"")
         (string-contains sitemap-content
                          "https://settings.example/guide-zh.html")))
  (test-assert "RSS settings provide the feed language"
    (string-contains rss-content "<language>zh-Hans</language>")))

(test-error "sitemap-derived rejects invalid site settings at construction" #t
  (sitemap-derived #:settings 'not-site-settings))
(test-error "rss-derived rejects invalid site settings at construction" #t
  (rss-derived #:settings 'not-site-settings))
(test-error "site render context rejects invalid site settings" #t
  (make-site-render-context forest #:settings 'not-site-settings))

(test-error "derived artifacts require a configuration" #t
  (make-derived-artifact
    #:name "unconfigured"
    #:procedure (lambda (context forest) '())))

(test-end "forester derived artifacts")
