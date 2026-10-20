;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester i18n-routes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester input scribble)
  #:use-module (forester render html)
  #:use-module (forester render sitemap)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (tests unit forester fixtures))

(test-begin "forester i18n routes")

(define (make-page id locale translation)
  (make-site-page
    id (string-append "Page " id)
    (evaluate-document "@paragraph{Body.}")
    #:metadata (append (if locale (list (cons 'locale locale)) '())
                       (if translation (list (cons 'translation translation)) '()))))

(define home (make-page "home" #f #f))
(define guide-en (make-page "guide" "en" "guide"))
(define guide-zh (make-page "guide-zh" "zh-Hans" "guide"))
(define forest (make-forest (make-site (list home guide-en guide-zh))))

(define (locale-of id)
  (let ((page (find-page forest id)))
    (and page (assq-ref (site-page-metadata page) 'locale))))

(define routes
  (make-html-routes #:home-page "home"
                    #:page-path (locale-page-path locale-of
                                                  #:default-locale "en")))

(test-equal "locale page paths leave default locales unprefixed"
  "guide.html" (html-page-path routes "guide"))
(test-equal "locale page paths prefix other locales"
  "zh-Hans/guide-zh.html" (html-page-path routes "guide-zh"))

(let ((page (assoc-ref (site->html forest #:routes routes #:mathjax-src #f
                                   #:hreflang? #t #:default-locale "en"
                                   #:language-switcher? #t)
                       "guide")))
  (test-assert "translated pages emit hreflang alternates"
    (and (string-contains page
                          "rel=\"alternate\" hreflang=\"en\" href=\"/guide.html\"")
         (string-contains page
                          "hreflang=\"zh-Hans\" href=\"/zh-Hans/guide-zh.html\"")
         (string-contains page
                          "hreflang=\"x-default\" href=\"/guide.html\"")))
  (test-assert "language switcher marks the current page"
    (and (string-contains
           page "<nav class=\"language-switcher\" aria-label=\"Language\">")
         (string-contains page "aria-current=\"true\""))))

(let ((page (assoc-ref (site->html forest #:routes routes #:mathjax-src #f)
                       "guide")))
  (test-assert "alternates stay off by default"
    (not (string-contains page "rel=\"alternate\""))))

(let ((sitemap (artifact-content
                 (car (render-sitemap forest #:site-url "https://example.org"
                                      #:routes routes
                                      #:default-locale "en")))))
  (test-assert "sitemap carries hreflang alternates"
    (and (string-contains sitemap "xhtml:link")
         (string-contains sitemap "hreflang=\"zh-Hans\"")
         (string-contains sitemap "hreflang=\"x-default\""))))

(let ((page (document->tree-page
              "x"
              (evaluate-document
                "@title{X}\n@locale[\"zh-Hans\"]\n@translation[\"intro\"]\n@paragraph{Body.}")
              #f)))
  (test-equal "reader extracts the page locale" "zh-Hans"
    (assq-ref (site-page-metadata page) 'locale))
  (test-equal "reader extracts the translation key" "intro"
    (assq-ref (site-page-metadata page) 'translation))
  (test-assert "locale metadata leaves the body"
    (not (any (lambda (child)
                (memq (content-element-kind child) '(locale translation)))
              (content-document-children (site-page-document page))))))

(test-end "forester i18n routes")
