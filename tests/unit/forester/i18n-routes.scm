;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester i18n-routes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester input scribble)
  #:use-module (forester render html)
  #:use-module (forester render rss)
  #:use-module (forester render site)
  #:use-module (forester render sitemap)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (tests unit forester fixtures))

(test-begin "forester i18n routes")

(let* ((index-path (string-copy "home.html"))
       (home-page (string-copy "home"))
       (routes (make-html-routes #:index-path index-path
                                 #:home-page home-page)))
  (string-set! index-path 0 #\x)
  (string-set! home-page 0 #\x)
  (test-equal "HTML routes snapshot and protect string configuration"
    '("home.html" "home")
    (list (html-index-path routes) (html-home-page routes)))
  (let ((exposed-home (html-home-page routes)))
    (string-set! exposed-home 0 #\x))
  (test-equal "HTML route accessors return independent strings"
    "home" (html-home-page routes)))

(let* ((index-path (string-copy "index.txt"))
       (routes (make-text-routes #:index-path index-path)))
  (string-set! index-path 0 #\x)
  (test-equal "text routes snapshot and protect index paths"
    "index.txt" (text-index-path routes)))

(let* ((site-url (string-copy "https://example.org"))
       (locale (string-copy "en"))
       (settings (make-site-settings #:site-url site-url
                                     #:default-locale locale)))
  (string-set! site-url 0 #\x)
  (string-set! locale 0 #\x)
  (test-equal "site settings snapshot string configuration"
    '("https://example.org" "en")
    (list (site-settings-site-url settings)
          (site-settings-default-locale settings))))

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

(test-equal "the configured home page keeps index.html"
  "index.html" (html-page-path routes "home"))
(test-equal "a translated home page keeps its locale suffix"
  "home-zh.zh-Hans.html"
  (html-page-path
    (make-html-routes
      #:home-page "home"
      #:page-path (locale-page-path
                    (lambda (id) (and (equal? id "home-zh") "zh-Hans"))
                    #:default-locale "en"))
    "home-zh"))

(test-equal "locale page paths leave default locales unprefixed"
  "guide.html" (html-page-path routes "guide"))
(test-equal "locale page paths suffix other locales"
  "guide-zh.zh-Hans.html" (html-page-path routes "guide-zh"))
(test-equal "custom locale path builders keep directory layouts"
  "zh-Hans/guide-zh.html"
  (html-page-path
    (make-html-routes
      #:home-page "home"
      #:page-path (locale-page-path
                    locale-of
                    (lambda (locale id)
                      (if locale
                          (string-append locale "/" id ".html")
                          (string-append id ".html")))
                    #:default-locale "en"))
    "guide-zh"))

(let ((page (assoc-ref (site->html forest
                                   #:options
                                   (make-html-options
                                     #:routes routes
                                     #:mathjax-src #f
                                     #:default-locale "en"))
                       "guide")))
  (test-assert "translated pages emit hreflang alternates by default"
    (and (string-contains page
                          "rel=\"alternate\" hreflang=\"en\" href=\"/guide.html\"")
         (string-contains page
                          "hreflang=\"zh-Hans\" href=\"/guide-zh.zh-Hans.html\"")
         (string-contains page
                          "hreflang=\"x-default\" href=\"/guide.html\"")))
  (test-assert "Open Graph locales describe the page and its translations"
    (and (string-contains page "property=\"og:locale\" content=\"en_US\"")
         (string-contains page
                          "property=\"og:locale:alternate\" content=\"zh_CN\"")))
  (test-assert "language switcher is on by default and marks the current page"
    (and (string-contains
           page "<nav class=\"language-switcher\" aria-label=\"Language\">")
         (string-contains page "aria-current=\"true\""))))

(let ((page (assoc-ref (site->html forest
                                   #:options
                                   (make-html-options
                                     #:routes routes
                                     #:mathjax-src #f
                                     #:hreflang? #f
                                     #:language-switcher? #f))
                       "guide")))
  (test-assert "alternates can be disabled"
    (not (string-contains page "rel=\"alternate\"")))
  (test-assert "the language switcher can be disabled"
    (not (string-contains page "language-switcher"))))

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


(let* ((site-url "https://example.org")
       (html (assoc-ref (site->html forest
                                    #:options
                                    (make-html-options
                                      #:routes routes
                                      #:mathjax-src #f
                                      #:site-url site-url
                                      #:default-locale "en"))
                        "guide"))
       (sitemap (artifact-content
                  (car (render-sitemap forest #:site-url site-url
                                       #:routes routes
                                       #:default-locale "en")))))
  (test-assert "HTML canonical and sitemap locations agree"
    (and (string-contains
           html
           (string-append "<link rel=\"canonical\" href=\"" site-url
                          "/guide.html\""))
         (string-contains
           sitemap
           (string-append "<loc>" site-url "/guide.html</loc>")))))

(let* ((settings (make-site-settings #:site-url "https://example.org"
                                     #:default-locale "en"))
       (html (assoc-ref (site->html forest
                                    #:options
                                    (make-html-options
                                      #:settings settings
                                      #:routes routes
                                      #:mathjax-src #f))
                        "guide"))
       (sitemap (artifact-content
                  (car (render-sitemap forest #:settings settings
                                       #:routes routes))))
       (feed (artifact-content
               (car (render-rss forest #:settings settings
                                #:routes routes)))))
  (test-assert "site settings feed HTML, sitemap, and RSS"
    (and (string-contains html "https://example.org/guide.html")
         (string-contains sitemap "<loc>https://example.org/guide.html</loc>")
         (string-contains feed "<language>en</language>"))))

(let* ((settings (make-site-settings #:site-url "https://example.org"
                                     #:default-locale "en"))
       (html (site->html forest
                         #:options
                         (make-html-options
                           #:settings settings
                           #:locale "zh-Hans"
                           #:routes routes
                           #:mathjax-src #f)))
       (home (assoc-ref html "home"))
       (guide (assoc-ref html "guide")))
  (test-assert "page fallback locale and translation default are independent"
    (and (string-contains home "<html lang=\"zh-Hans\"")
         (string-contains guide
                          "hreflang=\"x-default\" href=\"/guide.html\""))))

(let* ((settings (make-site-settings #:site-url "https://example.org"
                                     #:default-locale "zh-Hans"))
       (html (assoc-ref (site->html forest
                                    #:options
                                    (make-html-options
                                      #:settings settings
                                      #:routes routes
                                      #:mathjax-src #f))
                       "home")))
  (test-assert "HTML settings default the page locale when renderer locale is omitted"
    (string-contains html "<html lang=\"zh-Hans\"")))

(test-error "site settings reject an empty URL" #t
  (make-site-settings #:site-url ""))
(test-error "site settings reject an empty locale" #t
  (make-site-settings #:default-locale ""))
(test-error "RSS rejects a non-settings value" #t
  (render-rss forest #:settings 'not-site-settings
              #:site-url "https://example.org"))
(test-error "sitemap rejects a non-settings value" #t
  (render-sitemap forest #:settings 'not-site-settings
                  #:site-url "https://example.org"))
(test-end "forester i18n routes")
