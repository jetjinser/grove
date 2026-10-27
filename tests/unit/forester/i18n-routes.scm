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

(let* ((id (string-copy "guide"))
       (shared-path (string-copy "pages/guide.html"))
       (routes (make-html-routes
                 #:page-path
                 (lambda (argument)
                   (string-set! argument 0 #\X)
                   shared-path)))
       (returned (html-page-path routes id)))
  (string-set! returned 0 #\X)
  (test-equal "HTML path callbacks cannot mutate the caller's page ID"
    "guide" id)
  (test-equal "HTML route results do not expose callback-owned strings"
    "pages/guide.html" (html-page-path routes "guide"))
  (let* ((ids (list id))
         (table (html-route-table routes ids)))
    (string-set! (caar table) 0 #\X)
    (test-equal "HTML route tables own their page IDs" "guide" (car ids))))

(let* ((id (string-copy "guide"))
       (shared-path (string-copy "pages/guide.txt"))
       (routes (make-text-routes
                 #:page-path
                 (lambda (argument)
                   (string-set! argument 0 #\X)
                   shared-path)))
       (returned (text-page-path routes id)))
  (string-set! returned 0 #\X)
  (test-equal "text path callbacks cannot mutate the caller's page ID"
    "guide" id)
  (test-equal "text route results do not expose callback-owned strings"
    "pages/guide.txt" (text-page-path routes "guide")))

(let* ((default-locale (string-copy "en"))
       (paths (locale-page-path (lambda (_) #f)
                                #:default-locale default-locale)))
  (string-set! default-locale 0 #\X)
  (test-equal "locale routes snapshot their configured default locale"
    "guide.html" (paths "guide")))

(let* ((id (string-copy "guide"))
       (shared-locale (string-copy "zh-Hans"))
       (shared-path (string-copy "guide.zh-Hans.html"))
       (paths
         (locale-page-path
           (lambda (argument)
             (string-set! argument 0 #\X)
             shared-locale)
           (lambda (locale argument)
             (string-set! locale 0 #\X)
             (string-set! argument 0 #\X)
             shared-path)))
       (returned (paths id)))
  (string-set! returned 0 #\X)
  (test-equal "locale callbacks cannot mutate the caller's page ID" "guide" id)
  (test-equal "locale callbacks cannot alter later paths"
    "guide.zh-Hans.html" (paths "guide")))

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

(test-equal "site settings canonicalize locale aliases"
  "zh-Hans"
  (site-settings-default-locale
    (make-site-settings #:default-locale "zh-CN")))

(define (make-page id locale translation)
  (make-site-page
    id (string-append "Page " id)
    (evaluate-document "@paragraph{Body.}")
    #:metadata (append (if locale (list (cons 'locale locale)) '())
                       (if translation (list (cons 'translation translation)) '()))))

(define home (make-page "home" #f #f))
(define guide-en (make-page "guide" "en" "guide"))
(define guide-zh (make-page "guide-zh" "zh-CN" "guide"))
(define forest (make-forest (make-site (list home guide-en guide-zh))))

(test-equal "forest construction canonicalizes programmatic page locales"
  "zh-Hans"
  (assq-ref (site-page-metadata (require-page forest "guide-zh")) 'locale))
(test-equal "the Forest's exposed site shares canonical page metadata"
  "zh-Hans"
  (assq-ref (site-page-metadata
              (find (lambda (page)
                      (string=? "guide-zh" (site-page-id page)))
                    (site-pages (site forest))))
            'locale))

(define (locale-of id)
  (let ((page (require-page forest id)))
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
(test-equal "locale routes canonicalize aliases before comparing defaults"
  "guide.html"
  (html-page-path
    (make-html-routes
      #:page-path (locale-page-path (lambda (_) "zh-CN")
                                    #:default-locale "zh-Hans"))
    "guide"))
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
                                      #:default-locale "zh-CN")))))
  (test-assert "sitemap carries hreflang alternates"
    (and (string-contains sitemap "xhtml:link")
         (string-contains sitemap "hreflang=\"zh-Hans\"")
         (string-contains sitemap "hreflang=\"x-default\"")
         (string-contains sitemap
           "hreflang=\"x-default\" href=\"https://example.org/guide-zh.zh-Hans.html\""))))

(let ((page (document->tree-page
              "x"
              (evaluate-document
                "@title{X}\n@locale[\"zh-CN\"]\n@translation[\"intro\"]\n@paragraph{Body.}")
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
                                     #:default-locale "EN-us"))
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
         (string-contains feed "<language>en-US</language>"))))

(let* ((settings (make-site-settings #:site-url "https://example.org"
                                     #:default-locale "zh-CN"))
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
                          "hreflang=\"x-default\" href=\"/guide-zh.zh-Hans.html\""))))

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
(test-error "site settings reject malformed locale tags" #t
  (make-site-settings #:default-locale "not a locale"))
(test-error "RSS rejects a non-settings value" #t
  (render-rss forest #:settings 'not-site-settings
              #:site-url "https://example.org"))
(test-error "sitemap rejects a non-settings value" #t
  (render-sitemap forest #:settings 'not-site-settings
                  #:site-url "https://example.org"))
(test-end "forester i18n routes")
