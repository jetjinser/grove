;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester output-renderers)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-13)
  #:use-module (forester render graph)
  #:use-module (forester render context)
  #:use-module (forester render gemtext)
  #:use-module (forester render rss)
  #:use-module (forester render site)
  #:use-module (forester render sitemap)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (grove private renderer)
  #:use-module (grove render)
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

(let* ((root (string-append "/tmp/forester-idna-cache-test-"
                            (number->string (getpid))))
       (bin (string-append root "/bin"))
       (idn2 (string-append bin "/idn2"))
       (old-path (getenv "PATH"))
       (renderers
        (list (make-gemtext-renderer)
              (make-rss-renderer #:site-url "https://example.org")
              (make-sitemap-renderer #:site-url "https://example.org")))
       (before #f)
       (after #f))
  (define (write-idn2 version)
    (call-with-output-file idn2
      (lambda (port)
        (format port "#!/bin/sh\necho 'idn2 ~a'\n" version)))
    (chmod idn2 #o755))
  (mkdir root)
  (mkdir bin)
  (dynamic-wind
    (lambda ()
      (setenv "PATH" (string-append bin ":" (or old-path ""))))
    (lambda ()
      (write-idn2 "test-version-1")
      (set! before (renderer-configurations forest renderers))
      (write-idn2 "test-version-2")
      (set! after (renderer-configurations forest renderers))
      (test-assert "IDNA implementation changes invalidate web renderer identities"
        (not (equal? before after))))
    (lambda ()
      (if old-path (setenv "PATH" old-path) (unsetenv "PATH"))
      (system* "rm" "-rf" root))))

(test-equal "translation groups use association-list data"
  (list (cons "guide" (list (car (pages forest)) (cadr (pages forest)))))
  (derive-translation-groups (pages forest)))

(let ((routes (make-gemtext-routes #:index-path "capsule/index.gmi")))
  (test-equal "Gemtext routes keep their page suffix when only the index changes"
    (list "capsule/index.gmi" "guide.gmi")
    (list (text-index-path routes) (text-page-path routes "guide"))))

(let* ((settings (make-site-settings
                   #:site-url "https://example.org"
                   #:default-locale "en"))
       (routes (make-html-routes))
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
           (string-contains content "https://example.org/index.html")
           (string-contains content "hreflang=\"x-default\""))))
  (test-assert "RSS renderer uses shared site settings"
    (let ((content (artifact-content (cadr artifacts))))
      (and (string-contains content "<language>en</language>")
           (string-contains content
                            "<link>https://example.org/index.html</link>")))))

(let* ((origin "https://例え.テスト")
       (routes (make-html-routes #:index-path "首页.html"
                                 #:url-prefix "/出版/"))
       (settings (make-site-settings #:site-url origin))
       (artifacts
        (render-model forest
          (list (make-sitemap-renderer #:settings settings #:routes routes)
                (make-rss-renderer #:settings settings #:routes routes))))
       (sitemap (artifact-content (car artifacts)))
       (rss (artifact-content (cadr artifacts))))
  (test-equal "site settings preserve the authored origin IRI"
    origin (site-settings-site-url settings))
  (test-assert "sitemap maps a Unicode origin host to IDNA ASCII"
    (and (string-contains sitemap
                          "https://xn--r8jz45g.xn--zckzah/%E5%87%BA%E7%89%88/guide-zh.html")
         (not (string-contains sitemap "例え.テスト"))
         (not (string-contains sitemap "出版"))))
  (test-assert "RSS maps a Unicode origin host to IDNA ASCII"
    (and (string-contains rss
                          "https://xn--r8jz45g.xn--zckzah/%E5%87%BA%E7%89%88/%E9%A6%96%E9%A1%B5.html")
         (not (string-contains rss "例え.テスト"))
         (not (string-contains rss "出版")))))

(let ((forest-without-home
        (make-forest
          (make-site (site-pages (site forest)) #:title "Test"))))
  (test-error "RSS renderer requires a site home page"
    #t
    (render-rss forest-without-home #:site-url "https://example.org"))
  (test-assert "sitemap renders actual pages without requiring a home"
    (string-contains
      (artifact-content
        (car (render-sitemap forest-without-home
                             #:site-url "https://example.org")))
      "https://example.org/guide-en.html")))

(let* ((artifacts
        (render-model forest
          (list (make-sitemap-renderer #:site-url "HTTPS://example.org")
                (make-rss-renderer #:site-url "HTTPS://example.org")))))
  (test-assert "site renderer URL schemes are case-insensitive"
    (and (string-contains (artifact-content (car artifacts))
                          "HTTPS://example.org/index.html")
         (string-contains (artifact-content (cadr artifacts))
                          "HTTPS://example.org/index.html"))))

(let* ((routes (make-html-routes #:url-prefix "/notes/"))
       (artifacts
        (render-model forest
          (list (make-sitemap-renderer #:site-url "https://example.org"
                                       #:routes routes)
                (make-rss-renderer #:site-url "https://example.org"
                                   #:routes routes)))))
  (test-assert "site renderer URLs use the shared route prefix"
    (and (string-contains (artifact-content (car artifacts))
                          "https://example.org/notes/index.html")
         (string-contains (artifact-content (cadr artifacts))
                          "https://example.org/notes/index.html"))))

(let* ((settings (make-site-settings #:site-url "https://example.org"))
       (routes (make-html-routes
                 #:home-page "guide-zh"
                 #:index-path "welcome.html"
                 #:page-path (lambda (id)
                               (string-append "pages/" id ".html"))))
       (artifacts
        (render-model
          forest
          (list (make-rss-renderer #:settings settings #:routes routes)
                (make-sitemap-renderer #:settings settings #:routes routes)))))
  (test-assert "explicit shared routes override the site's inferred home"
    (and (string-contains (artifact-content (car artifacts))
                          "https://example.org/welcome.html")
         (string-contains (artifact-content (cadr artifacts))
                          "https://example.org/welcome.html")
         (string-contains (artifact-content (cadr artifacts))
                          "https://example.org/pages/guide-en.html"))))

(test-error "sitemap renderer rejects invalid settings at construction" #t
  (make-sitemap-renderer #:settings 'not-site-settings))
(test-error "RSS renderer rejects invalid settings at construction" #t
  (make-rss-renderer #:settings 'not-site-settings))
(test-error "sitemap rendering rejects invalid settings" #t
  (render-sitemap forest #:settings 'not-site-settings
                  #:site-url "https://example.org"))
(test-error "RSS rendering rejects invalid settings" #t
  (render-rss forest #:settings 'not-site-settings
              #:site-url "https://example.org"))
(test-error "sitemap renderer rejects empty default locales at construction" #t
  (make-sitemap-renderer #:site-url "https://example.org"
                         #:default-locale ""))
(test-error "RSS renderer rejects empty language tags at construction" #t
  (make-rss-renderer #:site-url "https://example.org" #:language ""))
(test-error "sitemap rendering rejects empty default locales" #t
  (render-sitemap forest #:site-url "https://example.org"
                  #:default-locale ""))
(test-error "RSS rendering rejects empty language tags" #t
  (render-rss forest #:site-url "https://example.org" #:language ""))
(test-error "sitemap renderer requires a site URL at construction" #t
  (make-sitemap-renderer))
(test-error "RSS renderer requires a site URL at construction" #t
  (make-rss-renderer))
(test-error "sitemap renderer rejects an HTTP URL without a host" #t
  (make-sitemap-renderer #:site-url "https:///path"))
(test-error "shared site settings reject an HTTP URL without a host" #t
  (make-site-settings #:site-url "https:///path"))
(test-error "shared site settings reject a URL query" #t
  (make-site-settings #:site-url "https://example.org/?preview=1"))
(test-error "shared site settings reject a deployment path" #t
  (make-site-settings #:site-url "https://example.org/notes/"))
(test-error "shared site settings reject origin credentials" #t
  (make-site-settings #:site-url "https://writer:secret@example.org"))
(test-error "RSS renderer rejects a site URL with a query" #t
  (make-rss-renderer #:site-url "https://example.org/?preview=1"))
(test-error "RSS renderer rejects a deployment path in its origin" #t
  (make-rss-renderer #:site-url "https://example.org/notes/"))
(test-error "sitemap renderer rejects a site URL with a fragment" #t
  (make-sitemap-renderer #:site-url "https://example.org/#section"))
(test-error "sitemap renderer validates its artifact path at construction" #t
  (make-sitemap-renderer #:site-url "https://example.org"
                         #:sitemap-path "../sitemap.xml"))
(test-error "RSS renderer validates its artifact path at construction" #t
  (make-rss-renderer #:site-url "https://example.org"
                     #:feed-path "../feed.xml"))
(test-error "RSS renderer rejects a configured home page absent from the forest" #t
  (render-rss forest
    #:site-url "https://example.org"
    #:routes (make-html-routes #:home-page "missing")))
(test-error "sitemap renderer rejects a configured home page absent from the forest" #t
  (render-sitemap forest
    #:site-url "https://example.org"
    #:routes (make-html-routes #:home-page "missing")))
(let* ((site-url (string-copy "https://example.org"))
       (sitemap-path (string-copy "map.xml"))
       (feed-path (string-copy "feed.xml"))
       (stylesheet-url (string-copy "https://example.org/feed.xsl"))
       (sitemap (make-sitemap-renderer
                  #:site-url site-url #:sitemap-path sitemap-path))
       (rss (make-rss-renderer
              #:site-url site-url #:feed-path feed-path
              #:stylesheet-url stylesheet-url)))
  (string-set! site-url 0 #\x)
  (string-set! sitemap-path 0 #\x)
  (string-set! feed-path 0 #\x)
  (string-set! stylesheet-url 0 #\x)
  (let ((artifacts (render-model forest (list sitemap rss))))
    (test-equal "site renderers snapshot URL and artifact-path options"
      '("map.xml" "feed.xml")
      (map artifact-path artifacts))
    (test-assert "RSS renderer snapshots stylesheet URL options"
      (string-contains (artifact-content (cadr artifacts))
                       "https://example.org/feed.xsl"))))

(test-end "Forester output renderers")
