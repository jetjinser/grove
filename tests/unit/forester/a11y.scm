;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester a11y)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 optargs)
  #:use-module (forester backmatter)
  #:use-module (forester input scribble)
  #:use-module (forester private render labels)
  #:use-module (forester render html)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove render context)
  #:use-module (grove render html routes)
  #:use-module (grove render rules)
  #:use-module (grove render html presentation)
  #:use-module (grove site)
  #:use-module (tests unit forester fixtures))

(test-begin "forester accessibility")

(define (a11y-forest)
  (make-forest
    (make-site
      (list (make-site-page
              "root" "Root"
              (evaluate-document
                "@subtree{@title{Inline section}@paragraph{Body text.}}")
              #:metadata '((summary . "Body text.")
                           (published . "2026-09-27")
                           (updated . "2026-09-28")))))))

(define (render-site . options)
  (let ((routes (make-html-routes #:home-page "root")))
    (site->html (a11y-forest)
                #:options
                (apply make-html-options
                       #:routes routes #:mathjax-src #f options))))

(define (render-page . options)
  (assoc-ref (apply render-site options) "root"))

(let ((page (render-page)))
  (test-assert "document shell exposes a main landmark"
    (string-contains page "<main id=\"main\" tabindex=\"-1\">"))
  (test-assert "document shell starts with a skip link"
    (string-contains page
                     "<a class=\"skip-link\" href=\"#main\">Skip to content</a>"))
  (test-assert "site navigation carries its accessible name"
    (string-contains page "<nav aria-label=\"Site\">"))
  (test-assert "dates stay machine-readable in time elements"
    (string-contains page
                     "<time datetime=\"2026-09-27\">2026-09-27</time>"))
  (test-assert "documents carry the site name for Open Graph"
    (string-contains page "property=\"og:site_name\" content=\"Site\""))
  (test-assert "documents default to a language tag"
    (string-contains page "<html lang=\"en\""))
  (test-assert "documents carry a viewport meta tag"
    (string-contains page "<meta name=\"viewport\""))
  (test-assert "no Open Graph locale is invented without a declared locale"
    (not (string-contains page "property=\"og:locale\"")))
  (test-assert "documents carry Open Graph defaults"
    (and (string-contains page "property=\"og:title\"")
         (string-contains page "property=\"og:description\"")
         (string-contains page "property=\"og:type\" content=\"website\""))))

(let ((page (render-page #:article? #t)))
  (test-assert "article pages expose their publication time"
    (and (string-contains page "property=\"og:type\" content=\"article\"")
         (string-contains
           page
           "property=\"article:published_time\" content=\"2026-09-27\"")))
  (test-assert "article pages expose their modification time"
    (string-contains
      page
      "property=\"article:modified_time\" content=\"2026-09-28\"")))

(let ((page (render-page
              #:presentation
              (make-html-presentation
                  #:head '((meta (@ (name "viewport") (content "width=1024")))
                           (meta (@ (property "og:title") (content "Custom"))))))))
  (test-assert "project head entries override generated defaults"
    (and (string-contains page "content=\"width=1024\"")
         (not (string-contains page "width=device-width"))
         (string-contains page "property=\"og:title\" content=\"Custom\"")
         (not (string-contains page "property=\"og:title\" content=\"Root\"")))))

(let ((page (render-page #:locale "zh-Hans")))
  (test-assert "skip link localizes"
    (string-contains page "跳到正文"))
  (test-assert "navigation labels localize"
    (and (string-contains page "aria-label=\"站点导航\"")
         (string-contains page "aria-label=\"目录\"")))
  (test-assert "updated dates localize without losing the machine value"
    (string-contains page
                     "<time datetime=\"2026-09-28\">更新于 2026年9月28日</time>")))

(let ((page (render-page
              #:locale "zh-Hans"
              #:presentation
              (make-html-presentation
                  #:metadata-items
                  '(("root" . ((li (@ (class "meta-item")) "custom"))))))))
  (test-assert "declarative metadata keeps localized defaults"
    (string-contains page "更新于 2026年9月28日")))

(test-equal "backmatter headings localize"
  "参考文献"
  (assq-ref
    (localized-backmatter-labels
      (make-render-context #:locale "zh-Hans"
                           #:messages default-message-catalog))
    'references))

(let* ((routes (make-html-routes #:home-page "root"))
       (page (render-page
               #:presentation
               (make-html-presentation
                   #:site-nav (list (make-html-nav-link "root" "Home"))))))
  (test-assert "declarative site navigation marks the current page"
    (and (string-contains page "<nav aria-label=\"Site\">")
         (string-contains page "aria-current=\"page\""))))

(let ((page (render-page
              #:messages (make-message-catalog
                          '(("en" . ((nav-site . "Browse"))))
                          #:default "en"))))
  (test-assert "projects override interface messages"
    (string-contains page "<nav aria-label=\"Browse\">")))

(let* ((root (dirname (getenv "GROVE_SRCDIR")))
       (css (call-with-input-file
              (string-append root "/modules/forester/style/default.css")
              get-string-all)))
  (test-assert "default styles hide the skip link until focused"
    (and (string-contains css ".skip-link")
         (string-contains css ".skip-link:focus")))
  (test-assert "default styles keep focus visible"
    (string-contains css ":focus-visible")))

(let ((page (render-page #:locale "ar-EG")))
  (test-assert "locale derives the document language and direction"
    (string-contains page "<html lang=\"ar-EG\" dir=\"rtl\">")))

(let ((page (render-page #:locale "zh-Hans"
                         #:html-attributes '((lang . "zh-Hant")))))
  (test-assert "explicit html attributes override the locale"
    (and (string-contains page "lang=\"zh-Hant\"")
         (string-contains page "dir=\"ltr\""))))

(test-assert "text output localizes its interface labels"
  (let ((text (assoc-ref
                (site->text (a11y-forest)
                            #:options (make-text-options #:locale "zh-Hans"))
                "root")))
    (and (string-contains text "目录")
         (string-contains text "首页"))))

(let ((page (render-page #:site-url "https://example.org")))
  (test-assert "a site URL enables canonical and og:url"
    (and (string-contains
           page "<link rel=\"canonical\" href=\"https://example.org/index.html\"")
         (string-contains
           page "property=\"og:url\" content=\"https://example.org/index.html\""))))

(let ((page (render-page)))
  (test-assert "no canonical URL is invented without a site URL"
    (not (string-contains page "rel=\"canonical\""))))

(let* ((root (make-site-page
               "root" "Root"
               (evaluate-document "@paragraph{Body.}")
               #:metadata '((locale . "zh-Hans")
                            (published . "2026-09-27"))))
       (forest (make-forest (make-site (list root))))
       (routes (make-html-routes #:home-page "root"))
       (page (assoc-ref (site->html forest
                                    #:options
                                    (make-html-options
                                      #:routes routes
                                      #:mathjax-src #f))
                        "root")))
  (test-assert "page locale derives lang and dir without a renderer locale"
    (string-contains page "<html lang=\"zh-Hans\" dir=\"ltr\">"))
  (test-assert "page locale localizes interface labels"
    (and (string-contains page "跳到正文")
         (string-contains page "aria-label=\"站点导航\"")))
  (test-assert "page locale localizes dates"
    (string-contains page
                     "<time datetime=\"2026-09-27\">2026年9月27日</time>"))
  (test-assert "declared locales appear as Open Graph locales"
    (string-contains page "property=\"og:locale\" content=\"zh_CN\"")))

(test-error "renderer locales reject non-tags" #t
  (resolve-interface-locale 42 default-message-catalog))
(test-error "renderer catalogs reject non-catalogs" #t
  (resolve-interface-catalog 42))

(define* (render-page-with document #:key (site-url #f)
                           (og-image #f) (og-image-alt #f)
                           (presentation #f))
  (let* ((page (document->tree-page "root" (evaluate-document document) #f))
         (forest (make-forest (make-site (list page))))
         (routes (make-html-routes #:home-page "root")))
    (assoc-ref
      (site->html forest
                  #:options
                  (make-html-options
                    #:routes routes
                    #:mathjax-src #f
                    #:site-url site-url
                    #:og-image og-image
                    #:og-image-alt og-image-alt
                    #:presentation presentation))
      "root")))


(let* ((picture-rule
         (lambda (context element children next)
           `(picture
              (source (@ (type "image/avif")
                         (srcset "/forest.avif")))
              (img (@ (src ,(content-element-property element 'source))
                      (alt ,(content-verbatim-string children)))))))
       (rules (make-render-rules (list (cons 'image picture-rule))))
       (page (render-page-with
               "@title{Root}@image{forest.jpg}{A forest}@paragraph{Body.}"
               #:presentation (make-html-presentation #:rules rules))))
  (test-assert "site->html accepts renderer rules"
    (string-contains page "<picture><source type=\"image/avif\"")))
(let ((page (render-page-with
              "@title{Root}@og-image{assets/cover.png}@paragraph{Body.}"
              #:site-url "https://example.org")))
  (test-assert "og:image resolves against the site URL"
    (string-contains
      page
      "property=\"og:image\" content=\"https://example.org/assets/cover.png\""))
  (test-assert "og:image:alt defaults to the page title"
    (string-contains
      page
      "property=\"og:image:alt\" content=\"Root\""))
  (test-assert "og:image metadata leaves the body"
    (not (string-contains page "<p>assets/cover.png</p>"))))


(let ((page (render-page-with
              "@title{Root}@og-image{https://cdn.example.org/cover.png}@paragraph{Body.}")))
  (test-assert "external og:image is used as-is"
    (string-contains
      page
      "property=\"og:image\" content=\"https://cdn.example.org/cover.png\"")))


(let ((page (render-page-with
              "@title{Root}@og-image{assets/cover.png}@paragraph{Body.}")))
  (test-assert "relative og:image is omitted without a site URL"
    (not (string-contains page "property=\"og:image\""))))


(let ((page (render-page-with
              "@title{Root}@paragraph{Body.}"
              #:site-url "https://example.org"
              #:og-image "assets/site.png"
              #:og-image-alt "Site cover")))
  (test-assert "site-level og:image is a fallback"
    (string-contains
      page
      "property=\"og:image\" content=\"https://example.org/assets/site.png\""))
  (test-assert "site-level og:image alt is used"
    (string-contains
      page
      "property=\"og:image:alt\" content=\"Site cover\"")))


(let ((page (render-page-with
              "@title{Root}@og-image{assets/page.png}@og-image-alt{Page cover}@paragraph{Body.}"
              #:site-url "https://example.org"
              #:og-image "assets/site.png"
              #:og-image-alt "Site cover")))
  (test-assert "page og:image overrides the site default"
    (string-contains
      page
      "property=\"og:image\" content=\"https://example.org/assets/page.png\""))
  (test-assert "page og:image alt overrides the site default"
    (string-contains
      page
      "property=\"og:image:alt\" content=\"Page cover\""))
  (test-assert "the site image is not emitted for a page image"
    (not (string-contains page "assets/site.png"))))


(let ((page (render-page-with
              "@title{Root}@og-image{assets/page.png}@paragraph{Body.}"
              #:site-url "https://example.org"
              #:og-image "assets/site.png"
              #:og-image-alt "Site cover")))
  (test-assert "a page image alt falls back to the title"
    (string-contains
      page
      "property=\"og:image:alt\" content=\"Root\"")))

(let* ((picture-rule
         (lambda (context element children next)
           `(picture
              (source (@ (type "image/avif")
                         (srcset "/forest.avif")))
              (img (@ (src ,(content-element-property element 'source))
                      (alt ,(content-verbatim-string children)))))))
       (rules (make-render-rules (list (cons 'image picture-rule))))
       (presentation (make-html-presentation #:rules rules))
       (page (render-page-with
               "@title{Root}@image{forest.jpg}{A forest}@paragraph{Body.}"
               #:presentation presentation)))
  (test-assert "site->html accepts an HTML presentation"
    (string-contains page "<picture><source type=\"image/avif\"")))
(test-end "forester accessibility")
