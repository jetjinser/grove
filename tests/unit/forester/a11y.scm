;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester a11y)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 textual-ports)
  #:use-module (forester backmatter)
  #:use-module (forester render labels)
  #:use-module (forester render html)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove render html routes)
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
    (apply site->html (a11y-forest)
           #:routes routes #:mathjax-src #f options)))

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
              #:head '((meta (@ (name "viewport") (content "width=1024")))
                       (meta (@ (property "og:title") (content "Custom")))))))
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

(test-equal "backmatter headings localize"
  "参考文献"
  (with-chrome-locale ("zh-Hans" default-message-catalog)
    (assq-ref (localized-backmatter-labels) 'references)))

(let ((routes (make-html-routes #:home-page "root")))
  (test-assert "site navigation links mark the current page"
    (member '(aria-current "page")
            (cadr (site-nav-link routes "root" "Home" #:current-page "root"))))
  (test-assert "site navigation links omit aria-current elsewhere"
    (not (member '(aria-current "page")
                 (cadr (site-nav-link routes "root" "Home"
                                      #:current-page "other"))))))

(let* ((routes (make-html-routes #:home-page "root"))
       (page (render-page
               #:site-nav (lambda (current)
                            (list (site-nav-link routes "root" "Home"
                                                 #:current-page current))))))
  (test-assert "site navigation procedures receive the current page"
    (and (string-contains page "<nav aria-label=\"Site\">")
         (string-contains page "aria-current=\"page\""))))

(let ((page (render-page
              #:messages (make-message-catalog
                          '(("en" . ((nav-site . "Browse"))))
                          #:default "en"))))
  (test-assert "projects override chrome messages"
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

(test-assert "text output localizes its chrome"
  (let ((text (assoc-ref (site->text (a11y-forest) #:locale "zh-Hans") "root")))
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
       (page (assoc-ref (site->html forest #:routes routes #:mathjax-src #f)
                        "root")))
  (test-assert "page locale derives lang and dir without a renderer locale"
    (string-contains page "<html lang=\"zh-Hans\" dir=\"ltr\">"))
  (test-assert "page locale localizes chrome"
    (and (string-contains page "跳到正文")
         (string-contains page "aria-label=\"站点导航\"")))
  (test-assert "page locale localizes dates"
    (string-contains page
                     "<time datetime=\"2026-09-27\">2026年9月27日</time>"))
  (test-assert "declared locales appear as Open Graph locales"
    (string-contains page "property=\"og:locale\" content=\"zh_CN\"")))

(let ((routes (make-html-routes #:home-page "root")))
  (test-assert "site-nav builds stable current-page markup"
    (member '(aria-current "page")
            (cadr (car (site-nav routes '(("root" . "Home"))
                                 #:current-page "root"))))))

(test-error "renderer locales reject non-tags" #t
  (resolve-chrome-locale 42 default-message-catalog))
(test-error "renderer catalogs reject non-catalogs" #t
  (resolve-chrome-catalog 42))
(test-end "forester accessibility")
