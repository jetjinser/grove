;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester a11y)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 textual-ports)
  #:use-module (forester backmatter)
  #:use-module (forester render html)
  #:use-module (forester site)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (tests unit forester fixtures))

(test-begin "forester accessibility")
(define (render-site . options)
  (let* ((root (make-site-page
                 "root" "Root"
                 (evaluate-document
                   "@subtree{@title{Inline section}@paragraph{Body text.}}")
                 #:metadata '((published . "2026-09-27")
                              (updated . "2026-09-28"))))
         (forest (make-forest (make-site (list root))))
         (routes (make-html-routes #:home-page "root")))
    (apply site->html forest #:routes routes #:mathjax-src #f options)))

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
                     "<time datetime=\"2026-09-27\">2026-09-27</time>")))

(let ((page (render-page #:locale "zh-Hans")))
  (test-assert "skip link localizes"
    (string-contains page "跳到正文"))
  (test-assert "navigation labels localize"
    (and (string-contains page "aria-label=\"站点导航\"")
         (string-contains page "aria-label=\"目录\"")))
  (test-assert "updated dates localize without losing the machine value"
    (string-contains page
                     "<time datetime=\"2026-09-28\">更新于 2026-09-28</time>")))

(test-equal "backmatter headings localize"
  "参考文献"
  (assq-ref (localized-backmatter-labels default-message-catalog "zh-Hans")
            'references))

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

(test-end "forester accessibility")
