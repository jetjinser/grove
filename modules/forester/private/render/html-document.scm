;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render html-document)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove i18n)
  #:use-module (grove private string-fun)
  #:use-module (grove render context)
  #:use-module (grove render html)
  #:use-module (grove render html attributes)
  #:use-module (grove render html routes)
  #:use-module (grove style html)
  #:use-module (forester private render labels)
  #:use-module (forester private web)
  #:use-module (srfi srfi-13)
  #:export (html-document-sxml
            serialize-html-document))

;; Page content is assembled by the Forester renderer. This module owns the
;; document-level shell: metadata, navigation, stylesheets, scripts and the
;; final HTML serialization.
(define og-default-territories
  '(("en" . "US") ("fr" . "FR") ("de" . "DE") ("es" . "ES")
    ("it" . "IT") ("pt" . "PT") ("nl" . "NL") ("sv" . "SE")
    ("da" . "DK") ("nb" . "NO") ("fi" . "FI") ("pl" . "PL")
    ("cs" . "CZ") ("tr" . "TR") ("ru" . "RU") ("el" . "GR")
    ("ja" . "JP") ("ko" . "KR") ("zh" . "CN")))

(define (og-region? part)
  (or (and (= (string-length part) 2)
           (string-every ascii-alpha? part))
      (and (= (string-length part) 3)
           (string-every ascii-digit? part))))

(define (og-script? part)
  (and (= (string-length part) 4)
       (string-every ascii-alpha? part)))

(define (open-graph-locale-tag locale)
  "Return the Open Graph language_TERRITORY form for LOCALE."
  (let* ((tag (locale-tag (if (locale? locale) locale (make-locale locale))))
         (parts (string-split tag #\-))
         (language (car parts))
         (subtags (cdr parts)))
    (let scan ((subtags subtags) (script #f) (region #f))
      (cond
        ((or (null? subtags) (= (string-length (car subtags)) 1))
         (let ((territory
                 (or region
                     (and (string=? language "zh")
                          (if (equal? script "Hant") "TW" "CN"))
                     (assoc-ref og-default-territories language))))
           (if territory
               (string-append language "_" territory)
               language)))
        ((og-region? (car subtags))
         (scan (cdr subtags) script (car subtags)))
        ((and (not script) (og-script? (car subtags)))
         (scan (cdr subtags) (car subtags) region))
        (else (scan (cdr subtags) script region))))))

(define (mathjax-script-url source routes)
  (and source
       (if (external-url? source)
           source
           (html-asset-url routes source))))

(define fragment-reveal-script
  "(function () {\n  function revealFragment() {\n    if (!location.hash) return;\n    var id = location.hash.slice(1);\n    try { id = decodeURIComponent(id); } catch (error) {}\n    var target = document.getElementById(id);\n    if (!target) return;\n    var node = target;\n    while (node) {\n      if (node.tagName === 'DETAILS') node.open = true;\n      node = node.parentElement;\n    }\n    window.requestAnimationFrame(function () { target.scrollIntoView(); });\n  }\n  window.addEventListener('hashchange', revealFragment);\n  revealFragment();\n}());")

(define (head-attribute node attribute)
  (let ((attrs (and (pair? node) (pair? (cdr node))
                    (eq? (caadr node) '@)
                    (cdadr node))))
    (and attrs
         (let ((entry (assq attribute attrs)))
           (and entry (cadr entry))))))

(define (head-key node)
  (let ((tag (and (pair? node) (car node))))
    (cond
      ((eq? tag 'meta)
       (cond ((head-attribute node 'name)
              => (lambda (value) (cons 'meta-name value)))
             ((head-attribute node 'property)
              => (lambda (value) (cons 'meta-property value)))
             (else #f)))
      ((eq? tag 'link)
       (let ((rel (head-attribute node 'rel))
             (hreflang (head-attribute node 'hreflang)))
         (cond ((and rel hreflang (string=? rel "alternate"))
                (cons 'link-alternate hreflang))
               (rel (cons 'link-rel rel))
               (else #f))))
      ((eq? tag 'title) 'title)
      (else #f))))

(define (head-keys nodes)
  (let loop ((rest nodes) (keys '()))
    (if (null? rest)
        (reverse keys)
        (let ((key (head-key (car rest))))
          (loop (cdr rest) (if key (cons key keys) keys))))))

(define (merge-head defaults overrides)
  "Append OVERRIDES to DEFAULTS, dropping defaults they replace by
name, property, rel, or title."
  (let ((keys (head-keys overrides)))
    (append (filter (lambda (node)
                      (let ((key (head-key node)))
                        (or (not key) (not (member key keys)))))
                    defaults)
            overrides)))

(define* (html-document-sxml title description body math? home-title style
                       routes mathjax-src inline-stylesheets?
                       #:key (html-attributes '()) (head '()) (site-nav '())
                       (home-link #f) (scripts '()) (current-page #f)
                       (language-nav '()) (article? #f) (published #f)
                       (og-locales '()) (updated #f) (og-image #f)
                       (og-image-alt #f) (context #f))
  (unless (render-context? context)
    (error "html-document requires a render context" context))
  (let* ((site-nav site-nav)
         (locale (render-context-ref context 'locale-object))
         (default-locale
           (render-context-ref context 'default-locale))
         (html-attributes
           (cond
             (locale
              (merge-html-attributes (locale-html-attributes locale)
                                     html-attributes))
             (default-locale
              (merge-html-attributes
                (locale-html-attributes (make-locale default-locale))
                html-attributes))
             (else
              (merge-html-attributes '((lang . "en")) html-attributes))))
         (site-url (render-context-ref context 'site-url))
         (page-url (and site-url current-page
                        (html-page-url routes current-page)))
         (absolute-url (and page-url
                            (string-append (string-trim-right site-url #\/)
                                           page-url)))
         (og-image-url
           (and og-image
                (cond ((or (string-prefix-ci? "http://" og-image)
                           (string-prefix-ci? "https://" og-image))
                       og-image)
                      ((and site-url (string-prefix? "/" og-image))
                       (string-append (string-trim-right site-url #\/)
                                      og-image))
                      (site-url
                       (string-append (string-trim-right site-url #\/)
                                      (html-asset-url routes og-image)))
                      (else #f)))))
    `(html
     ,@(if (null? html-attributes)
           '()
           (list (cons '@ (map (lambda (attribute)
                                 (list (car attribute) (cdr attribute)))
                               html-attributes))))
     (head
       ,@(merge-head
           (append
             (list `(meta (@ (charset "utf-8")))
                   `(meta (@ (name "viewport")
                             (content "width=device-width, initial-scale=1")))
                   `(title ,title)
                   `(meta (@ (property "og:title") (content ,title)))
                   `(meta (@ (property "og:type")
                             (content ,(if article? "article" "website")))))
             (if (and home-title (not (string-null? home-title)))
                 (list `(meta (@ (property "og:site_name")
                                 (content ,home-title))))
                 '())
             (if description
                 (list `(meta (@ (name "description") (content ,description)))
                       `(meta (@ (property "og:description")
                                 (content ,description))))
                 '())
             (if locale
                 (list `(meta (@ (property "og:locale")
                                  (content ,(open-graph-locale-tag
                                               (render-context-ref context 'locale))))))
                 '())
             (map (lambda (tag)
                    `(meta (@ (property "og:locale:alternate")
                              (content ,(open-graph-locale-tag tag)))))
                  og-locales)
             (if og-image-url
                 (append
                   (list `(meta (@ (property "og:image")
                                   (content ,og-image-url))))
                   (if og-image-alt
                       (list `(meta (@ (property "og:image:alt")
                                       (content ,og-image-alt))))
                       '()))
                 '())
             (if absolute-url
                 (list `(link (@ (rel "canonical") (href ,absolute-url)))
                       `(meta (@ (property "og:url") (content ,absolute-url))))
                 '())
             (if (and article? published)
                 (list `(meta (@ (property "article:published_time")
                                 (content ,published))))
                 '())
             (if (and article? updated)
                 (list `(meta (@ (property "article:modified_time")
                                 (content ,updated))))
                 '()))
           head)
       ,@(if inline-stylesheets?
             (map (lambda (stylesheet)
                    `(style ,(out:artifact-content/raw stylesheet)))
                  (html-style-stylesheets style))
             (map (lambda (stylesheet)
                    `(link (@ (rel "stylesheet")
                              (href ,(html-asset-url
                                       routes (out:artifact-path/raw stylesheet))))))
                  (html-style-stylesheets style)))
       ,@(if (and math? mathjax-src)
             `((script (@ (src ,(mathjax-script-url mathjax-src routes))
                         (defer "defer"))
                       " "))
             '()))
     (body
       (a (@ (class "skip-link") (href "#main"))
          ,(interface-message 'skip-to-content context))
       (header
         ,@(if (or home-title home-link (pair? site-nav) (pair? language-nav))
               (list `(nav (@ (aria-label ,(interface-message 'nav-site context)))
                           ,@(cond
                               (home-link (list home-link))
                               (home-title (list `(a (@ (href ,(html-index-url routes)))
                                                    ,home-title)))
                               (else '()))
                           ,@site-nav)
                     language-nav)
               '()))
       (main (@ (id "main") (tabindex "-1"))
         ,@body)
       ,@(map (lambda (script) `(script ,script)) scripts)
       (script ,fragment-reveal-script)))))

(define (serialize-html-document sxml)
  (string-append "<!DOCTYPE html>\n" (sxml->html sxml)))
