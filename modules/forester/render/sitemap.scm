;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render sitemap)
  #:use-module (grove i18n)
  #:use-module (grove render)
  #:use-module (grove private string-fun)
  #:use-module (forester private render-routes)
  #:use-module (forester private translations)
  #:use-module (forester private web)
  #:use-module (forester render context)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (render-sitemap
            make-sitemap-renderer))

(define (page-lastmod page)
  (let ((updated (assq-ref (site-page-metadata page) 'updated)))
    (when (and updated (not (iso-date? updated)))
      (error "Forester sitemap updated metadata must use YYYY-MM-DD"
             (site-page-id page) updated))
    updated))

(define (alternate-links base routes route-table groups default-locale page)
  (let ((group (page-translation-group page groups)))
    (if (or (not group) (< (length group) 2))
        '()
        (append
          (map (lambda (other)
                 `(xhtml:link (@ (rel "alternate")
                                 (hreflang ,(or (page-locale other) "und"))
                                 (href ,(web-absolute-route-url
                                          base
                                          (html-asset-url
                                            routes
                                            (cdr (assoc (site-page-id other)
                                                        route-table))))))))
               group)
          (let ((default (default-translation-page group default-locale)))
            (if default
                (list `(xhtml:link
                         (@ (rel "alternate")
                            (hreflang "x-default")
                            (href ,(web-absolute-route-url
                                     base
                                     (html-asset-url
                                       routes
                                       (cdr (assoc (site-page-id default)
                                                   route-table))))))))
                '()))))))

(define (page->sitemap-url base routes route-table groups default-locale page)
  (let ((lastmod (page-lastmod page))
        (url
          (web-absolute-route-url
            base
            (html-asset-url
              routes
              (cdr (assoc (site-page-id page) route-table))))))
    `(url
       "\n    " (loc ,url)
       ,@(if lastmod (list "\n    " `(lastmod ,lastmod)) '())
       ,@(map (lambda (link) (list "\n    " link))
              (alternate-links base routes route-table groups default-locale page))
       "\n  ")))

(define (sitemap-document base routes route-table groups default-locale pages)
  (call-with-output-string
    (lambda (port)
      (sxml->xml
        `(*TOP*
           (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
           "\n"
           (urlset (@ (xmlns "http://www.sitemaps.org/schemas/sitemap/0.9")
                      (xmlns:xhtml "http://www.w3.org/1999/xhtml"))
             ,@(append-map
                 (lambda (page)
                   (list "\n  "
                         (page->sitemap-url base routes route-table groups
                                            default-locale page)))
                 pages)
             "\n"))
        port))))

(define (normalize-sitemap-options settings site-url sitemap-path routes
                                   default-locale)
  (unless (or (not settings) (site-settings? settings))
    (error "Sitemap settings must be site settings or #f" settings))
  (set! site-url (or site-url
                     (and settings (site-settings-site-url settings))))
  (unless (web-origin? site-url)
    (error "Forester sitemap renderer requires an absolute HTTP(S) origin IRI"
           site-url))
  (unless (artifact-path? sitemap-path)
    (error "Sitemap path must be a relative artifact path" sitemap-path))
  (unless (html-routes? routes)
    (error "Forester sitemap renderer requires HTML routes" routes))
  (when default-locale
    (unless (non-empty-string? default-locale)
      (error "Sitemap default locale must be a non-empty tag or #f"
             default-locale))
    (set! default-locale (locale-tag (make-locale default-locale))))
  (values site-url sitemap-path routes default-locale))

(define* (render-sitemap forest #:key (settings #f)
                                (site-url
                                  (and (site-settings? settings)
                                       (site-settings-site-url settings)))
                                (sitemap-path "sitemap.xml")
                                (routes (make-html-routes))
                                (default-locale
                                  (and (site-settings? settings)
                                       (site-settings-default-locale settings))))
  "Render FOREST as one sitemap artifact.

SETTINGS supplies shared site URL and default locale values. Explicit keyword
values override them; ROUTES determine the absolute page URLs in the sitemap."
  (unless (forest? forest)
    (error "Forester sitemap renderer requires a forest" forest))
  (let-values (((site-url sitemap-path routes default-locale)
                (normalize-sitemap-options settings site-url sitemap-path
                                           routes default-locale)))
    (set! routes (validate-site-home-route
                   forest (resolve-site-routes forest routes)))
    (let* ((base (web-base-url (web-origin-uri site-url)))
           (route-table
             (html-route-table routes (map site-page-id (pages forest))))
           (groups (derive-translation-groups (pages forest)))
           (content (sitemap-document base routes route-table groups
                                      default-locale (pages forest))))
      (list (make-artifact sitemap-path content #:sources (source-paths forest))))))

(define* (make-sitemap-renderer #:key (settings #f) (site-url #f)
                                  (sitemap-path "sitemap.xml")
                                  (routes (make-html-routes))
                                  (default-locale
                                    (and (site-settings? settings)
                                         (site-settings-default-locale
                                           settings))))
  "Construct a cached sitemap renderer.

Provide SITE-URL directly or through SETTINGS; at least one must contain an
absolute HTTP(S) origin IRI. Put a deployment path prefix in ROUTES,
not SITE-URL. Explicit SITE-URL and DEFAULT-LOCALE values override the
corresponding SETTINGS defaults."
  (let-values (((site-url sitemap-path routes default-locale)
                (normalize-sitemap-options settings site-url sitemap-path
                                           routes default-locale)))
    (set! site-url (string-copy site-url))
    (set! sitemap-path (string-copy sitemap-path))
    (set! default-locale (and default-locale (string-copy default-locale)))
    (let* ((configuration
             (lambda (forest)
               (let ((routes (resolve-site-routes forest routes)))
                 (list 'sitemap sitemap-path site-url default-locale
                       (html-index-url routes)
                       (html-route-table routes
                                         (map site-page-id (pages forest))))))))
      (make-cached-renderer
        (lambda (forest)
          (render-sitemap forest #:site-url site-url
                          #:sitemap-path sitemap-path #:routes routes
                          #:default-locale default-locale))
        #:configuration configuration))))
