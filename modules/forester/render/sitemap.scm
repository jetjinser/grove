;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render sitemap)
  #:use-module (grove build)
  #:use-module (forester private date)
  #:use-module (forester private web)
  #:use-module (forester render context)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:export (render-sitemap
            make-sitemap-renderer))

(define (page-lastmod page)
  (let ((updated (assq-ref (site-page-metadata page) 'updated)))
    (when (and updated (not (forester-date? updated)))
      (error "Forester sitemap updated metadata must use YYYY-MM-DD"
             (site-page-id page) updated))
    updated))

(define (page-locale page)
  (assq-ref (site-page-metadata page) 'locale))

(define (alternate-links base routes route-table groups default-locale page)
  (let* ((key (assq-ref (site-page-metadata page) 'translation))
         (group (and key (reverse (hash-ref groups key)))))
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
          (let ((default (and default-locale
                              (find (lambda (other)
                                      (equal? (page-locale other) default-locale))
                                    group))))
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

(define* (render-sitemap forest #:key (settings #f)
                                (site-url
                                  (and (site-settings? settings)
                                       (site-settings-site-url settings)))
                                (sitemap-path "sitemap.xml")
                                (routes (make-html-routes #:home-page "index"))
                                (default-locale
                                  (and (site-settings? settings)
                                       (site-settings-default-locale settings))))
  (unless (forest? forest)
    (error "Forester sitemap renderer requires a forest" forest))
  (unless (or (not settings) (site-settings? settings))
    (error "Sitemap settings must be site settings or #f" settings))
  (unless (web-url? site-url)
    (error "Forester sitemap renderer requires an absolute HTTP(S) site URL"
           site-url))
  (unless (artifact-path? sitemap-path)
    (error "Sitemap path must be a relative artifact path" sitemap-path))
  (unless (html-routes? routes)
    (error "Forester sitemap renderer requires HTML routes" routes))
  ;;     home-page 是可选的：没有主页的项目不需要校验它存在。
  (unless (or (not (html-home-page routes))
              (find-page forest (html-home-page routes)))
    (error "configured HTML home page was not found" (html-home-page routes)))
  (let* ((base (web-base-url site-url))
         (route-table
           (html-route-table routes (map site-page-id (pages forest))))
         (groups (translation-groups (pages forest)))
         (content (sitemap-document base routes route-table groups
                                    default-locale (pages forest))))
    (list (make-artifact sitemap-path content (source-paths forest)))))

(define* (make-sitemap-renderer #:key (settings #f) (site-url #f)
                                  (sitemap-path "sitemap.xml")
                                  (routes (make-html-routes #:home-page "index"))
                                  (default-locale
                                    (and (site-settings? settings)
                                         (site-settings-default-locale
                                           settings))))
  (unless (or (not settings) (site-settings? settings))
    (error "sitemap settings must be site settings or #f" settings))
  (let* ((site-url (or site-url
                       (and settings (site-settings-site-url settings))))
         (configuration
           (lambda (forest)
             (list 'sitemap sitemap-path site-url default-locale
                   (html-index-url routes)
                   (html-route-table routes (map site-page-id (pages forest)))))))
    (make-cached-renderer
      (lambda (forest)
        (render-sitemap forest #:site-url site-url
                        #:sitemap-path sitemap-path #:routes routes
                        #:default-locale default-locale))
      #:configuration configuration)))
