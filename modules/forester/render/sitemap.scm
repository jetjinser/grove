;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render sitemap)
  #:use-module (forester private date)
  #:use-module (forester private web)
  #:use-module (forester site)
  #:use-module (grove build)
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

(define (page->sitemap-url base routes route-table page)
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
       "\n  ")))

(define (sitemap-document base routes route-table pages)
  (call-with-output-string
    (lambda (port)
      (sxml->xml
        `(*TOP*
           (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
           "\n"
           (urlset (@ (xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"))
             ,@(append-map
                 (lambda (page)
                   (list "\n  "
                         (page->sitemap-url base routes route-table page)))
                 pages)
             "\n"))
        port))))

(define* (render-sitemap forest #:key site-url
                                (sitemap-path "sitemap.xml")
                                (routes (make-html-routes #:home-page "index")))
  (unless (forest? forest)
    (error "Forester sitemap renderer requires a forest" forest))
  (unless (web-url? site-url)
    (error "Forester sitemap renderer requires an absolute HTTP(S) site URL"
           site-url))
  (unless (artifact-path? sitemap-path)
    (error "Sitemap path must be a relative artifact path" sitemap-path))
  (unless (html-routes? routes)
    (error "Forester sitemap renderer requires HTML routes" routes))
  (unless (find-page forest (html-home-page routes))
    (error "configured HTML home page was not found" (html-home-page routes)))
  (let* ((base (web-base-url site-url))
         (route-table
           (html-route-table routes (map site-page-id (pages forest))))
         (content (sitemap-document base routes route-table (pages forest))))
    (list (make-artifact sitemap-path content (source-paths forest)))))

(define* (make-sitemap-renderer #:key site-url
                                         (sitemap-path "sitemap.xml")
                                         (routes (make-html-routes #:home-page "index"))
                                         (cache? #t))
  (let ((render (lambda (forest)
                  (render-sitemap forest
                                         #:site-url site-url
                                         #:sitemap-path sitemap-path
                                         #:routes routes))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'sitemap site-url sitemap-path
                  (site-title (site forest)) (site-description (site forest))
                  (html-index-url routes)
                  (html-route-table routes
                                    (map site-page-id (pages forest))))))))
