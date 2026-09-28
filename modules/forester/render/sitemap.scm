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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (render-forest-sitemap
            make-forester-sitemap-renderer))

(define (page-lastmod page)
  (let ((updated (assq-ref (site-page-metadata page) 'updated)))
    (when (and updated (not (forester-date? updated)))
      (error "Forester sitemap updated metadata must use YYYY-MM-DD"
             (site-page-id page) updated))
    updated))

(define* (render-forest-sitemap forest #:key site-url
                                (sitemap-path "sitemap.xml")
                                (routes (make-html-routes)))
  (unless (forest? forest)
    (error "Forester sitemap renderer requires a forest" forest))
  (unless (web-url? site-url)
    (error "Forester sitemap renderer requires an absolute HTTP(S) site URL"
           site-url))
  (unless (artifact-path? sitemap-path)
    (error "Sitemap path must be a relative artifact path" sitemap-path))
  (unless (html-routes? routes)
    (error "Forester sitemap renderer requires HTML routes" routes))
  (let* ((base (web-base-url site-url))
         (urls
           (cons
             (string-append
               "    <url>\n      <loc>"
               (web-xml-escape
                 (web-absolute-route-url base (html-index-url routes)))
               "</loc>\n    </url>")
             (map (lambda (page)
                    (let ((lastmod (page-lastmod page)))
                      (string-append
                        "    <url>\n      <loc>"
                        (web-xml-escape
                          (web-absolute-route-url
                            base (html-page-url routes (site-page-id page))))
                        "</loc>\n"
                        (if lastmod
                            (string-append "      <lastmod>" lastmod
                                           "</lastmod>\n")
                            "")
                        "    </url>")))
                  (forest-pages forest))))
         (content
           (string-append
             "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
             "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
             (if (null? urls) "" (string-append (string-join urls "\n") "\n"))
             "</urlset>\n")))
    (list (make-artifact sitemap-path content (forest-source-paths forest)))))

(define* (make-forester-sitemap-renderer #:key site-url
                                         (sitemap-path "sitemap.xml")
                                         (routes (make-html-routes))
                                         (cache-key #f))
  (let ((render (lambda (forest)
                  (render-forest-sitemap forest
                                         #:site-url site-url
                                         #:sitemap-path sitemap-path
                                         #:routes routes))))
    (if cache-key
        (make-cached-renderer render #:cache-key cache-key)
        render)))
