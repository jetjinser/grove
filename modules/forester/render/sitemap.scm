;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render sitemap)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (srfi srfi-13)
  #:export (render-forest-sitemap
            make-forester-sitemap-renderer))

(define (absolute-site-url? url)
  (and (string? url)
       (or (string-prefix? "https://" url)
           (string-prefix? "http://" url))
       (> (string-length url)
          (if (string-prefix? "https://" url) 8 7))))

(define (trim-trailing-slashes url)
  (let ((end (string-length url)))
    (if (and (> end 0) (char=? (string-ref url (- end 1)) #\/))
        (trim-trailing-slashes (substring url 0 (- end 1)))
        url)))

(define (xml-escape text)
  (string-concatenate
    (map (lambda (character)
           (case character
             ((#\&) "&amp;")
             ((#\<) "&lt;")
             ((#\>) "&gt;")
             ((#\") "&quot;")
             ((#\') "&apos;")
             (else (string character))))
         (string->list text))))

(define (absolute-url base path)
  (string-append base "/" path))

(define* (render-forest-sitemap forest #:key site-url
                                (sitemap-path "sitemap.xml")
                                (routes (make-html-routes)))
  (unless (forest? forest)
    (error "Forester sitemap renderer requires a forest" forest))
  (unless (absolute-site-url? site-url)
    (error "Forester sitemap renderer requires an absolute HTTP(S) site URL"
           site-url))
  (unless (artifact-path? sitemap-path)
    (error "Sitemap path must be a relative artifact path" sitemap-path))
  (unless (html-routes? routes)
    (error "Forester sitemap renderer requires HTML routes" routes))
  (let* ((base (trim-trailing-slashes site-url))
         (site (forest-site forest))
         (paths (cons (html-index-path routes)
                      (map (lambda (page)
                             (html-page-path routes (site-page-id page)))
                           (site-pages site))))
         (urls
           (map (lambda (path)
                  (string-append "    <url><loc>"
                                 (xml-escape (absolute-url base path))
                                 "</loc></url>"))
                paths))
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
