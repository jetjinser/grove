;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render rss)
  #:use-module (grove build)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove render text)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (render-forest-rss
            make-forester-rss-renderer))

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

(define (page-url base routes page)
  (string-append base "/"
                 (html-page-path routes (site-page-id page))))

(define* (render-forest-rss forest #:key site-url
                            (feed-path "feed.xml")
                            (routes (make-html-routes)))
  (unless (forest? forest)
    (error "Forester RSS renderer requires a forest" forest))
  (unless (absolute-site-url? site-url)
    (error "Forester RSS renderer requires an absolute HTTP(S) site URL"
           site-url))
  (unless (artifact-path? feed-path)
    (error "RSS feed path must be a relative artifact path" feed-path))
  (unless (html-routes? routes)
    (error "Forester RSS renderer requires HTML routes" routes))
  (let* ((base (trim-trailing-slashes site-url))
         (site (forest-site forest))
         (home-url (string-append base "/" (html-index-path routes)))
         (items
           (map (lambda (page)
                  (let ((url (page-url base routes page)))
                    (string-append
                      "    <item>\n"
                      "      <title>" (xml-escape (site-page-title page))
                      "</title>\n"
                      "      <link>" (xml-escape url) "</link>\n"
                      "      <guid isPermaLink=\"true\">"
                      (xml-escape url) "</guid>\n"
                      "      <description>"
                      (xml-escape
                        (string-trim-both
                          (content->text (site-page-document page))))
                      "</description>\n"
                      "    </item>")))
                (site-pages site)))
         (content
           (string-append
             "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
             "<rss version=\"2.0\">\n"
             "  <channel>\n"
             "    <title>" (xml-escape (site-title site)) "</title>\n"
             "    <link>" (xml-escape home-url) "</link>\n"
             "    <description>" (xml-escape (site-title site))
             "</description>\n"
             (if (null? items) "" (string-append (string-join items "\n") "\n"))
             "  </channel>\n"
             "</rss>\n")))
    (list (make-artifact feed-path content (forest-source-paths forest)))))

(define* (make-forester-rss-renderer #:key site-url
                                     (feed-path "feed.xml")
                                     (routes (make-html-routes))
                                     (cache-key #f))
  (let ((render (lambda (forest)
                  (render-forest-rss forest
                                     #:site-url site-url
                                     #:feed-path feed-path
                                     #:routes routes))))
    (if cache-key
        (make-cached-renderer render #:cache-key cache-key)
        render)))
