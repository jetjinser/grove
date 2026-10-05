;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render rss)
  #:use-module (grove build)
  #:use-module (forester site)
  #:use-module (forester private web)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove render text)
  #:use-module (grove site)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (render-rss
            make-rss-renderer))

(define (page-url base routes page)
  (web-absolute-route-url base (html-page-url routes (site-page-id page))))

(define (page-pub-date page)
  (or (assq-ref (site-page-metadata page) 'updated)
      (assq-ref (site-page-metadata page) 'published)))

(define rss-text-handlers
  `((tag . ,(lambda (_element _body) ""))
    (taxon . ,(lambda (_element _body) ""))
    (blockquote . ,(lambda (_ body)
                     (string-append
                       (string-join
                         (map (lambda (line) (string-append "> " line))
                              (string-split body #\newline))
                         "\n")
                       "\n")))
    (item . ,(lambda (_ body) (string-append "- " body "\n")))
    (code . ,(lambda (_ body) body))
    (code-block . ,(lambda (_ body) (string-append body "\n")))
    (math . ,(lambda (_ body) (string-append body "\n")))))

(define (rss-date iso-date)
  (let* ((parsed (string->date iso-date "~Y-~m-~d"))
         (utc-date (make-date 0 0 0 0
                              (date-day parsed)
                              (date-month parsed)
                              (date-year parsed)
                              0)))
    (date->string utc-date "~a, ~d ~b ~Y 00:00:00 GMT")))

(define (page-description page)
  (or (assq-ref (site-page-metadata page) 'summary)
      (string-trim-both
        (content->text (site-page-document page)
                       #:handlers rss-text-handlers))))

(define (feed-pages pages)
  (stable-sort
    pages
    (lambda (left right)
      (let ((left-date (page-pub-date left))
            (right-date (page-pub-date right)))
        (cond
          ((and left-date right-date)
           (string>? left-date right-date))
          (left-date #t)
          (right-date #f)
          (else #f))))))

(define (channel-description site)
  (or (site-description site) (site-title site)))

(define (rss-stylesheet-url? url)
  (and (string? url)
       (not (string-null? url))
       (not (string-any char-whitespace? url))))

(define (page->rss-item base routes page)
  (let ((url (page-url base routes page))
        (pub-date (page-pub-date page)))
    `(item
       (title ,(site-page-title page))
       (link ,url)
       (guid (@ (isPermaLink "true")) ,url)
       ,@(if pub-date `((pubDate ,(rss-date pub-date))) '())
       (description ,(page-description page)))))

(define (rss-document site home-url items stylesheet-url stylesheet-type)
  (append
    (list '*TOP*
          (list '*PI* 'xml "version=\"1.0\" encoding=\"utf-8\"")
          "\n")
    (if stylesheet-url
        (list (list '*PI* 'xml-stylesheet
                    (string-append
                      "type=\"" (web-xml-escape stylesheet-type)
                      "\" href=\"" (web-xml-escape stylesheet-url) "\""))
              "\n")
        '())
    (list
      `(rss (@ (version "2.0"))
         (channel
           (title ,(site-title site))
           (link ,home-url)
           (description ,(channel-description site))
           ,@items)))))

(define (serialize-rss document)
  (call-with-output-string (lambda (port) (sxml->xml document port))))

(define* (render-rss forest #:key site-url
                            (feed-path "feed.xml")
                            (routes (make-html-routes #:home-page "index"))
                            stylesheet-url
                            (stylesheet-type "text/xsl"))
  (unless (forest? forest)
    (error "Forester RSS renderer requires a forest" forest))
  (unless (web-url? site-url)
    (error "Forester RSS renderer requires an absolute HTTP(S) site URL"
           site-url))
  (unless (artifact-path? feed-path)
    (error "RSS feed path must be a relative artifact path" feed-path))
  (unless (html-routes? routes)
    (error "Forester RSS renderer requires HTML routes" routes))
  (when (and stylesheet-url (not (rss-stylesheet-url? stylesheet-url)))
    (error "RSS stylesheet URL must be a non-empty URL without whitespace"
           stylesheet-url))
  (unless (rss-stylesheet-url? stylesheet-type)
    (error "RSS stylesheet type must be a non-empty media type without whitespace"
           stylesheet-type))
  (let* ((base (web-base-url site-url))
         (site (site forest))
         (home-url (web-absolute-route-url base (html-index-url routes)))
         (items
           (map (lambda (page) (page->rss-item base routes page))
                (feed-pages (site-pages site))))
         (content
           (serialize-rss
             (rss-document site home-url items stylesheet-url stylesheet-type))))
    (list (make-artifact feed-path content (source-paths forest)))))

(define* (make-rss-renderer #:key site-url
                                     (feed-path "feed.xml")
                                     (routes (make-html-routes #:home-page "index"))
                                     stylesheet-url
                                     (stylesheet-type "text/xsl")
                                     (cache? #t))
  (let ((render (lambda (forest)
                  (render-rss forest
                                     #:site-url site-url
                                     #:feed-path feed-path
                                     #:routes routes
                                     #:stylesheet-url stylesheet-url
                                     #:stylesheet-type stylesheet-type))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'rss site-url feed-path stylesheet-url stylesheet-type
                  (site-title (site forest)) (site-description (site forest))
                  (html-index-url routes)
                  (html-route-table routes
                                    (map site-page-id (pages forest))))))))
