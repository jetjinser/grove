;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render derived)
  #:use-module (grove render context)
  #:use-module (grove render derived)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (forester site)
  #:use-module (forester render graph)
  #:use-module (forester render rss)
  #:use-module (forester render site)
  #:use-module (forester render sitemap)
  #:export (sitemap-derived
            rss-derived
            graph-derived))

(define* (sitemap-derived #:key (settings #f) (site-url #f)
                                  (sitemap-path "sitemap.xml")
                                  (routes #f)
                                  (default-locale
                                    (and (site-settings? settings)
                                         (site-settings-default-locale
                                           settings))))
  (unless (or (not settings) (site-settings? settings))
    (error "sitemap settings must be site settings or #f" settings))
  (let ((routes (or routes (make-html-routes #:home-page "index"))))
    (make-derived-artifact
      #:name "sitemap"
      #:procedure
      (lambda (context forest)
        (render-sitemap forest
                        #:settings settings
                        #:site-url (or site-url
                                       (and settings
                                            (site-settings-site-url settings))
                                       (render-context-ref context 'site-url))
                        #:sitemap-path sitemap-path
                        #:routes routes
                        #:default-locale (or default-locale
                                             (and settings
                                                  (site-settings-default-locale
                                                    settings))
                                             (render-context-ref context
                                                                 'default-locale))))
      #:configuration
      (lambda (context forest)
        (list 'sitemap
              sitemap-path
              (or site-url
                  (and settings (site-settings-site-url settings))
                  (render-context-ref context 'site-url))
              (or default-locale
                  (and settings (site-settings-default-locale settings))
                  (render-context-ref context 'default-locale))
              (html-index-url routes)
              (html-route-table routes (map site-page-id (pages forest))))))))

(define* (rss-derived #:key (settings #f) (site-url #f)
                              (feed-path "feed.xml")
                              (routes #f) (stylesheet-url #f)
                              (stylesheet-type "text/xsl")
                              (language
                                (and (site-settings? settings)
                                     (site-settings-default-locale
                                       settings))))
  (unless (or (not settings) (site-settings? settings))
    (error "RSS settings must be site settings or #f" settings))
  (let ((routes (or routes (make-html-routes #:home-page "index"))))
    (make-derived-artifact
      #:name "rss"
      #:procedure
      (lambda (context forest)
        (render-rss forest
                    #:settings settings
                    #:site-url (or site-url
                                   (and settings
                                        (site-settings-site-url settings))
                                   (render-context-ref context 'site-url))
                    #:feed-path feed-path
                    #:routes routes
                    #:stylesheet-url stylesheet-url
                    #:stylesheet-type stylesheet-type
                    #:language (or language
                                   (and settings
                                        (site-settings-default-locale settings))
                                   (render-context-ref context 'locale))))
      #:configuration
      (lambda (context forest)
        (list 'rss
              feed-path
              (or site-url
                  (and settings (site-settings-site-url settings))
                  (render-context-ref context 'site-url))
              stylesheet-url
              stylesheet-type
              (or language
                  (and settings (site-settings-default-locale settings))
                  (render-context-ref context 'locale))
              (html-index-url routes)
              (html-route-table routes (map site-page-id (pages forest))))))))

(define* (graph-derived #:key (path "forest-graph.scm"))
  (make-derived-artifact
    #:name "graph"
    #:procedure (lambda (context forest)
                  (render-graph forest #:path path))
    #:configuration (lambda (context forest)
                      (list 'graph path))))
