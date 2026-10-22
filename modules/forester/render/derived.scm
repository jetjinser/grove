;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render derived)
  #:use-module (grove render context)
  #:use-module (grove render derived)
  #:use-module (grove render html routes)
  #:use-module (forester render graph)
  #:use-module (forester render rss)
  #:use-module (forester render site)
  #:use-module (forester render sitemap)
  #:export (sitemap-derived
            rss-derived
            graph-derived))

(define* (sitemap-derived #:key (settings #f) (sitemap-path "sitemap.xml")
                                  (routes #f) (default-locale #f))
  (make-derived-artifact
    #:name "sitemap"
    #:procedure
    (lambda (context forest)
      (render-sitemap forest
                      #:settings settings
                      #:site-url (or (and settings
                                          (site-settings-site-url settings))
                                     (render-context-ref context 'site-url))
                      #:sitemap-path sitemap-path
                      #:routes (or routes
                                   (make-html-routes #:home-page "index"))
                      #:default-locale (or default-locale
                                           (render-context-ref context
                                                               'default-locale))))))

(define* (rss-derived #:key (settings #f) (feed-path "feed.xml")
                              (routes #f) (stylesheet-url #f)
                              (stylesheet-type "text/xsl")
                              (language #f))
  (make-derived-artifact
    #:name "rss"
    #:procedure
    (lambda (context forest)
      (render-rss forest
                  #:settings settings
                  #:site-url (or (and settings
                                      (site-settings-site-url settings))
                                 (render-context-ref context 'site-url))
                  #:feed-path feed-path
                  #:routes (or routes
                                (make-html-routes #:home-page "index"))
                  #:stylesheet-url stylesheet-url
                  #:stylesheet-type stylesheet-type
                  #:language (or language
                                 (render-context-ref context 'locale))))))

(define* (graph-derived #:key (path "forest-graph.scm"))
  (make-derived-artifact
    #:name "graph"
    #:procedure (lambda (context forest)
                  (render-graph forest #:path path))))
