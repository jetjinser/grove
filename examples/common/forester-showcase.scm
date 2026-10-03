;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 textual-ports)
             (forester input scribble)
             (forester render graph)
             (forester render html)
             (forester render rss)
             (forester render sitemap)
             (forester render text)
             (forester site)
             (forester style html)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove project site)
             (grove render html routes)
             (grove style html))

(define* (build-forester-showcase directory
                                  #:key title description site-url home-page
                                  output-directory cache-key)
  (let* ((source-directory (string-append directory "/plants"))
         (module-directory (string-append directory "/modules"))
         (stylesheet-path (string-append directory "/style.css"))
         (stylesheet (call-with-input-file stylesheet-path get-string-all))
         (style
           (compose-html-styles
             default-html-style
             (make-html-style
               #:stylesheets
               (list (make-artifact "style.css" stylesheet
                                    (list stylesheet-path))))))
         (routes (make-html-routes #:home-page home-page))
         (project
           (make-site-project
             #:title title
             #:description description
             #:source-directory source-directory
             #:extra-sources (list stylesheet-path)
             #:prepare (make-scribble-module-setup module-directory)
             #:read-page
             (make-page-reader (make-scribble-document-reader))
             #:analyze make-forest
             #:renderers
             (list (make-html-renderer
                     #:style style #:routes routes
                     #:cache-key (string-append cache-key "-html-v1"))
                   (make-text-renderer
                     #:prefix "text"
                     #:cache-key (string-append cache-key "-text-v1"))
                   (make-rss-renderer
                     #:site-url site-url #:routes routes
                     #:cache-key (string-append cache-key "-rss-v1:" site-url))
                   (make-sitemap-renderer
                     #:site-url site-url #:routes routes
                     #:cache-key
                     (string-append cache-key "-sitemap-v1:" site-url))
                   (make-graph-renderer
                     #:path "forest-graph.scm"
                     #:cache-key (string-append cache-key "-graph-v1")))
             #:output-directory output-directory
             #:cache-key
             (string-append cache-key "-project-v1:" title ":" description))))
    (build-project project)))
