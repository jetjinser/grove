;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 textual-ports)
             (forester input scribble)
             (forester render derived)
             (forester render html)
             (forester render site)
             (forester render text)
             (forester site)
             (forester style html)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove project site)
             (grove render html presentation)
             (grove render html routes)
             (grove style html))

(define* (make-forester-showcase directory
                                 #:key title description site-url home-page
                                 output-directory)
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
         (settings (make-site-settings #:site-url site-url))
         (project
           (make-site-project
             #:title title
             #:description description
             #:source-directory source-directory
             #:extra-sources
             (list stylesheet-path
                   (string-append directory "/../../common/forester-showcase.scm"))
             #:prepare (make-scribble-module-setup module-directory)
             #:read-page
             (make-page-reader (make-scribble-document-reader))
             #:analyze make-forest
             #:renderers
             (list (make-html-renderer
                     #:options
                     (make-html-options
                       #:presentation (make-html-presentation #:style style)
                       #:routes routes
                       #:settings settings))
                   (make-text-renderer
                     #:options (make-text-options #:prefix "text")))
             #:derived
             (list (rss-derived #:settings settings #:routes routes)
                   (sitemap-derived #:settings settings #:routes routes)
                   (graph-derived #:path "forest-graph.scm"))
             #:output-directory output-directory)))
    project))
