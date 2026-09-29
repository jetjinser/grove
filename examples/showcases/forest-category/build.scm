;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (forester input scribble)
             (forester render graph)
             (forester render html)
             (forester render rss)
             (forester render sitemap)
             (forester render text)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove project site)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-forest-category"))
(define source-directory (string-append examples-directory "/source"))
(define module-directory (string-append examples-directory "/modules"))
(define stylesheet-path (string-append examples-directory "/style.css"))
(define read-document (make-scribble-document-reader))
(define stylesheet (call-with-input-file stylesheet-path get-string-all))
(define style
  (make-html-style
    #:stylesheets
    (list (make-artifact "style.css" stylesheet (list stylesheet-path)))))

(define routes
  (make-html-routes
    #:page-path (lambda (id) (string-append id ".html"))))

(define project
  (make-site-project
    #:title "A Small Category Theory Forest"
    #:description "A CC0 excerpt adapted from two trees in Jinser Kafka's forest."
    #:source-directory source-directory
    #:extra-sources (list stylesheet-path)
    #:prepare (make-scribble-module-setup module-directory)
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer
            #:style style
            #:routes routes
            #:cache-key "forest-category-html-v1")
          (make-forester-text-renderer
            #:prefix "text"
            #:cache-key "forest-category-text-v1")
          (make-forester-rss-renderer
            #:site-url "https://category.example"
            #:routes routes
            #:cache-key "forest-category-rss-v1")
          (make-forester-sitemap-renderer
            #:site-url "https://category.example"
            #:routes routes
            #:cache-key "forest-category-sitemap-v1")
          (make-forester-graph-renderer
            #:path "forest-graph.scm"
            #:cache-key "forest-category-graph-v1"))
    #:output-directory output-directory
    #:cache-key "forest-category-project-v1"))

(build-project project)

(display "Wrote category forest outputs to ")
(display output-directory)
(newline)
