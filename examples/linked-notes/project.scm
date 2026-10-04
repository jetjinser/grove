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
             (grove render static)
             (forester render text)
             (forester render text routes)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove project site)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory "build")

(define source-directory (string-append examples-directory "/plants"))
(define module-directory (string-append examples-directory "/modules"))
(define notes-css-path (string-append examples-directory "/style.css"))
(define read-document (make-scribble-document-reader))

(define notes-css
  (call-with-input-file notes-css-path get-string-all))

(define notes-style
  (make-html-style
    #:stylesheets
    (list
      (make-artifact "style.css" notes-css (list notes-css-path)))))

(define notes-routes
  (make-html-routes
    #:home-page "math-0001"
    #:page-path
    (lambda (id)
      (string-append id ".html"))))

(define notes-text-routes
  (make-text-routes
    #:page-path
    (lambda (id)
      (string-append id ".txt"))))

(define notes-project
  (make-site-project
    #:title "Linked Notes"
    #:description "Connected notes on mathematics and its ideas."
    #:source-directory source-directory
    #:extra-sources (list notes-css-path)
    #:prepare
    (make-scribble-module-setup module-directory)
    #:read-page (make-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-html-renderer
            #:style notes-style
            #:routes notes-routes
            #:cache-key "linked-notes-html-v2")
          (make-text-renderer
            #:prefix "text"
            #:routes notes-text-routes
            #:cache-key "linked-notes-text-v2")
          (make-rss-renderer
            #:site-url "https://notes.example"
            #:routes notes-routes
            #:cache-key "linked-notes-rss-v2")
          (make-sitemap-renderer
            #:site-url "https://notes.example"
            #:routes notes-routes
            #:cache-key "linked-notes-sitemap-v2")
          (make-graph-renderer
            #:path "forest-graph.scm"
            #:cache-key "linked-notes-graph-v1")
          (make-static-assets-renderer
            #:source-directory (string-append examples-directory "/static")))
    #:output-directory output-directory
    #:cache-key "linked-notes-project-v2"))

notes-project
