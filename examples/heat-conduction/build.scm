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
             (forester render text routes)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove project site)
             (grove render html routes)
             (grove style html))

(define example-directory (dirname (current-filename)))
(define source-directory (string-append example-directory "/source"))
(define module-directory (string-append example-directory "/modules"))
(define stylesheet-path (string-append example-directory "/style.css"))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-heat-conduction"))
(define read-document (make-scribble-document-reader))

(define heat-style
  (make-html-style
    #:stylesheets
    (list (make-artifact "style.css"
                         (call-with-input-file stylesheet-path get-string-all)
                         (list stylesheet-path)))))
(define heat-routes
  (make-html-routes #:page-path
    (lambda (id) (string-append "heat/" id ".html"))))
(define text-routes
  (make-text-routes #:page-path
    (lambda (id) (string-append "heat/" id ".txt"))))

(define heat-project
  (make-site-project
    #:title "Heat: a derivation in notes"
    #:description "A linked, worked derivation of the heat equation."
    #:source-directory source-directory
    #:extra-sources (list stylesheet-path)
    #:prepare (make-scribble-module-setup module-directory)
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer
            #:style heat-style #:routes heat-routes
            #:cache-key "heat-conduction-html-v1")
          (make-forester-text-renderer
            #:prefix "text" #:routes text-routes
            #:cache-key "heat-conduction-text-v1")
          (make-forester-rss-renderer
            #:site-url "https://heat.example"
            #:routes heat-routes #:cache-key "heat-conduction-rss-v1")
          (make-forester-sitemap-renderer
            #:site-url "https://heat.example"
            #:routes heat-routes #:cache-key "heat-conduction-sitemap-v1")
          (make-forester-graph-renderer
            #:path "forest-graph.scm"
            #:cache-key "heat-conduction-graph-v1"))
    #:output-directory output-directory
    #:cache-key "heat-conduction-project-v1"))

(build-project heat-project)
(display "Wrote heat-conduction site to ")
(display output-directory)
(newline)
