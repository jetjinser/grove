;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (forester input scribble)
             (forester render html)
             (forester render sitemap)
             (forester render text)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-linked-notes"))

(define source-directory (string-append examples-directory "/source"))
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
    #:page-path
    (lambda (id)
      (string-append "notes/" id ".html"))))

(define notes-project
  (make-project
    #:title "Linked Notes"
    #:description "Connected notes on mathematics and its ideas."
    #:source-directory source-directory
    #:module-directory (string-append source-directory "/modules")
    #:extra-sources (list notes-css-path)
    #:load-modules load-scribble-modules!
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer
            #:style notes-style
            #:routes notes-routes
            #:cache-key "linked-notes-html-v1")
          (make-forester-text-renderer
            #:prefix "text"
            #:cache-key "linked-notes-text-v1")
          (make-forester-sitemap-renderer
            #:site-url "https://notes.example"
            #:routes notes-routes
            #:cache-key "linked-notes-sitemap-v1"))
    #:output-directory output-directory
    #:cache-key "linked-notes-project-v1"))

(build-project notes-project)

(display "Wrote HTML, text, and sitemap outputs to ")
(display output-directory)
(newline)
