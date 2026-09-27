;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (forester input scribble)
             (forester render html)
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
(define read-document (make-scribble-document-reader))

(define notes-style
  (make-html-style
    #:stylesheets
    (list
      (make-artifact
        "style.css"
        (call-with-input-file
          (string-append examples-directory "/style.css")
          get-string-all)))))

(define notes-routes
  (make-html-routes
    #:page-path
    (lambda (id)
      (string-append "notes/" id ".html"))))

(define notes-project
  (make-project
    #:title "Linked Notes"
    #:source-directory source-directory
    #:module-directory (string-append source-directory "/modules")
    #:load-modules load-scribble-modules!
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer
            #:style notes-style #:routes notes-routes)
          (lambda (forest)
            (prefix-artifacts "text" (render-forest-text forest))))
    #:output-directory output-directory))

(build-project notes-project)

(display "Wrote HTML to ")
(display output-directory)
(display " and text to ")
(display (string-append output-directory "/text"))
(newline)
