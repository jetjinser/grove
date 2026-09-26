;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (srfi srfi-13)
             (forester input scribble)
             (forester render html)
             (forester render text)
             (forester site)
             (grove build)
             (grove collect)
             (grove input scribble)
             (grove output)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-linked-notes"))

(define source-directory (string-append examples-directory "/source"))

(define notes-site
  (build-site
    (collect-directory
      source-directory
      (lambda (filename)
        (string-suffix? ".grv" filename)))
    (make-forester-page-reader
      (make-scribble-document-reader))
    #:title "Linked Notes"))

(define notes-forest (make-forest notes-site))

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

(define artifacts
  (render-site
    notes-forest
    (list (make-forester-html-renderer
            #:style notes-style #:routes notes-routes)
          (lambda (forest)
            (prefix-artifacts "text" (render-forest-text forest))))))

(write-artifacts output-directory artifacts)

(display "Wrote HTML to ")
(display output-directory)
(display " and text to ")
(display (string-append output-directory "/text"))
(newline)
