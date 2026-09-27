;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-13)
             (grove collect)
             (grove ir)
             (grove output)
             (grove project)
             (grove render html)
             (grove render text)
             (grove style html))

(define-record-type <quote-entry>
  (make-quote-entry title body source)
  quote-entry?
  (title quote-entry-title)
  (body quote-entry-body)
  (source quote-entry-source))

(define-record-type <quote-catalog>
  (make-quote-catalog entries)
  quote-catalog?
  (entries quote-catalog-entries))

(define example-directory (dirname (current-filename)))
(define quote-directory (string-append example-directory "/quotes"))
(define catalog-css-path (string-append example-directory "/style.css"))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-catalog"))

(define (read-quote path)
  (string-trim-both
    (string-join
      (filter (lambda (line) (not (string-prefix? ";; SPDX-" line)))
              (string-split (call-with-input-file path get-string-all) #\newline))
      "\n")))

(define (quote-title path)
  (string-titlecase
    (string-join
      (string-split
        (string-drop-right (basename path) (string-length ".quote"))
        #\-)
      " ")))

(define (analyze-catalog context)
  (make-quote-catalog
    (map (lambda (input)
           (make-quote-entry (quote-title (project-input-path input))
                             (project-input-value input)
                             (project-input-path input)))
         (project-context-inputs context))))

(define (catalog-document catalog)
  (make-content-document
    (append-map
      (lambda (entry)
        (list (make-content-element 'heading (list (quote-entry-title entry)))
              (make-content-element 'paragraph (list (quote-entry-body entry)))))
      (quote-catalog-entries catalog))))

(define (catalog-sources catalog)
  (map quote-entry-source (quote-catalog-entries catalog)))

(define catalog-style
  (make-html-style
    #:stylesheets
    (list (make-artifact
            "catalog.css"
            (call-with-input-file catalog-css-path get-string-all)
            (list catalog-css-path)))))

(define (render-catalog-html catalog)
  (append
    (html-style-artifacts catalog-style)
    (list
      (make-artifact
        "index.html"
        (sxml->html
          `(html
             (head
               (title "Quote catalog")
               (link (@ (rel "stylesheet") (href "/catalog.css"))))
             (body ,(content->sxml (catalog-document catalog)))))
        (catalog-sources catalog)))))

(define (render-catalog-text catalog)
  (list (make-artifact "catalog.txt"
                       (content->text (catalog-document catalog))
                       (catalog-sources catalog))))

(define catalog-project
  (make-project
    #:collect-sources
    (lambda ()
      (collect-directory quote-directory
                         (lambda (name) (string-suffix? ".quote" name))))
    #:extra-sources (list catalog-css-path)
    #:read-source read-quote
    #:analyze analyze-catalog
    #:renderers (list render-catalog-html render-catalog-text)
    #:output-directory output-directory))

(build-project catalog-project)
(display "Wrote the quote catalog to ")
(display output-directory)
(newline)
