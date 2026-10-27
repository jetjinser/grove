;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style html)
  #:use-module ((grove private output) #:prefix out:)
  #:use-module (grove output)
  #:use-module (grove private snapshot)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (html-style?
            make-html-style
            compose-html-styles
            html-style-stylesheets
            html-style-artifacts
            make-html-attributes html-attributes? html-attributes-assoc
            make-html-tag html-tag? html-tag-name
            make-html-void html-void?))

(define-record-type <html-style>
  (%make-html-style stylesheets assets)
  html-style?
  (stylesheets html-style-stylesheets/raw)
  (assets html-style-extra-assets/raw))

(define (snapshot-artifact artifact)
  (let ((path (snapshot-data (out:artifact-path/raw artifact)))
        (content (out:artifact-content/raw artifact))
        (sources (out:artifact-sources/raw artifact)))
    (if (string? content)
        (make-artifact path (snapshot-data content)
                       (snapshot-data sources))
        (make-byte-artifact path (bytevector-copy content)
                            (snapshot-data sources)))))

(define (html-style-stylesheets style)
  (map snapshot-artifact (html-style-stylesheets/raw style)))

(define* (make-html-style #:key (stylesheets '()) (assets '()))
  (unless (and (list? stylesheets)
               (list? assets)
               (every artifact? stylesheets)
               (every artifact? assets))
    (error "HTML style requires artifact lists" stylesheets assets))
  (%make-html-style (map snapshot-artifact (snapshot-data stylesheets))
                    (map snapshot-artifact (snapshot-data assets))))

(define (compose-html-styles . styles)
  (unless (every html-style? styles)
    (error "HTML styles can only be composed from HTML styles" styles))
  (make-html-style
    #:stylesheets (append-map html-style-stylesheets styles)
    #:assets (append-map html-style-extra-assets/raw styles)))

(define (html-style-artifacts style)
  (append (html-style-stylesheets style)
          (map snapshot-artifact (html-style-extra-assets/raw style))))

(define-record-type <html-attributes>
  (%make-html-attributes assoc)
  html-attributes?
  (assoc html-attributes-assoc/raw))

(define (html-attributes-assoc attributes)
  (snapshot-data (html-attributes-assoc/raw attributes)))

(define (valid-html-attributes? assoc)
  (and (list? assoc)
       (every (lambda (attribute)
                (and (pair? attribute)
                     (symbol? (car attribute))
                     (string? (cdr attribute))))
              assoc)
       (let loop ((remaining assoc) (seen '()))
         (or (null? remaining)
             (and (not (memq (caar remaining) seen))
                  (loop (cdr remaining) (cons (caar remaining) seen)))))))

(define (make-html-attributes assoc)
  "Add HTML attributes to the tag produced for a styled content element."
  (unless (valid-html-attributes? assoc)
    (error "HTML attributes must be unique symbol/string pairs" assoc))
  (%make-html-attributes (snapshot-data assoc)))

(define-record-type <html-tag>
  (%make-html-tag name)
  html-tag?
  (name html-tag-name/raw))

(define (html-tag-name tag)
  (snapshot-data (html-tag-name/raw tag)))

(define (html-tag-name? name)
  (and (string? name) (> (string-length name) 0)
       (char-alphabetic? (string-ref name 0))
       (every (lambda (character)
                (or (char-alphabetic? character) (char-numeric? character)
                    (memv character '(#\- #\_ #\:))))
              (string->list (substring name 1)))))

(define (make-html-tag name)
  "Use NAME instead of the HTML renderer's default tag for a content element."
  (let ((name (if (symbol? name) (symbol->string name) name)))
    (unless (html-tag-name? name)
      (error "invalid HTML tag name" name))
    (%make-html-tag (snapshot-data name))))

(define-record-type <html-void>
  (%make-html-void marker)
  html-void?
  (marker html-void-marker))

(define (make-html-void)
  "Mark a styled content element as empty: the HTML renderer emits a
self-closing tag and rejects content.  Voidness is the vocabulary
author's decision; the renderer keeps no element table of its own."
  (%make-html-void 'void))
