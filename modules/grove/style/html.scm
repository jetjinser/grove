;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style html)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private html-attributes)
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
  "Return STYLE's stylesheet artifacts as independent copies."
  (map snapshot-artifact (html-style-stylesheets/raw style)))

(define* (make-html-style #:key (stylesheets '()) (assets '()))
  "Describe stylesheet and extra asset artifacts contributed by an HTML style.

Both arguments are artifact lists; the constructor snapshots their contents.
Paths across both lists must be unique and must not conflict as files and
directories."
  (unless (and (list? stylesheets)
               (list? assets)
               (every artifact? stylesheets)
               (every artifact? assets))
    (error "HTML style requires artifact lists" stylesheets assets))
  (let ((stylesheets (map snapshot-artifact (snapshot-data stylesheets)))
        (assets (map snapshot-artifact (snapshot-data assets))))
    (validate-artifacts (append stylesheets assets))
    (%make-html-style stylesheets assets)))

(define (compose-html-styles . styles)
  "Combine STYLES in order, appending their stylesheets and extra assets.
With no arguments, return an empty HTML style."
  (unless (every html-style? styles)
    (error "HTML styles can only be composed from HTML styles" styles))
  (make-html-style
    #:stylesheets (append-map html-style-stylesheets styles)
    #:assets (append-map html-style-extra-assets/raw styles)))

(define (html-style-artifacts style)
  "Return STYLE's stylesheet artifacts followed by its extra assets."
  (append (html-style-stylesheets style)
          (map snapshot-artifact (html-style-extra-assets/raw style))))

(define-record-type <html-attributes>
  (%make-html-attributes assoc)
  html-attributes?
  (assoc html-attributes-assoc/raw))

(define (html-attributes-assoc attributes)
  (snapshot-data (html-attributes-assoc/raw attributes)))

(define (make-html-attributes assoc)
  "Construct a validated HTML-attributes value from HTML-name/string pairs.

Use it in content styles and HTML presentation values."
  (unless (valid-html-attribute-list? assoc)
    (error "HTML attributes must have unique valid names and string values" assoc))
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
  "Mark a styled content element as empty and reject content.

The HTML renderer emits a self-closing tag. Use this only for an element
that HTML defines as void: browsers ignore the self-closing slash on other
HTML elements. Grove leaves that tag-semantic choice to the vocabulary."
  (%make-html-void 'void))
