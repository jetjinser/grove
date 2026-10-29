;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style html element)
  #:use-module (grove private html-attributes)
  #:use-module (grove private html-tag)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-9)
  #:export (make-html-attributes
            html-attributes?
            html-attributes-assoc
            make-html-tag
            html-tag?
            html-tag-name))

(define-record-type <html-attributes>
  (%make-html-attributes assoc)
  html-attributes?
  (assoc html-attributes-assoc/raw))

(define (html-attributes-assoc attributes)
  (snapshot-data (html-attributes-assoc/raw attributes)))

(define (make-html-attributes assoc)
  "Construct validated HTML attributes for content and presentation values."
  (unless (valid-html-attribute-list? assoc)
    (error "HTML attributes must have unique names and string values" assoc))
  (%make-html-attributes (snapshot-data assoc)))

(define-record-type <html-tag>
  (%make-html-tag name)
  html-tag?
  (name html-tag-name/raw))

(define (html-tag-name tag)
  (snapshot-data (html-tag-name/raw tag)))

(define (make-html-tag name)
  "Use NAME instead of the HTML renderer's default tag for a content element."
  (let ((name (if (symbol? name) (symbol->string name) name)))
    (unless (valid-html-tag-name? name)
      (error "invalid HTML tag name" name))
    (%make-html-tag (snapshot-data name))))
