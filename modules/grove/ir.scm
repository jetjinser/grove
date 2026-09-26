;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove ir)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (content-text?
            make-content-text
            content-text-value
            content-text-source
            content-layout?
            make-content-layout
            content-layout-kind
            content-layout-raw
            content-layout-source
            content-verbatim-string
            content-element?
            make-content-element
            content-element-kind
            content-element-children
            content-element-properties
            content-element-property
            content-element-source
            content-document?
            make-content-document
            content-document-children
            content-document-source))

(define-record-type <content-text>
  (%make-content-text value source)
  content-text?
  (value content-text-value)
  (source content-text-source))

(define* (make-content-text value #:optional (source #f))
  (%make-content-text value source))

(define-record-type <content-layout>
  (%make-content-layout kind raw source)
  content-layout?
  (kind content-layout-kind)
  (raw content-layout-raw)
  (source content-layout-source))

(define* (make-content-layout kind raw #:optional (source #f))
  (%make-content-layout kind raw source))

(define-record-type <content-element>
  (%make-content-element kind children properties source)
  content-element?
  (kind content-element-kind)
  (children content-element-children)
  (properties content-element-properties)
  (source content-element-source))

(define (normalize-child child)
  (if (string? child)
      (make-content-text child)
      child))

(define* (make-content-element kind children #:optional (source #f)
                               #:key (properties '()))
  (%make-content-element kind (map normalize-child children)
                         properties source))

(define (content-element-property element key)
  (assq-ref (content-element-properties element) key))

(define-record-type <content-document>
  (%make-content-document children source)
  content-document?
  (children content-document-children)
  (source content-document-source))

(define* (make-content-document children #:optional (source #f))
  (%make-content-document (map normalize-child children) source))

(define (content-verbatim-string fragments)
  (string-concatenate
    (map (lambda (fragment)
           (cond
             ((string? fragment) fragment)
             ((content-text? fragment) (content-text-value fragment))
             ((content-layout? fragment) (content-layout-raw fragment))
             (else (error "not a content fragment" fragment))))
         fragments)))
