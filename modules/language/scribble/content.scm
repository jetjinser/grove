;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble content)
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
            content-element-tag
            content-element-children
            content-element-source
            content-document?
            make-content-document
            content-document-children
            content-document-source
            content-link?
            make-content-link
            content-link-target
            content-link-children
            content-math?
            make-content-math
            content-math-source-text
            content-math-display?
            content->sxml))

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
  (%make-content-element tag children source)
  content-element?
  (tag content-element-tag)
  (children content-element-children)
  (source content-element-source))

(define (normalize-child child)
  (if (string? child)
      (make-content-text child)
      child))

(define* (make-content-element tag children #:optional (source #f))
  (%make-content-element tag (map normalize-child children) source))

(define-record-type <content-document>
  (%make-content-document children source)
  content-document?
  (children content-document-children)
  (source content-document-source))

(define* (make-content-document children #:optional (source #f))
  (%make-content-document (map normalize-child children) source))

(define-record-type <content-link>
  (%make-content-link target children)
  content-link?
  (target content-link-target)
  (children content-link-children))

(define (make-content-link target children)
  (%make-content-link target (map normalize-child children)))

(define-record-type <content-math>
  (%make-content-math source-text display?)
  content-math?
  (source-text content-math-source-text)
  (display? content-math-display?))

(define (make-content-math source-text display?)
  (%make-content-math source-text display?))

(define (content-verbatim-string fragments)
  (string-concatenate
    (map (lambda (fragment)
           (cond
             ((string? fragment) fragment)
             ((content-text? fragment) (content-text-value fragment))
             ((content-layout? fragment) (content-layout-raw fragment))
             (else (error "not a content fragment" fragment))))
         fragments)))

(define (content->sxml content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content)
     (cons (content-element-tag content)
           (map content->sxml (content-element-children content))))
    ((content-link? content)
     `(a (@ (href ,(string-append "/" (content-link-target content) ".html")))
         ,@(map content->sxml (content-link-children content))))
    ((content-math? content)
     `(,(if (content-math-display? content) 'div 'span)
        (@ (class ,(if (content-math-display? content)
                       "math-display" "math-inline")))
        ,(string-append
           (if (content-math-display? content) "\\[" "\\(")
           (content-math-source-text content)
           (if (content-math-display? content) "\\]" "\\)"))))
    ((content-document? content)
     (cons 'article
           (map content->sxml (content-document-children content))))
    (else (error "not a content node" content))))
