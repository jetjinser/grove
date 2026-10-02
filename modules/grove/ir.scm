;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (content-text?
            make-content-text
            content-text-value
            content-text-source
            content-text-location
            content-with-source-location
            content-layout?
            make-content-layout
            content-layout-kind
            content-layout-raw
            content-layout-source
            content-verbatim-string
            content-element?
            make-content-element
            content-element-kind
            content-element-flow-kind
            content-element-block?
            content-element-children
            content-element-properties
            content-element-property
            content-element-source
            content-document?
            make-content-document
            content-document-children
            content-document-source))

(define-record-type <content-text>
  (%make-content-text value source location)
  content-text?
  (value content-text-value)
  (source content-text-source)
  (location content-text-location))

(define* (make-content-text value #:optional (source #f)
                            #:key (location #f))
  (unless (string? value)
    (error "content text must be a string" value))
  (%make-content-text value source location))

(define-record-type <content-layout>
  (%make-content-layout kind raw source)
  content-layout?
  (kind content-layout-kind)
  (raw content-layout-raw)
  (source content-layout-source))

(define* (make-content-layout kind raw #:optional (source #f))
  (unless (symbol? kind)
    (error "content layout kind must be a symbol" kind))
  (unless (string? raw)
    (error "content layout raw value must be a string" raw))
  (%make-content-layout kind raw source))

(define-record-type <content-element>
  (%make-content-element kind children properties source flow-kind)
  content-element?
  (kind content-element-kind)
  (children content-element-children)
  (properties content-element-properties)
  (source content-element-source)
  (flow-kind content-element-flow-kind))

(define (normalize-child child)
  (if (string? child)
      (make-content-text child)
      child))

(define* (make-content-element kind children #:optional (source #f)
                               #:key (properties '())
                                     (flow-kind #f))
  (unless (symbol? kind)
    (error "content element kind must be a symbol" kind))
  (unless (valid-properties? properties)
    (error "content element properties must have unique symbol keys"
           properties))
  (let ((flow-kind (or flow-kind 'inline)))
  (unless (memq flow-kind '(inline block))
    (error "content element flow kind must be inline or block" flow-kind))
  (%make-content-element kind (normalize-children children 'element)
                         properties source flow-kind)))

(define (content-element-block? element)
  (eq? (content-element-flow-kind element) 'block))

(define (content-element-property element key)
  (assq-ref (content-element-properties element) key))

(define-record-type <content-document>
  (%make-content-document children source)
  content-document?
  (children content-document-children)
  (source content-document-source))

(define (content-child? value)
  (or (string? value)
      (content-text? value)
      (content-layout? value)
      (content-element? value)))

(define (normalize-children children who)
  (unless (list? children)
    (error "content children must be a proper list" who children))
  (let ((normalized (map normalize-child children)))
    (unless (every content-child? normalized)
      (error "content children must contain only content values"
             who children))
    normalized))

(define (valid-properties? properties)
  (and (list? properties)
       (every (lambda (entry)
                (and (pair? entry)
                     (symbol? (car entry))))
              properties)
       (let loop ((entries properties) (keys '()))
         (or (null? entries)
             (and (not (memq (caar entries) keys))
                  (loop (cdr entries) (cons (caar entries) keys)))))))

(define* (make-content-document children #:optional (source #f))
  (unless (list? children)
    (error "content children must be a proper list" 'document children))
  (let ((children
          (append-map (lambda (child)
                        (let ((normalized (normalize-child child)))
                          (if (content-document? normalized)
                              (content-document-children normalized)
                              (list normalized))))
                      children)))
    (unless (every content-child? children)
      (error "content children must contain only content values"
             'document children))
    (%make-content-document children source)))

(define (content-with-source-location content location)
  "Attach a source start alist with file, one-based line and column fields."
  (cond
    ((content-text? content)
     (%make-content-text (content-text-value content)
                         (or (content-text-source content)
                             (assq-ref location 'file))
                         location))
    ((content-element? content)
     (let* ((properties (content-element-properties content))
            (properties (filter (lambda (entry)
                                  (not (eq? (car entry) 'source-location)))
                                properties)))
       (%make-content-element
         (content-element-kind content)
         (content-element-children content)
         `((source-location . ,location) ,@properties)
         (content-element-source content)
         (content-element-flow-kind content))))
    ((content-document? content)
     (make-content-document (content-document-children content)
                           (content-document-source content)))
    ((and (list? content) (every content-child? content))
     (map (lambda (child) (content-with-source-location child location))
          content))
    (else content)))

(define (content-verbatim-string fragments)
  (string-concatenate
    (map (lambda (fragment)
           (cond
             ((string? fragment) fragment)
             ((content-text? fragment) (content-text-value fragment))
             ((content-layout? fragment) (content-layout-raw fragment))
             (else (error "not a content fragment" fragment))))
         fragments)))
