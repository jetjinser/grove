;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove ir)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (grove style)
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
            content-element-with-children
            content-element-properties
            content-element-property
            content-element-style
            content-with-style
            content-with-property
            checked-anchor-id
            content-element-source
            make-content-disclosure
            content-document?
            make-content-document
            content-document-children
            content-document-source
            content-source-paths))

;; Content nodes are values: constructors snapshot caller-owned strings,
;; bytevectors, and pair structure; collection accessors return fresh
;; structure. Nested content and style records keep their own value semantics.

(define-record-type <content-text>
  (%make-content-text value source location)
  content-text?
  (value content-text-value/raw)
  (source content-text-source/raw)
  (location content-text-location/raw))

(define (content-text-value content)
  (snapshot-data (content-text-value/raw content)))

(define (content-text-source content)
  (snapshot-data (content-text-source/raw content)))

(define (content-text-location content)
  (snapshot-data (content-text-location/raw content)))

(define (valid-content-source? source)
  (or (not source) (non-empty-string? source)))

(define (check-content-source who source)
  (unless (valid-content-source? source)
    (error "content source must be a non-empty path or #f" who source)))

(define (valid-source-location? location)
  (and (list? location)
       (every (lambda (entry)
                (and (pair? entry) (symbol? (car entry))))
              location)
       (let loop ((entries location) (keys '()))
         (or (null? entries)
             (and (not (memq (caar entries) keys))
                  (loop (cdr entries) (cons (caar entries) keys)))))
       (let ((file (assq 'file location))
             (line (assq 'line location))
             (column (assq 'column location)))
         (and (or (not file) (non-empty-string? (cdr file)))
              (or (not line)
                  (and (integer? (cdr line)) (positive? (cdr line))))
              (or (not column)
                  (and (integer? (cdr column)) (positive? (cdr column))))))))

(define* (make-content-text value #:optional (source #f)
                            #:key (location #f))
  (unless (string? value)
    (error "content text must be a string" value))
  (check-content-source 'text source)
  (unless (or (not location) (valid-source-location? location))
    (error "content text location must be a source-location alist" location))
  (%make-content-text (snapshot-data value)
                      (snapshot-data source)
                      (snapshot-data location)))

(define-record-type <content-layout>
  (%make-content-layout kind raw source)
  content-layout?
  (kind content-layout-kind)
  (raw content-layout-raw/raw)
  (source content-layout-source/raw))

(define (content-layout-raw content)
  (snapshot-data (content-layout-raw/raw content)))

(define (content-layout-source content)
  (snapshot-data (content-layout-source/raw content)))

(define* (make-content-layout kind raw #:optional (source #f))
  (unless (symbol? kind)
    (error "content layout kind must be a symbol" kind))
  (unless (string? raw)
    (error "content layout raw value must be a string" raw))
  (check-content-source 'layout source)
  (%make-content-layout kind (snapshot-data raw) (snapshot-data source)))

(define-record-type <content-element>
  (%make-content-element kind children properties style source flow-kind)
  content-element?
  (kind content-element-kind)
  (children content-element-children/raw)
  (properties content-element-properties/raw)
  (style content-element-style)
  (source content-element-source/raw)
  (flow-kind content-element-flow-kind))

(define (content-element-children element)
  (snapshot-data (content-element-children/raw element)))

(define (content-element-with-children element children)
  "Return ELEMENT with CHILDREN, preserving its kind and other properties."
  (unless (content-element? element)
    (error "content-element-with-children requires a content element" element))
  (make-content-element
    (content-element-kind element) children
    (content-element-source/raw element)
    #:properties (content-element-properties/raw element)
    #:style (content-element-style element)
    #:flow-kind (content-element-flow-kind element)))

(define (content-element-properties element)
  (snapshot-data (content-element-properties/raw element)))

(define (content-element-source element)
  (snapshot-data (content-element-source/raw element)))

(define (normalize-child child)
  (if (string? child)
      (make-content-text child)
      child))

(define* (make-content-element kind children #:optional (source #f)
                               #:key (properties '())
                                     (style #f)
                                     (flow-kind #f))
  (unless (symbol? kind)
    (error "content element kind must be a symbol" kind))
  (check-content-source 'element source)
  (unless (valid-properties? properties)
    (error "content element properties must have unique symbol keys"
           properties))
  (set! style (normalize-style style))
  (let ((flow-kind (or flow-kind 'inline)))
  (unless (memq flow-kind '(inline block))
    (error "content element flow kind must be inline or block" flow-kind))
  (%make-content-element kind
                         (snapshot-data (normalize-children children 'element))
                         (snapshot-data properties) style
                         (snapshot-data source) flow-kind)))

(define (normalize-style style)
  (cond
    ((or (not style) (content-style? style)) style)
    ((or (symbol? style) (string? style)) (make-content-style style))
    (else (error "content element style must be a style name, content style, or #f" style))))

(define (content-with-style element style)
  "Set an element's presentation style, preserving its content and provenance."
  (unless (content-element? element)
    (error "content-with-style requires a content element" element))
  (let ((style (normalize-style style)))
    (if (eq? style (content-element-style element)) element
        (%make-content-element
          (content-element-kind element) (content-element-children element)
          (content-element-properties element) style
          (content-element-source element) (content-element-flow-kind element)))))

(define (content-element-block? element)
  (eq? (content-element-flow-kind element) 'block))

(define (content-element-property element key)
  (snapshot-data (content-element-property/raw element key)))

(define (content-element-property/raw element key)
  (assq-ref (content-element-properties/raw element) key))

(define (content-with-property element key value)
  "Return ELEMENT with property KEY replaced by VALUE, preserving everything else."
  (unless (content-element? element)
    (error "content-with-property requires a content element" element))
  (unless (symbol? key)
    (error "content property key must be a symbol" key))
  (when (eq? key 'source-location)
    (unless (valid-source-location? value)
      (error "content element source-location must be a source-location alist"
             value)))
  (%make-content-element
    (content-element-kind element) (content-element-children element)
    (cons (cons key (snapshot-data value))
          (filter (lambda (entry) (not (eq? (car entry) key)))
                  (content-element-properties element)))
    (content-element-style element)
    (content-element-source element) (content-element-flow-kind element)))

(define (checked-anchor-id id)
  "Validate an optional anchor ID: #f, or a non-empty string with no whitespace or #."
  (if (not id)
      #f
      (begin
        (unless (and (string? id)
                     (not (string-null? id))
                     (not (string-any char-whitespace? id))
                     (not (string-index id #\#)))
          (error "anchor ID must be a non-empty string without whitespace or #" id))
        id)))

(define* (make-content-disclosure title children #:key (open? #t)
                                  (addressable? #f) (style #f))
  "Make a titled disclosure. Addressable disclosures may become trees in a host site."
  (unless (non-empty-string? title)
    (error "content disclosure requires a non-empty title" title))
  (unless (boolean? open?)
    (error "content disclosure open? must be a boolean" open?))
  (unless (boolean? addressable?)
    (error "content disclosure addressable? must be a boolean" addressable?))
  (make-content-element 'disclosure children
    #:properties `((title . ,title) (open? . ,open?)
                   (addressable? . ,addressable?))
    #:style style
    #:flow-kind 'block))

(define-record-type <content-document>
  (%make-content-document children source)
  content-document?
  (children content-document-children/raw)
  (source content-document-source/raw))

(define (content-document-children document)
  (snapshot-data (content-document-children/raw document)))

(define (content-document-source document)
  (snapshot-data (content-document-source/raw document)))

(define (content-source-location-file element)
  (let ((location (content-element-property/raw element 'source-location)))
    (and (valid-source-location? location)
         (assq-ref location 'file))))

(define (content-source-paths content)
  "Return unique source paths recorded anywhere in CONTENT, in traversal order.

Text, layout, element, and document sources are included. File fields from
text locations and element source-location properties are included as well.
Raw strings have no source; a list may contain any proper sequence of content
values."
  (define (paths value)
    (cond
      ((string? value) '())
      ((content-text? value)
       (filter identity
         (list (content-text-source/raw value)
               (and (content-text-location/raw value)
                    (assq-ref (content-text-location/raw value) 'file)))))
      ((content-layout? value)
       (filter identity (list (content-layout-source/raw value))))
      ((content-element? value)
       (append
         (filter identity
           (list (content-element-source/raw value)
                 (content-source-location-file value)))
         (append-map paths (content-element-children/raw value))))
      ((content-document? value)
       (append
         (filter identity (list (content-document-source/raw value)))
         (append-map paths (content-document-children/raw value))))
      ((and (list? value) (every content-child? value))
       (append-map paths value))
      (else
       (error "content-source-paths requires content values" value))))
  (snapshot-data (delete-duplicates (paths content) string=?)))

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
                     (symbol? (car entry))
                     (or (not (eq? (car entry) 'source-location))
                         (valid-source-location? (cdr entry)))))
              properties)
       (let loop ((entries properties) (keys '()))
         (or (null? entries)
             (and (not (memq (caar entries) keys))
                  (loop (cdr entries) (cons (caar entries) keys)))))))

(define* (make-content-document children #:optional (source #f))
  (unless (list? children)
    (error "content children must be a proper list" 'document children))
  (check-content-source 'document source)
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
    (%make-content-document (snapshot-data children)
                            (snapshot-data source))))

(define (content-with-source-location content location)
  "Attach a source start alist with file, one-based line and column fields.
Raw strings become located text values. For a document, attach the location to
each top-level content value."
  (unless (valid-source-location? location)
    (error "content source location must be a source-location alist" location))
  (cond
    ((string? content)
     (make-content-text content #:location location))
    ((content-text? content)
     (%make-content-text (content-text-value content)
                         (or (content-text-source content)
                             (and location (assq-ref location 'file)))
                         (snapshot-data location)))
    ((content-element? content)
     (content-with-property content 'source-location location))
    ((content-document? content)
     (make-content-document
       (map (lambda (child) (content-with-source-location child location))
            (content-document-children content))
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
