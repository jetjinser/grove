;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove ir)
  #:use-module (grove private snapshot)
  #:use-module (grove string-fun)
  #:use-module (ice-9 format)
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
            content-element-properties
            content-element-property
            content-element-style
            content-with-style
            content-with-property
            checked-anchor-id
            content-element-source
            element->plain-text
            content->plain-text
            make-content-disclosure
            content-document?
            make-content-document
            content-document-children
            content-document-source))

;; Content nodes are values: constructors snapshot caller-owned strings and
;; pair structure, and collection accessors return fresh structure. Nested
;; content and style records keep their own value semantics.

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

(define* (make-content-text value #:optional (source #f)
                            #:key (location #f))
  (unless (string? value)
    (error "content text must be a string" value))
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
    (%make-content-document (snapshot-data children)
                            (snapshot-data source))))

(define (content-with-source-location content location)
  "Attach a source start alist with file, one-based line and column fields."
  (cond
    ((content-text? content)
     (%make-content-text (content-text-value content)
                         (or (content-text-source content)
                             (assq-ref location 'file))
                         (snapshot-data location)))
    ((content-element? content)
     (let* ((properties (content-element-properties content))
            (properties (filter (lambda (entry)
                                  (not (eq? (car entry) 'source-location)))
                                properties)))
       (%make-content-element
         (content-element-kind content)
         (content-element-children content)
         `((source-location . ,(snapshot-data location)) ,@properties)
         (content-element-style content)
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

(define (element->plain-text kind properties parts)
  (case kind
    ((math-token)
     (let ((text (string-concatenate parts)))
       (if (and (eq? (assq-ref properties 'token-kind) 'mo)
                (member text '("+" "=" "×" "·" "→"
                               "≈" "<" ">" "≤" "≥")))
           (format #f " ~a " text)
           text)))
    ((math-frac)
     (format #f "(~a)/(~a)" (or (first parts) "")
             (or (second parts) "")))
    ((math-sup) (format #f "~a^(~a)" (or (first parts) "")
                        (or (second parts) "")))
    ((math-sub) (format #f "~a_(~a)" (or (first parts) "")
                        (or (second parts) "")))
    ((math-parens) (string-append "(" (string-concatenate parts) ")"))
    ((math-vector) (format #f "vec(~a)" (string-concatenate parts)))
    ((math-column) (string-join parts ", "))
    ((math-align) (string-join parts " ; "))
    (else (string-concatenate parts))))

(define (content->plain-text content)
  "Flatten CONTENT - a fragment, element, or document - to plain text:
strings pass through, text and layout unwrap their values, elements
recurse into their children with kind-specific joiners."
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content)
     (element->plain-text
       (content-element-kind content)
       (content-element-properties content)
       (map content->plain-text (content-element-children content))))
    ((content-document? content)
     (string-concatenate (map content->plain-text
                              (content-document-children content))))
    (else "")))
