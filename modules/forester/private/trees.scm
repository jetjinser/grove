;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private trees)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove private string-fun)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (forester input scribble)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (walk-content
            element-kind?
            link-elements
            query-link-elements
            math-elements
            subtree-elements
            direct-subtree-elements
            declared-anchor-elements
            content-source-paths
            transclusion-elements
            owned-content
            subtree-pages
            assign-subtree-identifiers
            assign-unique-occurrence-anchors
            assign-equation-numbers))

(define* (walk-content content select? #:key (postorder? #f) (stop? (const #f)))
  (define (walk node)
    (let* ((element? (content-element? node))
           (children
             (cond
               ((and element? (not (stop? node)))
                (ir:content-element-children/raw node))
               ((content-document? node)
                (ir:content-document-children/raw node))
               (else '())))
           (nested (append-map walk children))
           (selected (if (select? node) (list node) '())))
      (if postorder? (append nested selected) (append selected nested))))
  (walk content))

(define (element-kind? kind)
  (lambda (content)
    (and (content-element? content)
         (eq? kind (content-element-kind content)))))

(define (link-elements content)
  (walk-content content (element-kind? 'link) #:postorder? #t))

(define (query-link-elements content)
  (walk-content content (element-kind? 'query-link) #:postorder? #t))

(define (math-elements content)
  (walk-content content (element-kind? 'math) #:postorder? #t))

(define (subtree-elements content)
  (walk-content content (element-kind? 'subtree)))

(define (direct-subtree-elements content)
  (walk-content content (element-kind? 'subtree)
    #:stop? (element-kind? 'subtree)))

(define (content-source-paths content)
  (filter-map (lambda (node)
                (and (content-text? node) (ir:content-text-source/raw node)))
    (walk-content content content-text?)))

(define (transclusion-elements content)
  (walk-content content (element-kind? 'transclude) #:postorder? #t))

(define (declared-anchor-elements content)
  ;;     Ordinary elements declaring an `anchor' property - a term
  ;;     definition, a figure.  Tree boundaries stop the walk: their
  ;;     interiors are other trees with anchors of their own.
  (walk-content content
    (lambda (element)
      (and (content-element? element)
           (not (memq (content-element-kind element)
                      '(subtree transclude forest-query)))
           (ir:content-element-property/raw element 'anchor)))
    #:stop? (element-kind? 'subtree)))

(define (owned-content content)
  (cond
    ((content-element? content)
     (if (memq (content-element-kind content) '(subtree forest-query))
         #f
         (content-element-with-children
           content
           (filter-map owned-content (ir:content-element-children/raw content)))))
    ((content-document? content)
     (make-content-document
       (filter-map owned-content (ir:content-document-children/raw content))
       (ir:content-document-source/raw content)))
    (else content)))

(define (subtree-page element source)
  (let ((id (ir:content-element-property/raw element 'tree-id))
        (title (ir:content-element-property/raw element 'title)))
    (unless (and (non-empty-string? id)
                 (non-empty-string? title))
      (error "invalid Forester subtree element" element))
    (document->tree-page
      id (make-content-document (ir:content-element-children/raw element) source) source
      #:title title
      #:metadata `((inline-subtree . #t)
                   ,@(let ((location (ir:content-element-property/raw element 'source-location)))
                       (if location `((source-location . ,location)) '()))))))

(define (subtree-pages pages)
  (define (descendants page)
    (append-map
      (lambda (element)
        (let ((child
                (subtree-page element (site-page-source page))))
          (cons child (descendants child))))
      (direct-subtree-elements (site-page-document page))))
  (append-map descendants pages))

(define (assign-subtree-identifiers pages)
  (let ((next-id 0))
    (define (transform content)
      (cond
        ((content-element? content)
         (let* ((addressable-disclosure?
                  (and (eq? (content-element-kind content) 'disclosure)
                       (ir:content-element-property/raw content 'addressable?)))
                (subtree? (or (eq? (content-element-kind content) 'subtree)
                              addressable-disclosure?))
                (kind (if addressable-disclosure? 'subtree
                          (content-element-kind content)))
                (explicit-id
                  (and subtree?
                       (ir:content-element-property/raw content 'tree-id)))
                (id (and subtree?
                         (or explicit-id
                         (begin
                           (let ((id next-id))
                             (set! next-id (1+ next-id))
                             (format #f "unstable-~a" id))))))
                (children (map transform (ir:content-element-children/raw content)))
                (properties
                  (filter (lambda (entry)
                            (not (memq (car entry)
                                       '(tree-id addressable?))))
                          (ir:content-element-properties/raw content))))
           (make-content-element
             kind children
             (ir:content-element-source/raw content)
             #:properties (if id
                              (cons (cons 'tree-id id) properties)
                              properties)
             #:style (content-element-style content)
             #:flow-kind (content-element-flow-kind content))))
        ((content-document? content)
         (make-content-document
           (map transform (ir:content-document-children/raw content))
           (ir:content-document-source/raw content)))
        (else content)))
    (map (lambda (page)
           (make-site-page
             (site-page-id page)
             (site-page-title page)
             (transform (site-page-document page))
             (site-page-source page)
             #:metadata (site-page-metadata page)))
         pages)))

(define (assign-unique-occurrence-anchors pages)
  "Anchor every tree occurrence that is unique within its page with the
tree's own ID. Elements carrying an explicit anchor keep it; repeated
occurrences stay unanchored, so links aimed at an anchor retired this way
surface as fragment warnings. Unnamed subtrees anchor under their unstable-N
IDs and inherit those IDs' instability. Transcluded content is out of scope:
a tree brought in by another tree's transclusion repeats anchors in the host
DOM without being counted, the same global-anchor limitation the uniqueness
check documents."
  (define (occurrence-id element)
    (ir:content-element-property/raw element
      (if (eq? (content-element-kind element) 'subtree)
          'tree-id
          'target)))
  (define (rewrite-document document counts)
    (let next ((content document))
      (cond
        ((content-element? content)
         (let* ((children (map next (ir:content-element-children/raw content)))
                (kind (content-element-kind content))
                (id (and (memq kind '(subtree transclude))
                         (occurrence-id content)))
                (anchor
                  (and id
                       (= 1 (hash-ref counts id 0))
                       (not (ir:content-element-property/raw content 'anchor))
                       id))
                (properties
                  (if anchor
                      (cons (cons 'anchor anchor)
                            (ir:content-element-properties/raw content))
                      (ir:content-element-properties/raw content))))
           (make-content-element
             kind children
             (ir:content-element-source/raw content)
             #:properties properties
             #:style (content-element-style content)
             #:flow-kind (content-element-flow-kind content))))
        ((content-document? content)
         (make-content-document
           (map next (ir:content-document-children/raw content))
           (ir:content-document-source/raw content)))
        (else content))))
  (map
    (lambda (page)
      (let* ((document (site-page-document page))
             (counts (make-hash-table)))
        (for-each
          (lambda (element)
            (let ((id (occurrence-id element)))
              (when id
                (hash-set! counts id (1+ (hash-ref counts id 0))))))
          (append (subtree-elements document)
                  (transclusion-elements document)))
        (make-site-page
          (site-page-id page)
          (site-page-title page)
          (rewrite-document document counts)
          (site-page-source page)
          #:metadata (site-page-metadata page))))
    pages))

(define (assign-equation-numbers pages)
  "Number every display equation in each page's own content, in document
order. All display equations count, named or not; the number lands on
equations the author did not tag, and a generated unstable-eq-N ID lands on
equations the author did not name, joining the anchor registry like any
other math ID. The chapter prefix is the page's declared chapter, never
invented here."
  (define (rewrite page)
    (let* ((document (site-page-document page))
           (chapter (assq-ref (site-page-metadata page) 'chapter))
           (counter 0))
      (define (number-string)
        (if chapter
            (format #f "~a.~a" chapter counter)
            (format #f "~a" counter)))
      (let next ((content document) (nested? #f))
        (cond
          ((content-document? content)
           (make-content-document
             (map (lambda (child) (next child nested?))
                  (ir:content-document-children/raw content))
             (ir:content-document-source/raw content)))
          ((not (content-element? content)) content)
          ;; Equations inside subtrees belong to the subtrees' own pages.
          ((or nested?
               (memq (content-element-kind content) '(subtree forest-query)))
           content)
          ((and (eq? (content-element-kind content) 'math)
                (ir:content-element-property/raw content 'display?))
           (set! counter (1+ counter))
           (let* ((id (or (ir:content-element-property/raw content 'id)
                          (format #f "unstable-eq-~a" counter)))
                  ;;     \tag is a TeX notion; MathML equations always number.
                  (tagged? (and (ir:content-element-property/raw content 'tex?)
                                (string-contains
                                  (content-verbatim-string
                                    (ir:content-element-children/raw content))
                                  "\\tag")))
                  (properties
                    (cons (cons 'id id)
                          (if tagged?
                              (filter (lambda (entry)
                                        (not (eq? (car entry) 'id)))
                                      (ir:content-element-properties/raw content))
                              (cons (cons 'number (number-string))
                                    (filter (lambda (entry)
                                              (not (eq? (car entry) 'id)))
                                            (ir:content-element-properties/raw content)))))))
             (make-content-element
               'math (ir:content-element-children/raw content)
               (ir:content-element-source/raw content)
               #:properties properties
               #:style (content-element-style content)
               #:flow-kind (content-element-flow-kind content))))
          (else
           (content-element-with-children
             content
             (map (lambda (child) (next child nested?))
                  (ir:content-element-children/raw content))))))))
  (map (lambda (page)
         (make-site-page
           (site-page-id page)
           (site-page-title page)
           (rewrite page)
           (site-page-source page)
           #:metadata (site-page-metadata page)))
       pages))
