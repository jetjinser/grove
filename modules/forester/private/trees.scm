;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private trees)
  #:use-module (grove string-fun)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (forester input scribble)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (walk-content
            element-kind?
            link-elements
            subtree-elements
            direct-subtree-elements
            content-source-paths
            transclusion-elements
            owned-content
            subtree-pages
            assign-subtree-identifiers))

(define* (walk-content content select? #:key (postorder? #f) (stop? (const #f)))
  (define (walk node)
    (let* ((element? (content-element? node))
           (children
             (cond
               ((and element? (not (stop? node)))
                (content-element-children node))
               ((content-document? node)
                (content-document-children node))
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

(define (subtree-elements content)
  (walk-content content (element-kind? 'subtree)))

(define (direct-subtree-elements content)
  (walk-content content (element-kind? 'subtree)
    #:stop? (element-kind? 'subtree)))

(define (content-source-paths content)
  (filter-map (lambda (node)
                (and (content-text? node) (content-text-source node)))
    (walk-content content content-text?)))

(define (transclusion-elements content)
  (walk-content content (element-kind? 'transclude) #:postorder? #t))

(define (owned-content content)
  (cond
    ((content-element? content)
     (if (memq (content-element-kind content) '(subtree forest-query))
         #f
         (make-content-element
           (content-element-kind content)
           (filter-map owned-content (content-element-children content))
           (content-element-source content)
           #:properties (content-element-properties content)
           #:style (content-element-style content)
           #:flow-kind (content-element-flow-kind content))))
    ((content-document? content)
     (make-content-document
       (filter-map owned-content (content-document-children content))
       (content-document-source content)))
    (else content)))

(define (subtree-page element source)
  (let ((id (content-element-property element 'tree-id))
        (title (content-element-property element 'title)))
    (unless (and (non-empty-string? id)
                 (non-empty-string? title))
      (error "invalid Forester subtree element" element))
    (document->tree-page
      id (make-content-document (content-element-children element) source) source
      #:title title
      #:metadata `((inline-subtree . #t)
                   ,@(let ((location (content-element-property element 'source-location)))
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
                       (content-element-property content 'addressable?)))
                (subtree? (or (eq? (content-element-kind content) 'subtree)
                              addressable-disclosure?))
                (kind (if addressable-disclosure? 'subtree
                          (content-element-kind content)))
                (explicit-id
                  (and subtree?
                       (content-element-property content 'tree-id)))
                (id (and subtree?
                         (or explicit-id
                         (begin
                           (let ((id next-id))
                             (set! next-id (1+ next-id))
                             (format #f "unstable-~a" id))))))
                (children (map transform (content-element-children content)))
                (properties
                  (filter (lambda (entry)
                            (not (memq (car entry)
                                       '(tree-id addressable?))))
                          (content-element-properties content))))
           (make-content-element
             kind children
             (content-element-source content)
             #:properties (if id
                              (cons (cons 'tree-id id) properties)
                              properties)
             #:style (content-element-style content)
             #:flow-kind (content-element-flow-kind content))))
        ((content-document? content)
         (make-content-document
           (map transform (content-document-children content))
           (content-document-source content)))
        (else content)))
    (map (lambda (page)
           (make-site-page
             (site-page-id page)
             (site-page-title page)
             (transform (site-page-document page))
             (site-page-source page)
             #:metadata (site-page-metadata page)))
         pages)))
