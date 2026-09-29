;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (forest?
            make-forest
            site
            pages
            topic-relation
            topic-pages
            find-page
            links
            backlinks
            common-backlinks
            reachable-pages
            page-id-from-link
            fragment-from-link
            heading-ids
            reference?
            reference-source-page-id
            reference-source-id
            reference-target-page-id
            reference-target-fragment
            reference-label
            heading?
            heading-id
            heading-label
            headings
            outgoing-references
            incoming-references
            subtree-definitions
            outgoing-transclusions
            incoming-transclusions
            transcluding-pages
            source-paths
            module-paths
            page-source-paths))

(define (link-elements content)
  (cond
    ((content-element? content)
     (append (append-map link-elements
                         (content-element-children content))
             (if (eq? (content-element-kind content) 'link)
                 (list content)
                 '())))
    ((content-document? content)
     (append-map link-elements (content-document-children content)))
    (else '())))

(define (subtree-elements content)
  (cond
    ((content-element? content)
     (append (if (eq? (content-element-kind content) 'subtree)
                 (list content)
                 '())
             (append-map subtree-elements
                         (content-element-children content))))
    ((content-document? content)
     (append-map subtree-elements (content-document-children content)))
    (else '())))

(define (direct-subtree-elements content)
  (cond
    ((content-element? content)
     (if (eq? (content-element-kind content) 'subtree)
         (list content)
         (append-map direct-subtree-elements
                     (content-element-children content))))
    ((content-document? content)
     (append-map direct-subtree-elements (content-document-children content)))
    (else '())))

(define (transclusion-elements content)
  (cond
    ((content-element? content)
     (append (append-map transclusion-elements
                         (content-element-children content))
             (if (eq? (content-element-kind content) 'transclude)
                 (list content)
                 '())))
    ((content-document? content)
     (append-map transclusion-elements (content-document-children content)))
    (else '())))

(define (describe-tree-cycle pages cycle)
  (string-join
    (map (lambda (tree-id)
           (let ((page (find (lambda (candidate)
                               (equal? tree-id (site-page-id candidate)))
                             pages)))
             (format #f "~a (~a)" tree-id
                     (or (and page (site-page-source page))
                         "<unknown source>"))))
         cycle)
    " -> "))

(define (validate-composition-cycles! pages composition)
  (let ((edges
          (map (lambda (page)
                 (cons (site-page-id page)
                       (composition:target-ids
                         composition (site-page-id page))))
               pages)))
    (letrec ((visit
               (lambda (id path)
                 (when (member id path equal?)
                   (error
                     (format #f "cyclic Forester tree composition: ~a"
                             (describe-tree-cycle
                               pages (reverse (cons id path))))))
                 (for-each (lambda (target)
                             (visit target (cons id path)))
                           (cdr (assoc id edges))))))
      (for-each (lambda (page) (visit (site-page-id page) '())) pages))))

(define (owned-content content)
  (cond
    ((content-element? content)
     (if (eq? (content-element-kind content) 'subtree)
         #f
         (make-content-element
           (content-element-kind content)
           (filter-map owned-content (content-element-children content))
           (content-element-source content)
           #:properties (content-element-properties content))))
    ((content-document? content)
     (make-content-document
       (filter-map owned-content (content-document-children content))
       (content-document-source content)))
    (else content)))

(define (subtree-page element source)
  (let ((id (content-element-property element 'tree-id))
        (title (content-element-property element 'title)))
    (unless (and (string? id) (not (string-null? id))
                 (string? title) (not (string-null? title)))
      (error "invalid Forester subtree element" element))
    (make-site-page
      id title
      (make-content-document (content-element-children element) source)
      source
      #:metadata '((inline-subtree . #t)))))

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
         (let* ((subtree? (eq? (content-element-kind content) 'subtree))
                (explicit-id
                  (and subtree?
                       (content-element-property content 'tree-id)))
                (id (and subtree?
                         (or explicit-id
                         (begin
                           (let ((id next-id))
                             (set! next-id (1+ next-id))
                             (format #f "unstable-~d" id))))))
                (children (map transform (content-element-children content)))
                (properties
                  (filter (lambda (entry) (not (eq? (car entry) 'tree-id)))
                          (content-element-properties content))))
           (make-content-element
             (content-element-kind content) children
             (content-element-source content)
             #:properties (if id
                              (cons (cons 'tree-id id) properties)
                              properties))))
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

(define (split-link-target target)
  (unless (and (string? target) (not (string-null? target)))
    (error "Forester link target must be a non-empty string" target))
  (let ((separator (string-index target #\#)))
    (if separator
        (let ((page-id (substring target 0 separator))
              (fragment (substring target (+ separator 1))))
          (when (or (string-null? page-id)
                    (string-null? fragment)
                    (string-index fragment #\#))
            (error "invalid Forester page fragment target" target))
          (cons page-id fragment))
        (cons target #f))))

(define (page-id-from-link target)
  (car (split-link-target target)))

(define (fragment-from-link target)
  (cdr (split-link-target target)))

(define (content->plain-text content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content)
     (string-concatenate (map content->plain-text
                              (content-element-children content))))
    ((content-document? content)
     (string-concatenate (map content->plain-text
                              (content-document-children content))))
    (else "")))

(define (heading-elements content)
  (cond
    ((content-element? content)
     (append (append-map heading-elements
                         (content-element-children content))
             (if (eq? (content-element-kind content) 'heading)
                 (list content)
                 '())))
    ((content-document? content)
     (append-map heading-elements (content-document-children content)))
    (else '())))

(define (heading-slug heading)
  (let loop ((characters (string->list (content->plain-text heading)))
             (result '())
             (separator? #f))
    (if (null? characters)
        (let ((slug (list->string (reverse result))))
          (if (string-null? slug) "section" slug))
        (let ((character (car characters)))
          (if (or (char-alphabetic? character)
                  (char-numeric? character))
              (loop (cdr characters)
                    (cons (char-downcase character)
                          (if (and separator? (pair? result))
                              (cons #\- result)
                              result))
                    #f)
              (loop (cdr characters) result (pair? result)))))))

(define (unique-heading-slugs headings)
  (let loop ((headings headings) (used '()) (result '()))
    (if (null? headings)
        (reverse result)
        (let* ((base (heading-slug (car headings)))
               (id (let find-id ((suffix 1))
                     (let ((candidate
                             (if (= suffix 1)
                                 base
                                 (format #f "~a-~a" base suffix))))
                       (if (member candidate used string=?)
                           (find-id (+ suffix 1))
                           candidate)))))
          (loop (cdr headings) (cons id used) (cons id result))))))

(define-record-type <reference>
  (%make-reference source-page-id source-id target-page-id
                          target-fragment label)
  reference?
  (source-page-id reference-source-page-id)
  (source-id reference-source-id)
  (target-page-id reference-target-page-id)
  (target-fragment reference-target-fragment)
  (label reference-label))

(define-record-type <heading>
  (%make-heading id label)
  heading?
  (id heading-id)
  (label heading-label))

(define-record-type <forest>
  (%make-forest site pages reference-index heading-index reference-relation
                composition topic-relation)
  forest?
  (site site)
  (pages pages)
  (reference-index reference-index)
  (heading-index heading-index)
  (reference-relation reference-relation)
  (composition composition)
  (topic-relation topic-relation))

(define (page-topics page)
  (let ((taxon (assq-ref (site-page-metadata page) 'taxon)))
    (append (if taxon (list (cons 'taxon taxon)) '())
            (map (lambda (tag) (cons 'tag tag))
                 (or (assq-ref (site-page-metadata page) 'tags) '())))))

(define* (topic-pages forest topic-id #:optional kind)
  (let ((page-ids
          (topic-page-ids (topic-relation forest) topic-id kind)))
    (filter (lambda (page)
              (member (site-page-id page) page-ids string=?))
            (pages forest))))

(define (make-page-topic-rows pages)
  (let ((rows
          (append-map
            (lambda (page)
              (map (lambda (entry)
                     (let ((kind (car entry))
                           (id (cadr entry))
                           (label (cddr entry)))
                       (unless (and (string? id) (not (string-null? id))
                                    (string? label) (not (string-null? label)))
                         (error "invalid topic metadata on page"
                                (site-page-id page) entry))
                       (list kind id label (site-page-id page))))
                   (page-topics page)))
            pages)))
    (for-each
      (lambda (row)
        (let ((same-id (find (lambda (candidate)
                               (string=? (cadr candidate) (cadr row)))
                             rows)))
          (unless (string=? (caddr row) (caddr same-id))
            (error "inconsistent labels for Forester topic"
                   (cadr row) (caddr same-id) (caddr row)))))
      rows)
    (make-topic-relation rows)))

(define (make-forest site)
  (unless (site? site)
    (error "forest requires a Grove site" site))
  (let* ((root-pages (assign-subtree-identifiers (site-pages site)))
         (subtrees (subtree-pages root-pages))
         (pages (append root-pages subtrees))
         (_ (make-site pages
                       #:title (site-title site)
                       #:description (site-description site)
                       #:sources (site-sources site)))
         (heading-index
           (map (lambda (page)
                  (let* ((elements
                           (heading-elements
                             (owned-content (site-page-document page))))
                         (ids (unique-heading-slugs elements)))
                    (cons
                      (site-page-id page)
                      (map (lambda (element id)
                             (%make-heading
                               id (content->plain-text element)))
                           elements ids))))
                pages))
         (reference-index
           (map
             (lambda (source-page)
               (let* ((source-id (site-page-id source-page))
                      (elements
                        (link-elements
                          (owned-content
                            (site-page-document source-page)))))
                 (cons
                   source-id
                   (map
                     (lambda (element index)
                       (let* ((target
                                (content-element-property element 'target))
                              (target-page-id
                                (page-id-from-link target))
                              (fragment (fragment-from-link target))
                              (target-page
                                (find
                                  (lambda (page)
                                    (equal? target-page-id
                                            (site-page-id page)))
                                  pages)))
                         (unless target-page
                           (error
                             (format #f
                               "unresolved Forester link in page ~a (~a): ~a"
                               source-id
                               (or (site-page-source source-page)
                                   "<unknown source>")
                               target)))
                         (when (and fragment
                                    (not (member
                                           fragment
                                           (map heading-id
                                                (cdr (assoc target-page-id
                                                            heading-index)))
                                           string=?)))
                           (error
                             (format #f
                               "unresolved Forester fragment in page ~a (~a): ~a"
                               source-id
                               (or (site-page-source source-page)
                                   "<unknown source>")
                               target)))
                         (%make-reference
                           source-id
                           (format #f "ref-~a" index)
                           target-page-id
                           fragment
                           (content->plain-text element))))
                     elements
                     (iota (length elements) 1)))))
             pages))
         (transclusion-index
           (map
             (lambda (source-tree)
               (let* ((source-id (site-page-id source-tree))
                      (elements
                        (transclusion-elements
                          (owned-content
                            (site-page-document source-tree)))))
                 (cons
                   source-id
                   (map
                     (lambda (element index)
                       (let* ((target-id
                                (content-element-property element 'target))
                              (transclusion-id
                                (format #f "transclude-~a" index))
                              (source-path
                                (or (site-page-source source-tree)
                                    "<unknown source>")))
                         (unless (and (string? target-id)
                                      (not (string-null? target-id)))
                           (error
                             (format #f
                               "invalid Forester transclusion ~a in tree ~a (~a): ~s"
                               transclusion-id source-id source-path target-id)))
                         (unless (find
                                   (lambda (page)
                                     (equal? target-id (site-page-id page)))
                                   pages)
                           (error
                             (format #f
                               "unresolved Forester transclusion ~a in tree ~a (~a): target ~a"
                               transclusion-id source-id source-path target-id)))
                         (composition:make-transclusion
                           source-id transclusion-id target-id)))
                     elements
                     (iota (length elements) 1)))))
             pages))
         (reference-relation
           (make-reference-relation
             (append-map
               (lambda (entry)
                 (map (lambda (reference)
                        (list
                          (reference-source-page-id reference)
                          (reference-source-id reference)
                          (reference-target-page-id reference)
                          (reference-target-fragment reference)
                          (reference-label reference)))
                      (cdr entry)))
               reference-index)))
         (topic-relation (make-page-topic-rows pages))
         (composition
           (composition:make-composition
             (append-map
               (lambda (page)
                 (map (lambda (element)
                        (composition:make-subtree-definition
                          (site-page-id page)
                          (content-element-property element 'tree-id)))
                      (direct-subtree-elements
                        (site-page-document page))))
               pages)
             (append-map cdr transclusion-index)))
         (_ (validate-composition-cycles! pages composition)))
    (%make-forest site pages reference-index heading-index
                  reference-relation composition topic-relation)))

(define (find-page forest id)
  (or (find (lambda (page) (equal? id (site-page-id page)))
            (pages forest))
      (error "unknown forest page" id)))

(define (subtree-definitions forest)
  (map (lambda (definition)
       (list (composition:subtree-definition-parent definition)
               (composition:subtree-definition-child definition)))
       (composition:subtree-definitions
         (composition forest))))

(define (heading-ids forest id)
  (map heading-id (headings forest id)))

(define (headings forest id)
  (let ((entry (assoc id (heading-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (cdr entry)))

(define (links forest id)
  (map (lambda (reference)
         (let ((fragment (reference-target-fragment reference)))
           (string-append (reference-target-page-id reference)
                          (if fragment (string-append "#" fragment) ""))))
       (outgoing-references forest id)))

(define (outgoing-references forest id)
  (let ((entry (assoc id (reference-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (let ((reference-ids
            (outgoing-reference-ids
              (reference-relation forest) id)))
      (filter (lambda (reference)
                (member (reference-source-id reference)
                        reference-ids equal?))
              (cdr entry)))))

(define (outgoing-transclusions forest id)
  (unless (assoc id (heading-index forest))
    (error "unknown forest page" id))
  (composition:outgoing-transclusions
    (composition forest) id))

(define (incoming-transclusions forest id)
  (unless (assoc id (heading-index forest))
    (error "unknown forest page" id))
  (composition:incoming-transclusions
    (composition forest) id))

(define (transcluding-pages forest id)
  (let ((source-ids
          (delete-duplicates
            (map composition:transclusion-source-tree-id
                 (incoming-transclusions forest id))
            equal?)))
    (filter (lambda (page)
              (member (site-page-id page) source-ids equal?))
            (pages forest))))

(define* (incoming-references forest id #:optional fragment)
  (unless (assoc id (heading-index forest))
    (error "unknown forest page" id))
  (when (and fragment
             (not (member fragment (heading-ids forest id) string=?)))
    (error "unknown forest heading fragment" id fragment))
  (let* ((keys (incoming-reference-keys
                 (reference-relation forest) id fragment))
         (key? (lambda (reference)
                 (member (list (reference-source-page-id reference)
                               (reference-source-id reference))
                         keys equal?))))
    (filter key? (append-map cdr (reference-index forest)))))

(define (backlinks forest id)
  (unless (assoc id (heading-index forest))
    (error "unknown forest page" id))
  (let ((source-ids
          (backlink-page-ids (reference-relation forest) id)))
    (filter (lambda (page) (member (site-page-id page) source-ids equal?))
            (pages forest))))

(define (common-backlinks forest left-id right-id)
  (for-each
    (lambda (id)
      (unless (assoc id (heading-index forest))
        (error "unknown forest page" id)))
    (list left-id right-id))
  (let ((source-ids
          (common-backlink-page-ids
            (reference-relation forest) left-id right-id)))
    (filter (lambda (page) (member (site-page-id page) source-ids equal?))
            (pages forest))))

(define (reachable-pages forest id)
  (unless (assoc id (heading-index forest))
    (error "unknown forest page" id))
  (let ((reachable-ids
          (reachable-page-ids (reference-relation forest) id)))
    (filter (lambda (page)
              (member (site-page-id page) reachable-ids equal?))
            (pages forest))))

(define (source-paths forest)
  (let ((site (site forest)))
    (delete-duplicates
      (append (site-sources site)
              (filter-map site-page-source (site-pages site)))
      string=?)))

(define (module-paths forest)
  (let* ((site (site forest))
         (page-paths (filter-map site-page-source (site-pages site))))
    (filter (lambda (path) (not (member path page-paths string=?)))
            (site-sources site))))

(define (included-tree-ids forest start-id)
  (let loop ((pending (list start-id)) (seen '()))
    (if (null? pending)
        seen
        (let ((id (car pending)))
          (if (member id seen equal?)
              (loop (cdr pending) seen)
              (let ((page (find-page forest id)))
                (loop
                  (append
            (map (lambda (element)
                   (content-element-property element 'target))
                 (transclusion-elements
                   (site-page-document (find-page forest id))))
                    (map (lambda (element)
                           (content-element-property element 'tree-id))
                         (subtree-elements (site-page-document page)))
                    (cdr pending))
                  (cons id seen))))))))

(define (page-source-paths forest page)
  (delete-duplicates
    (append (module-paths forest)
            (filter-map
              (lambda (id)
                (site-page-source (find-page forest id)))
              (included-tree-ids forest (site-page-id page)))
            (filter-map site-page-source
                        (backlinks forest (site-page-id page))))
    string=?))
