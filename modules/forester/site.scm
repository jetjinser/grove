;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove string-fun)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module (forester input scribble)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (forest?
            make-forest
            site
            pages
            page-relation
            content-node-relation
            content-node
            topic-pages
            related-pages
            reference-pages
            find-page
            links
            tree-occurrences
            tree-occurrence-target
            backlinks
            common-backlinks
            reachable-pages
            reference?
            reference-source-page-id
            reference-source-id
            reference-target-page-id
            reference-label
            outgoing-references
            incoming-references
            subtree-definitions
            outgoing-transclusions
            incoming-transclusions
            transcluding-pages
            source-paths
            module-paths
            page-source-paths))

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
                             (format #f "unstable-~d" id))))))
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

(define-record-type <reference>
  (%make-reference source-page-id source-id target-page-id label)
  reference?
  (source-page-id reference-source-page-id)
  (source-id reference-source-id)
  (target-page-id reference-target-page-id)
  (label reference-label))

(define-record-type <forest>
  (%make-forest site pages reference-relation
                composition page-index page-relation content-node-relation
                content-node-index query-page-ids)
  forest?
  (site site)
  (pages pages)
  (reference-relation reference-relation)
  (composition composition)
  (page-index page-index)
  (page-relation page-relation)
  (content-node-relation content-node-relation)
  (content-node-index content-node-index)
  (query-page-ids query-page-ids))

(define* (topic-pages forest topic-id #:optional kind)
  (let ((page-ids
          (topic-page-ids (content-node-relation forest) topic-id kind)))
    (filter (lambda (page)
              (member (site-page-id page) page-ids string=?))
            (pages forest))))

(define* (related-pages forest page-id
                        #:optional (reference-taxon-id "Reference"))
  "Return direct link targets except pages in the reference taxon."
  (let ((reference-page-ids
          (map site-page-id
               (topic-pages forest reference-taxon-id 'taxon))))
    (map (lambda (target-id) (find-page forest target-id))
         (filter (lambda (target-id)
                   (not (member target-id reference-page-ids string=?)))
                 (delete-duplicates
                   (map reference-target-page-id
                        (outgoing-references forest page-id))
                   string=?)))))

(define* (reference-pages forest page-id
                          #:optional (reference-taxon-id "Reference"))
  "Return reference-taxon pages linked from this tree and its composition."
  (let ((reference-page-ids
          (map site-page-id
               (topic-pages forest reference-taxon-id 'taxon))))
    (map (lambda (target-id) (find-page forest target-id))
         (filter (lambda (target-id)
                   (member target-id reference-page-ids string=?))
                 (delete-duplicates
                   (append-map
                     (lambda (tree-id)
                       (map reference-target-page-id
                            (outgoing-references forest tree-id)))
                     (included-tree-ids forest page-id))
                   string=?)))))

(define (validate-topic-facts! facts)
  (let ((topics
          (filter-map
            (lambda (row)
              (and (memq (list-ref row 3) '(tag taxon))
                   (list (list-ref row 1)
                         (assq-ref (list-ref row 5) 'id)
                         (list-ref row 6))))
            facts)))
    (for-each
      (lambda (topic)
        (let ((id (cadr topic))
              (label (caddr topic)))
          (unless (and (non-empty-string? id)
                       (non-empty-string? label))
            (error "invalid topic IR element" topic))
          (let ((same-id
                  (find (lambda (candidate)
                          (string=? id (cadr candidate)))
                        topics)))
            (unless (string=? label (caddr same-id))
              (error "inconsistent labels for Forester topic"
                     id (caddr same-id) label)))))
      topics)))

(define (content-node-facts pages)
  (let ((index '())
        (next-id 0))
    ;; Each visit returns its preorder facts alongside its accumulated text.
    ;; Reusing child text avoids recursively flattening every subtree again.
    (letrec
      ((visit
         (lambda (page content parent-id position)
           (cond
             ((not (content-element? content))
              (cons '() (content->plain-text content)))
             ((eq? (content-element-kind content) 'forest-query)
              (cons '() ""))
             (else
              (let* ((id next-id)
                     (kind (content-element-kind content))
                     (properties (content-element-properties content))
                     (children (content-element-children content)))
                (set! next-id (1+ next-id))
                (set! index (cons (cons id content) index))
                (let* ((child-results
                         (if (eq? kind 'subtree)
                             '()
                             (map (lambda (child child-position)
                                    (visit page child id child-position))
                                  children
                                  (iota (length children)))))
                       (text (if (eq? kind 'subtree)
                                 (content->plain-text content)
                                 (element->plain-text
                                   kind properties
                                   (map cdr child-results)))))
                  (cons (cons
                          (list id (site-page-id page) parent-id kind position
                                properties text (string-length text)
                                (element-plain-text? content))
                          (append-map car child-results))
                        text))))))))
      (let ((facts
              (append-map
                (lambda (page)
                  (let ((children
                          (content-document-children
                            (site-page-document page))))
                    (append-map
                      car
                      (map (lambda (child position)
                             (visit page child #f position))
                           children
                           (iota (length children))))))
        pages)))
        (validate-topic-facts! facts)
        (let ((node-index (make-vector next-id #f)))
          (for-each (lambda (entry)
                      (vector-set! node-index (car entry) (cdr entry)))
                    index)
          (values (make-content-node-relation facts) node-index))))))

(define (query-in-content? content)
  (and (content-document? content)
       (any (lambda (child)
              (and (content-element? child)
                   (eq? (content-element-kind child) 'forest-query)))
            (content-document-children content))))

(define (nested-query-in-content? content)
  (cond
    ((content-element? content)
     (or (eq? (content-element-kind content) 'forest-query)
         (and (not (eq? (content-element-kind content) 'subtree))
              (any nested-query-in-content?
                   (content-element-children content)))))
    ((content-document? content)
     (any nested-query-in-content? (content-document-children content)))
    (else #f)))

(define (query-source-label page request)
  (let ((location (content-element-property request 'source-location)))
    (format #f "~a~a"
      (or (and (list? location) (assq-ref location 'file))
          (site-page-source page)
          (site-page-id page))
      (if (and (list? location) (assq-ref location 'line))
          (format #f ":~a:~a"
            (assq-ref location 'line)
            (or (assq-ref location 'column) 1))
          ""))))

(define (query-output-content forest page request)
  (let* ((query (content-element-property request 'query))
         (source (query-source-label page request))
         (result
           (catch #t
             (lambda () (query forest (site-page-id page)))
             (lambda (key . args)
               (error (format #f "Forester query failed at ~a: ~s"
                              source (cons key args)))))))
    (define (validate-query-link target)
      (catch #t
        (lambda ()
          (find-page forest target))
        (lambda (key . args)
          (error (format #f "Invalid query link at ~a to ~s: ~s"
                         source target (cons key args))))))
    (define (prepare content)
      (cond
        ((content-element? content)
         (let ((kind (content-element-kind content)))
           (when (memq kind '(forest-query subtree))
             (error (format #f
               "Forester query at ~a returned unsupported ~a content"
               source kind)))
           (let* ((kind (case kind
                          ((link) 'query-link)
                          ((transclude) 'query-transclude)
                          (else kind)))
                  (element
                    (make-content-element
                      kind (map prepare (content-element-children content))
                      (content-element-source content)
                      #:properties (content-element-properties content)
                      #:style (content-element-style content)
                      #:flow-kind (content-element-flow-kind content))))
             (when (memq kind '(query-link query-transclude))
               (validate-query-link
                 (content-element-property element 'target)))
             element)))
        (else content)))
    (unless (content-document? result)
      (error (format #f
        "Forester query at ~a must return a content document, got ~s"
        source result)))
    (make-content-document
      (map prepare (content-document-children result))
      (content-document-source result))))

(define (resolve-page-queries forest page)
  (define (resolve content)
    (if (and (content-element? content)
             (eq? (content-element-kind content) 'forest-query))
        (content-document-children
          (query-output-content forest page content))
        (list content)))
  (make-content-document
    (append-map resolve (content-document-children
                          (site-page-document page)))
    (content-document-source (site-page-document page))))

(define (validate-query-composition! forest resolved-pages)
  (let ((generated
          (append-map
            (lambda (page)
              (map (lambda (element index)
                     (composition:make-transclusion
                       (site-page-id page) (format #f "query-~a" index)
                       (content-element-property element 'target)))
                   (walk-content (owned-content (site-page-document page))
                                 (element-kind? 'query-transclude))
                   (iota (length (walk-content
                                   (owned-content (site-page-document page))
                                   (element-kind? 'query-transclude))))))
            resolved-pages)))
    (validate-composition-cycles!
      resolved-pages
      (composition:make-composition
        (composition:subtree-definitions (composition forest))
        (append generated
                (append-map (lambda (page)
                              (outgoing-transclusions forest (site-page-id page)))
                            resolved-pages))))))

(define (tree-occurrences forest id)
  "Return direct tree displays in body order, stopping at tree boundaries."
  (define (tree-display? node)
    (and (content-element? node)
         (memq (content-element-kind node) '(subtree transclude query-transclude))))
  (walk-content (site-page-document (find-page forest id))
                tree-display? #:stop? tree-display?))

(define (tree-occurrence-target element)
  (content-element-property element
    (if (eq? (content-element-kind element) 'subtree) 'tree-id 'target)))

(define (resolve-forest-queries forest)
  (for-each
    (lambda (page)
      (when (any nested-query-in-content?
                 (filter (lambda (child)
                           (not (and (content-element? child)
                                     (eq? (content-element-kind child)
                                          'forest-query))))
                         (content-document-children
                           (site-page-document page))))
        (error "Forester query must be a top-level page command"
               (site-page-source page))))
    (pages forest))
  (let* ((query-pages
           (filter (lambda (page)
                     (query-in-content? (site-page-document page)))
                   (pages forest))))
    (if (null? query-pages)
        forest
        (let* ((query-page-ids (map site-page-id query-pages))
               (resolved-pages
                 (map (lambda (page)
                        (if (member (site-page-id page) query-page-ids string=?)
                            (make-site-page
                              (site-page-id page)
                              (site-page-title page)
                              (resolve-page-queries forest page)
                              (site-page-source page)
                              #:metadata (site-page-metadata page))
                            page))
                      (pages forest)))
               (resolved-page-index (make-hash-table))
               (_ (for-each
                    (lambda (page)
                      (hash-set! resolved-page-index (site-page-id page) page))
                    resolved-pages))
               (resolved-site
                 (make-site resolved-pages
                   #:title (site-title (site forest))
                   #:description (site-description (site forest))
                   #:sources (site-sources (site forest)))))
          (validate-query-composition! forest resolved-pages)
          (%make-forest
            resolved-site resolved-pages
            (reference-relation forest) (composition forest)
            resolved-page-index (page-relation forest)
            (content-node-relation forest) (content-node-index forest)
            query-page-ids)))))

(define (element-plain-text? element)
  (and (content-element? element)
       (every (lambda (child) (not (content-element? child)))
              (content-element-children element))))

(define (content-node forest id)
  (let ((index (content-node-index forest)))
    (if (and (integer? id)
             (exact? id)
             (>= id 0)
             (< id (vector-length index)))
        (vector-ref index id)
        (error "unknown content node" id))))

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
         (page-index (make-hash-table))
         (_ (for-each (lambda (page)
                        (hash-set! page-index (site-page-id page) page))
                      pages))
         (reference-facts
           (append-map
             (lambda (source-page)
               (let* ((source-id (site-page-id source-page))
                      (elements
                        (link-elements
                          (owned-content
                            (site-page-document source-page)))))
                 (map
                   (lambda (element index)
                     (let* ((target
                              (content-element-property element 'target))
                            (target-page-id target)
                            (target-page (and (non-empty-string? target-page-id)
                                              (not (string-index target-page-id #\#))
                                              (hash-ref page-index target-page-id))))
                       (unless target-page
                         (error
                           (format #f
                             "unresolved Forester link in page ~a (~a): ~a"
                             source-id
                             (or (site-page-source source-page)
                                 "<unknown source>")
                             target)))
                       (list source-id (format #f "ref-~a" index)
                             target-page-id
                             (content->plain-text element))))
                   elements (iota (length elements) 1))))
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
                         (unless (non-empty-string? target-id)
                           (error
                             (format #f
                               "invalid Forester transclusion ~a in tree ~a (~a): ~s"
                               transclusion-id source-id source-path target-id)))
                         (unless (hash-ref page-index target-id)
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
           (make-reference-relation reference-facts))
         (page-relation
           (make-page-relation
             (map (lambda (page)
                    (list (site-page-id page)
                          (site-page-title page)
                          (site-page-metadata page)))
                  pages)))
         (content-node-state
           (call-with-values
             (lambda () (content-node-facts pages))
             list))
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
    (resolve-forest-queries
      (%make-forest site pages
                    reference-relation composition page-index page-relation
                    (car content-node-state)
                    (cadr content-node-state)
                    '()))))

(define (find-page forest id)
  (or (hash-ref (page-index forest) id)
      (error "unknown forest page" id)))

(define (subtree-definitions forest)
  (map (lambda (definition)
       (list (composition:subtree-definition-parent definition)
               (composition:subtree-definition-child definition)))
       (composition:subtree-definitions
         (composition forest))))

(define (links forest id)
  (map reference-target-page-id (outgoing-references forest id)))

(define (outgoing-references forest id)
  (find-page forest id)
  (map (lambda (row) (apply %make-reference row))
       (outgoing-reference-rows (reference-relation forest) id)))

(define (outgoing-transclusions forest id)
  (find-page forest id)
  (composition:outgoing-transclusions
    (composition forest) id))

(define (incoming-transclusions forest id)
  (find-page forest id)
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

(define (incoming-references forest id)
  (find-page forest id)
  (map (lambda (row) (apply %make-reference row))
       (incoming-reference-rows (reference-relation forest) id)))

(define (backlinks forest id)
  (find-page forest id)
  (let ((source-ids
          (backlink-page-ids (reference-relation forest) id)))
    (filter (lambda (page) (member (site-page-id page) source-ids equal?))
            (pages forest))))

(define (common-backlinks forest left-id right-id)
  (for-each
    (lambda (id)
      (find-page forest id))
    (list left-id right-id))
  (let ((source-ids
          (common-backlink-page-ids
            (reference-relation forest) left-id right-id)))
    (filter (lambda (page) (member (site-page-id page) source-ids equal?))
            (pages forest))))

(define (reachable-pages forest id)
  (find-page forest id)
  (let ((reachable-ids
          (reachable-page-ids (reference-relation forest) id)))
    (filter (lambda (page)
              (member (site-page-id page) reachable-ids equal?))
            (pages forest))))

(define (source-paths forest)
  (let ((site (site forest)))
    (delete-duplicates
      (append (site-sources site)
              (filter-map site-page-source (site-pages site))
              (append-map
                (lambda (page)
                  (content-source-paths (site-page-document page)))
                (site-pages site)))
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
  (let ((included (included-tree-ids forest (site-page-id page))))
    (if (any (lambda (id) (member id (query-page-ids forest) string=?))
             included)
        (source-paths forest)
        (delete-duplicates
          (append (module-paths forest)
                  (filter-map
                    (lambda (id)
                      (site-page-source (find-page forest id)))
                    included)
                  (content-source-paths (site-page-document page))
                  (filter-map site-page-source
                              (backlinks forest (site-page-id page)))
                  (filter-map site-page-source
                              (transcluding-pages forest
                                                  (site-page-id page)))
                  (append-map
                    (lambda (tree-id)
                      (filter-map
                        (lambda (reference)
                          (site-page-source
                            (find-page forest
                              (reference-target-page-id reference))))
                        (outgoing-references forest tree-id)))
                    included))
          string=?))))
