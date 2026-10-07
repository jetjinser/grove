;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove string-fun)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (grove project)
  #:use-module ((forester private trees) #:prefix tree-pass:)
  #:use-module ((forester private content-index) #:prefix content-index:)
  #:use-module ((forester private query-output) #:prefix query-output:)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module (grove relation dsl)
  #:use-module (ice-9 threads)
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

(define (validate-tree-cycles! pages targets)
  (let ((edges
          (map (lambda (page)
                 (cons (site-page-id page)
                       (targets page)))
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

(define (id-member-set ids)
  (let ((table (make-hash-table)))
    (for-each (lambda (id) (hash-set! table id #t)) ids)
    table))

(define* (topic-pages forest topic-id #:optional kind)
  (let ((page-ids
          (topic-page-ids (content-node-relation forest) topic-id kind)))
    (filter (lambda (page)
              (member (site-page-id page) page-ids string=?))
            (pages forest))))

(define* (related-pages forest page-id
                        #:optional (reference-taxon-id "reference"))
  "Return direct link targets except pages in the reference taxon."  (let ((reference-ids
          (id-member-set
            (map site-page-id
                 (topic-pages forest reference-taxon-id 'taxon)))))
    (map (lambda (target-id) (find-page forest target-id))
         (filter (lambda (target-id)
                   (not (hash-ref reference-ids target-id)))
                 (delete-duplicates
                   (map reference-target-page-id
                        (outgoing-references forest page-id))
                   string=?)))))

(define* (reference-pages forest page-id
                          #:optional (reference-taxon-id "reference"))
  "Return reference-taxon pages linked from this tree and its composition."
  (let ((reference-ids
          (id-member-set
            (map site-page-id
                 (topic-pages forest reference-taxon-id 'taxon)))))
    (map (lambda (target-id) (find-page forest target-id))
         (filter (lambda (target-id)
                   (hash-ref reference-ids target-id))
                 (delete-duplicates
                   (append-map
                     (lambda (tree-id)
                       (map reference-target-page-id
                            (outgoing-references forest tree-id)))
                     (included-tree-ids forest page-id))
                   string=?)))))

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

(define (resolve-page-queries forest anchor-index page)
  (define (resolve content)
    (if (and (content-element? content)
             (eq? (content-element-kind content) 'forest-query))
        (let* ((source (query-source-label page content))
               (query (content-element-property content 'query))
               (result
                 (catch #t
                   (lambda () (query forest (site-page-id page)))
                   (lambda (key . args)
                     (error (format #f "Forester query failed at ~a: ~s"
                                    source (cons key args))))))
               (known-anchor?
                 (lambda (target fragment)
                   (and anchor-index
                        (member fragment
                                (hash-ref anchor-index target '())
                                string=?)))))
          (content-document-children
            (query-output:prepare-query-output result source
              (lambda (target) (find-page forest target))
              known-anchor?)))
        (list content)))
  (make-content-document
    (append-map resolve (content-document-children
                          (site-page-document page)))
    (content-document-source (site-page-document page))))

(define (validate-query-composition! forest resolved-pages)
  ;; Validate the resolved display graph without adding generated facts to the
  ;; authored composition snapshot.
  (validate-tree-cycles!
    resolved-pages
    (lambda (page)
      (append
        (composition:target-ids (composition forest) (site-page-id page))
        (map (lambda (element) (content-element-property element 'target))
             (tree-pass:walk-content (tree-pass:owned-content (site-page-document page))
                           (tree-pass:element-kind? 'query-transclude)))))))

(define (tree-occurrences forest id)
  "Return direct tree displays in body order, stopping at tree boundaries."
  (define (tree-display? node)
    (and (content-element? node)
         (memq (content-element-kind node) '(subtree transclude query-transclude))))
  (tree-pass:walk-content (site-page-document (find-page forest id))
                tree-display? #:stop? tree-display?))

(define (tree-occurrence-target element)
  (content-element-property element
    (if (eq? (content-element-kind element) 'subtree) 'tree-id 'target)))

(define (resolve-forest-queries forest anchor-index)
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
               (query-ids (id-member-set query-page-ids))
               (resolved-pages
                 (map (lambda (page)
                        (if (hash-ref query-ids (site-page-id page))
                            (make-site-page
                              (site-page-id page)
                              (site-page-title page)
                              (resolve-page-queries forest anchor-index page)
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
                   #:sources (site-sources (site forest))
                   #:home (site-home (site forest)))))
          (validate-query-composition! forest resolved-pages)
          (%make-forest
            resolved-site resolved-pages
            (reference-relation forest) (composition forest)
            resolved-page-index (page-relation forest)
            (content-node-relation forest) (content-node-index forest)
            query-page-ids)))))

(define (content-node forest id)
  (let ((index (content-node-index forest)))
    (if (and (integer? id)
             (exact? id)
             (>= id 0)
             (< id (vector-length index)))
        (vector-ref index id)
        (error "unknown content node" id))))

(define (page-anchor-facts page)
  "Return (kind . id) pairs for the anchor IDs a page declares in its own
document: the page's root anchor, explicit math IDs from owned content, and
the anchors of its subtree and transclusion elements. Anchors brought in
through transclusions belong to their own pages and stay out of this set."
  (let ((document (site-page-document page)))
    (append
      (list (cons "anchor" (string-append "tree-root-" (site-page-id page))))
      (map (lambda (id) (cons "math" id))
           (filter-map
             (lambda (element) (content-element-property element 'id))
             (tree-pass:math-elements
               (tree-pass:owned-content document))))
      (map (lambda (id) (cons "anchor" id))
           (filter-map
             (lambda (element) (content-element-property element 'anchor))
             (append (tree-pass:subtree-elements document)
                     (tree-pass:transclusion-elements document)))))))

(define (ensure-unique-anchor-ids pages)
  "Reject duplicate author-facing anchor IDs in a page's own rendering.

A page's scope is the set page-anchor-facts returns. Content pulled in
through transclusions is not scanned, and author anchors may collide with the
generated tree-N position anchors; both are known limitations of global
anchors."
  (for-each
    (lambda (page)
      (let ((seen (make-hash-table)))
        (for-each
          (lambda (fact)
            (let ((id (cdr fact)))
              (when (hash-ref seen id)
                (error
                  (format #f "duplicate ~a ID in page ~a (~a): ~a"
                          (car fact)
                          (site-page-id page)
                          (or (site-page-source page) "<unknown source>")
                          id)))
              (hash-set! seen id #t)))
          (page-anchor-facts page))))
    pages))

(define (report-forest-phase phase seconds)
  "Forward to the caller's analysis-timing hook when one is installed."
  ((current-analysis-timings) phase seconds))

(define* (make-forest site #:key (timings #f))
  (define (report-phase phase thunk)
    (let ((report (or timings
                      (and (current-analysis-timings)
                           (lambda (phase seconds)
                             (report-forest-phase phase seconds))))))
      (if (not report)
          (thunk)
          (let* ((start (get-internal-real-time))
                 (result (thunk)))
            (report phase (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second))
            result))))
  (define-syntax-rule (measure phase expression)
    (report-phase phase (lambda () expression)))
  (unless (site? site)
    (error "forest requires a Grove site" site))
  (unless (or (not timings) (procedure? timings))
    (error "forest timings must be a procedure" timings))
  (let* ((root-pages
           (measure 'identify-trees
             (tree-pass:assign-subtree-identifiers (site-pages site))))
         (subtrees (measure 'extract-trees (tree-pass:subtree-pages root-pages)))
         (pages (append root-pages subtrees))
         (pages (measure 'occurrence-anchors
                  (tree-pass:assign-unique-occurrence-anchors pages)))
         (pages (measure 'equation-numbers
                  (tree-pass:assign-equation-numbers pages)))
         (_ (make-site pages
                       #:title (site-title site)
                       #:description (site-description site)
                       #:sources (site-sources site)
                       #:home (site-home site)))
         (page-index (make-hash-table))
         (_ (for-each (lambda (page)
                        (hash-set! page-index (site-page-id page) page))
                      pages))
         (_ (ensure-unique-anchor-ids pages))
         (anchor-index
           (measure 'anchors
             (let ((index (make-hash-table)))
               (for-each
                 (lambda (page)
                   (hash-set! index (site-page-id page)
                              (map cdr (page-anchor-facts page))))
                 pages)
               index)))
         (reference-facts
           (measure 'references (append-map
             (lambda (source-page)
               (let* ((source-id (site-page-id source-page))
                      (elements
                        (tree-pass:link-elements
                          (tree-pass:owned-content
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
                       (let ((fragment
                               (content-element-property element 'fragment)))
                         (when (and fragment
                                    (not (member fragment
                                                 (hash-ref anchor-index
                                                           target-page-id '())
                                                 string=?)))
                           (format (current-warning-port)
                             "warning: fragment ~s does not resolve in page ~a (link authored in ~a at ~a)\n"
                             fragment target-page-id source-id
                             (or (site-page-source source-page)
                                 "<unknown source>"))))
                       (list source-id (format #f "ref-~a" index)
                             target-page-id
                             (content-index:content->plain-text element))))
                   elements (iota (length elements) 1))))
             pages)))
         (transclusion-index
           (measure 'transclusions (map
             (lambda (source-tree)
               (let* ((source-id (site-page-id source-tree))
                      (elements
                        (tree-pass:transclusion-elements
                          (tree-pass:owned-content
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
             pages)))
         (reference-relation
           (measure 'reference-index (make-reference-relation reference-facts)))
         (page-relation
           (measure 'page-index (make-page-relation
             (map (lambda (page)
                    (list (site-page-id page)
                          (site-page-title page)
                          (site-page-metadata page)))
                  pages))))
         (content-node-state
           (measure 'content (call-with-values
             (lambda () (content-index:build-content-node-index pages))
             list)))
         (composition
           (measure 'composition (composition:make-composition
             (append-map
               (lambda (page)
                 (map (lambda (element)
                        (composition:make-subtree-definition
                          (site-page-id page)
                          (content-element-property element 'tree-id)))
                      (tree-pass:direct-subtree-elements
                        (site-page-document page))))
               pages)
             (append-map cdr transclusion-index))))
         (_ (validate-tree-cycles! pages
              (lambda (page)
                (composition:target-ids composition (site-page-id page))))))
    ;;     Force every column index while still single-threaded: page
    ;;     rendering reads them in parallel and a concurrent force would
    ;;     race inside the promise.
    (for-each
      (lambda (relation)
        (for-each force (relation-indexes relation)))
      (list reference-relation page-relation
            (car content-node-state)))
    (measure 'queries (resolve-forest-queries
      (%make-forest site pages
                    reference-relation composition page-index page-relation
                    (car content-node-state)
                    (cadr content-node-state)
                    '())
      anchor-index))))

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

;; Relation queries solve through miniKanren, so the same rows would be
;; derived again for every page that renders a backmatter section. A
;; forest's relations never change once built, so results memoize per
;; forest for the forest's lifetime. Page rendering reads these caches
;; from several threads, so writes take the site lock.
(define %query-cache (make-weak-key-hash-table 0))
(define %site-lock (make-mutex))

(define (cached-rows forest key thunk)
  (with-mutex %site-lock
    (let ((forest-cache
            (or (hash-ref %query-cache forest)
                (let ((table (make-hash-table)))
                  (hash-set! %query-cache forest table)
                  table))))
      (or (hash-ref forest-cache key)
          (let ((value (thunk)))
            (hash-set! forest-cache key value)
            value)))))

(define (outgoing-references forest id)
  (find-page forest id)
  (cached-rows forest `(outgoing-references ,id)
    (lambda ()
      (map (lambda (row) (apply %make-reference row))
           (outgoing-reference-rows (reference-relation forest) id)))))

(define (outgoing-transclusions forest id)
  (find-page forest id)
  (composition:outgoing-transclusions
    (composition forest) id))

(define (incoming-transclusions forest id)
  (find-page forest id)
  (composition:incoming-transclusions
    (composition forest) id))

(define (transcluding-pages forest id)
  (cached-rows forest `(transcluding-pages ,id)
    (lambda ()
      (let ((source-ids
              (id-member-set
                (delete-duplicates
                  (map composition:transclusion-source-tree-id
                       (incoming-transclusions forest id))
                  equal?))))
        (filter (lambda (page)
                  (hash-ref source-ids (site-page-id page)))
                (pages forest))))))

(define (incoming-references forest id)
  (find-page forest id)
  (cached-rows forest `(incoming-references ,id)
    (lambda ()
      (map (lambda (row) (apply %make-reference row))
           (incoming-reference-rows (reference-relation forest) id)))))

(define (backlinks forest id)
  (find-page forest id)
  (let ((source-ids
          (cached-rows forest `(backlinks ,id)
            (lambda ()
              (id-member-set
                (backlink-page-ids (reference-relation forest) id))))))
    (filter (lambda (page) (hash-ref source-ids (site-page-id page)))
            (pages forest))))

(define (common-backlinks forest left-id right-id)
  (for-each
    (lambda (id)
      (find-page forest id))
    (list left-id right-id))
  (let ((source-ids
          (id-member-set
            (common-backlink-page-ids
              (reference-relation forest) left-id right-id))))
    (filter (lambda (page) (hash-ref source-ids (site-page-id page)))
            (pages forest))))

(define (reachable-pages forest id)
  (find-page forest id)
  (let ((reachable-ids
          (id-member-set
            (reachable-page-ids (reference-relation forest) id))))
    (filter (lambda (page)
              (hash-ref reachable-ids (site-page-id page)))
            (pages forest))))

(define (source-paths forest)
  (let ((site (site forest)))
    (delete-duplicates
      (append (site-sources site)
              (filter-map site-page-source (site-pages site))
              (append-map
                (lambda (page)
                  (tree-pass:content-source-paths (site-page-document page)))
                (site-pages site)))
      string=?)))

(define (module-paths forest)
  (let* ((site (site forest))
         (page-paths (filter-map site-page-source (site-pages site))))
    (filter (lambda (path) (not (member path page-paths string=?)))
            (site-sources site))))

(define (included-tree-ids forest start-id)
  (cached-rows forest `(included-tree-ids ,start-id)
    (lambda ()
      (let loop ((pending (list start-id)) (seen '()))
        (if (null? pending)
            seen
            (let ((id (car pending)))
              (if (member id seen equal?)
                  (loop (cdr pending) seen)
                  (let ((page (find-page forest id)))
                    (loop
                      (append
                       (map composition:transclusion-target-tree-id
                            (composition:outgoing-transclusions
                              (composition forest) id))
                       (map composition:subtree-definition-child
                            (filter (lambda (definition)
                                      (equal? (composition:subtree-definition-parent
                                                definition)
                                            id))
                                    (composition:subtree-definitions
                                      (composition forest))))
                       (cdr pending))
                      (cons id seen))))))))))

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
                  (tree-pass:content-source-paths (site-page-document page))
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
