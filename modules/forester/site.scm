;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove i18n)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove private snapshot)
  #:use-module (grove string-fun)
  #:use-module (grove ir)
  #:use-module (grove render plain-text)
  #:use-module (grove site)
  #:use-module (grove private relation)
  #:use-module (grove project)
  #:use-module (grove private project timing)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module ((forester private trees) #:prefix tree-pass:)
  #:use-module ((forester private content-index) #:prefix content-index:)
  #:use-module ((forester private query-output) #:prefix query-output:)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module ((forester relations) #:prefix relations:)
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
            described-entity-relation
            describing-pages
            content-node-relation
            content-node
            topic-pages
            related-pages
            reference-pages
            find-page
            lookup-page
            page-taxon
            page-tags
            links
            tree-occurrences
            tree-occurrence-target
            page-anchor-facts
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
            context-pages
            used-codepoints
            source-paths
            module-paths
            page-source-paths
            term-definition-relation
            term-mention-relation
            term-definitions
            term-definition-conflicts
            undefined-term-mentions))

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
  (source-page-id reference-source-page-id/raw)
  (source-id reference-source-id/raw)
  (target-page-id reference-target-page-id/raw)
  (label reference-label/raw))

(define (make-reference source-page-id source-id target-page-id label)
  (%make-reference (snapshot-data source-page-id)
                   (snapshot-data source-id)
                   (snapshot-data target-page-id)
                   (snapshot-data label)))

(define (reference-source-page-id reference)
  (snapshot-data (reference-source-page-id/raw reference)))

(define (reference-source-id reference)
  (snapshot-data (reference-source-id/raw reference)))

(define (reference-target-page-id reference)
  (snapshot-data (reference-target-page-id/raw reference)))

(define (reference-label reference)
  (snapshot-data (reference-label/raw reference)))

(define-record-type <forest>
  (%make-forest site pages reference-relation
                composition page-index page-relation described-entity-relation
                content-node-relation
                content-node-index term-definition-relation
                term-mention-relation query-page-ids)
  forest?
  (site site)
  (pages forest-pages/raw)
  (reference-relation reference-relation)
  (composition composition)
  (page-index page-index)
  (page-relation page-relation)
  (described-entity-relation described-entity-relation)
  (content-node-relation content-node-relation)
  (content-node-index content-node-index)
  (term-definition-relation term-definition-relation)
  (term-mention-relation term-mention-relation)
  (query-page-ids query-page-ids))

(define (pages forest)
  (snapshot-data (forest-pages/raw forest)))

(define (id-member-set ids)
  (let ((table (make-hash-table)))
    (for-each (lambda (id) (hash-set! table id #t)) ids)
    table))

(define* (topic-pages forest topic-id #:optional kind)
  ;;     Backmatter consults this for every rendered page, so the scan
  ;;     over all pages must run once per topic, not once per page: the
  ;;     memo key carries the topic and the kind, and the id set turns
  ;;     the membership test into a hash lookup instead of a repeated
  ;;     string-list scan.
  (cached-rows forest `(topic-pages ,topic-id ,kind)
    (lambda ()
      (let ((page-ids
              (id-member-set
                (topic-page-ids (content-node-relation forest)
                                topic-id kind))))
        (filter (lambda (page)
                  (hash-ref page-ids (site-page-id page)))
                (pages forest))))))

(define* (related-pages forest page-id
                        #:optional (reference-taxon-id "reference"))
  "Return direct link targets except pages in the reference taxon."
  (let ((reference-ids
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
  (let* ((reference-ids
          (id-member-set
            (map site-page-id
                 (topic-pages forest reference-taxon-id 'taxon))))
         (seen (make-hash-table)))
    (filter-map
      (lambda (target-id)
        ;; Dedup, the reference-taxon filter, and page resolution fuse
        ;; here: the appended reference rows can carry thousands of
        ;; entries per page and a quadratic rescan showed up in builds.
        (and (not (hash-ref seen target-id))
             (begin (hash-set! seen target-id #t)
                    (and (hash-ref reference-ids target-id)
                         (find-page forest target-id)))))
      (append-map
        (lambda (tree-id)
          (map reference-target-page-id
               (outgoing-references forest tree-id)))
        (included-tree-ids forest page-id)))))

(define (query-in-content? content)
  (and (content-document? content)
       (any (lambda (child)
              (and (content-element? child)
                   (eq? (content-element-kind child) 'forest-query)))
            (ir:content-document-children/raw content))))

(define (nested-query-in-content? content)
  (cond
    ((content-element? content)
     (or (eq? (content-element-kind content) 'forest-query)
         (and (not (eq? (content-element-kind content) 'subtree))
              (any nested-query-in-content?
                   (ir:content-element-children/raw content)))))
    ((content-document? content)
     (any nested-query-in-content? (ir:content-document-children/raw content)))
    (else #f)))

(define (query-source-label page request)
  (let ((location (ir:content-element-property/raw request 'source-location)))
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
               (query (ir:content-element-property/raw content 'query))
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
          (ir:content-document-children/raw
            (query-output:prepare-query-output result source
              (lambda (target) (find-page forest target))
              known-anchor?)))
        (list content)))
  (make-content-document
    (append-map resolve (ir:content-document-children/raw
                          (site-page-document page)))
    (ir:content-document-source/raw (site-page-document page))))

(define (validate-query-composition! forest resolved-pages)
  ;; Validate the resolved display graph without adding generated facts to the
  ;; authored composition snapshot.
  (validate-tree-cycles!
    resolved-pages
    (lambda (page)
      (append
        (composition:target-ids (composition forest) (site-page-id page))
        (map (lambda (element) (ir:content-element-property/raw element 'target))
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
                         (ir:content-document-children/raw
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
            (described-entity-relation forest)
            (content-node-relation forest) (content-node-index forest)
            (term-definition-relation forest)
            (term-mention-relation forest)
            query-page-ids)))))

(define (content-node forest id)
  (let ((index (content-node-index forest)))
    (if (and (integer? id)
             (exact? id)
             (>= id 0)
             (< id (vector-length index)))
        (vector-ref index id)
        (error "unknown content node" id))))

(define (page-topic-elements page kind)
  "The page's own top-level topic elements of KIND ('taxon or 'tag):
frontmatter topics declared at the top of the tree.  Topics belonging
to nested trees or arriving through transclusions stay out - they are
those trees' frontmatter."
  (filter-map
    (lambda (node)
      (and (content-element? node)
           (eq? (content-element-kind node) kind)
           node))
    (ir:content-document-children/raw (site-page-document page))))

(define (page-taxon page)
  "The page's own taxon element, or #f when it declares none."
  (let loop ((nodes (page-topic-elements page 'taxon)))
    (if (null? nodes) #f (car nodes))))

(define (page-tags page)
  "The page's own tag elements, in declaration order."
  (page-topic-elements page 'tag))

(define (page-anchor-facts page)
  "Return (kind . id) pairs for the anchor IDs a page declares in its own
document: the page's root anchor, explicit math IDs from owned content, the
anchors of its subtree and transclusion elements, and anchors declared by
ordinary elements (a term definition, a figure). Anchors brought in through
transclusions belong to their own pages and stay out of this set."
  (let ((document (site-page-document page)))
    (append
      (list (cons "anchor" (string-append "tree-root-" (site-page-id page))))
      (map (lambda (id) (cons "math" id))
           (filter-map
             (lambda (element) (ir:content-element-property/raw element 'id))
             (tree-pass:math-elements
               (tree-pass:owned-content document))))
      (map (lambda (id) (cons "anchor" id))
           (filter-map
             (lambda (element) (ir:content-element-property/raw element 'anchor))
             (append (tree-pass:subtree-elements document)
                     (tree-pass:transclusion-elements document))))
      (map (lambda (element)
             (cons "anchor" (ir:content-element-property/raw element 'anchor)))
           (tree-pass:declared-anchor-elements document)))))

(define (element-tag-name element)
  "The HTML tag a vocabulary element renders as, from the style's tag
marker - dfn for a defining occurrence, say."
  (let ((style (content-element-style element)))
    (and style
      (let loop ((properties (content-style-properties style)))
        (cond
          ((null? properties) #f)
          ((html-tag? (car properties))
           (html-tag-name (car properties)))
          (else (loop (cdr properties))))))))

(define (dom-anchor-index pages composition transclusion-index)
  "Build a page-id → rendered-anchor-ids table.  A page's set is its own
anchors plus, transitively, the anchors of every tree its rendered DOM
includes through subtrees and transclusions - the set of anchors a
fragment link against this page can actually scroll to.  Composition
cycles are rejected before this runs; the visited guard only guards the
walk itself."
  (let* ((own
          (let ((table (make-hash-table)))
            (for-each
              (lambda (page)
                (hash-set! table (site-page-id page)
                           (map cdr (page-anchor-facts page))))
              pages)
            table))
         (children
           (let ((table (make-hash-table)))
             (define (link! parent child)
               (hash-set! table parent
                          (cons child (hash-ref table parent '()))))
             (for-each
               (lambda (entry)
                 (for-each
                   (lambda (transclusion)
                     (link! (car entry)
                            (composition:transclusion-target-tree-id
                              transclusion)))
                   (cdr entry)))
               transclusion-index)
             (for-each
               (lambda (definition)
                 (link! (composition:subtree-definition-parent definition)
                        (composition:subtree-definition-child definition)))
               (composition:subtree-definitions composition))
             table))
         (dom (make-hash-table)))
    (define (dom-of id)
      (or (hash-ref dom id)
          (let ()
            (hash-set! dom id '())      ;; cycle guard for the walk
            (let ((anchors
                    (apply lset-union equal?
                           (hash-ref own id '())
                           (map dom-of (hash-ref children id '())))))
              (hash-set! dom id anchors)
              anchors))))
    (for-each
      (lambda (page)
        (hash-set! dom (site-page-id page)
                   (dom-of (site-page-id page))))
      pages)
    dom))

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
  "Forward analysis timing to the caller's callback when one is installed."
  ((current-analysis-timings) phase seconds))

(define (normalize-page-locale page)
  (let* ((metadata (site-page-metadata page))
         (locale (assq-ref metadata 'locale)))
    (if (not locale)
        page
        (let ((canonical
                (catch #t
                  (lambda () (locale-tag (make-locale locale)))
                  (lambda (key . arguments)
                    (error "Forester page locale must be a BCP 47 tag"
                           (site-page-source page) locale)))))
          (if (string=? locale canonical)
              page
              (make-site-page
                (site-page-id page)
                (site-page-title page)
                (site-page-document page)
                (site-page-source page)
                #:metadata
                (map (lambda (entry)
                       (if (eq? (car entry) 'locale)
                           (cons 'locale canonical)
                           entry))
                     metadata)))))))

(define* (make-forest site #:key (timings #f))
  (define (report-phase phase thunk)
    (let ((report (or timings
                      (and (current-analysis-timings)
                           (lambda (phase seconds)
                             (report-forest-phase phase seconds))))))
      (if (not report)
          (thunk)
          (let ((announce (current-analysis-phase-start)))
            (when announce (announce phase))
            (let* ((start (get-internal-real-time))
                   (result (thunk)))
              (report phase (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second))
              result)))))
  (define-syntax-rule (measure phase expression)
    (report-phase phase (lambda () expression)))
  (unless (site? site)
    (error "forest requires a Grove site" site))
  (unless (or (not timings) (procedure? timings))
    (error "forest timings must be a procedure" timings))
  (set! site
    (make-site (map normalize-page-locale (site-pages site))
      #:title (site-title site)
      #:description (site-description site)
      #:sources (site-sources site)
      #:home (site-home site)))
  (let* ((root-pages
           (measure 'identify-trees
             (tree-pass:assign-subtree-identifiers
               (site-pages site))))
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
                              (ir:content-element-property/raw element 'target))
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
                                (ir:content-element-property/raw element 'target))
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
         (term-definition-relation
           (measure 'term-definitions
             (make-term-definition-relation
               (append-map
                 (lambda (page)
                   (filter-map
                     (lambda (element)
                       (let ((anchor
                               (ir:content-element-property/raw element 'anchor)))
                         (and anchor
                              (string=? "dfn" (element-tag-name element))
                              (list anchor
                                    (site-page-id page)
                                    anchor
                                    (content->plain-text element)))))
                     (tree-pass:declared-anchor-elements
                       (site-page-document page))))
                 pages))))
         (page-relation
           (measure 'page-index (make-page-relation
             (map (lambda (page)
                    (list (site-page-id page)
                          (site-page-title page)
                          (site-page-metadata page)))
                  pages))))
         (described-entity-relation
           (measure 'described-entities
             (make-described-entity-relation
               (append-map
                 (lambda (page)
                   (map (lambda (uri) (list (site-page-id page) uri))
                        (or (assq-ref (site-page-metadata page) 'describes) '())))
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
                          (ir:content-element-property/raw element 'tree-id)))
                      (tree-pass:direct-subtree-elements
                        (site-page-document page))))
               pages)
             (append-map cdr transclusion-index))))
         (anchor-index
           (measure 'anchors
             (dom-anchor-index pages composition transclusion-index)))
         (term-mention-relation
           (measure 'term-mentions
             (make-term-mention-relation
               (append-map
                 (lambda (source-page)
                   (let ((source-id (site-page-id source-page)))
                     (filter-map
                       (lambda (element)
                         (let ((term-mark
                                 (ir:content-element-property/raw element 'term))
                               (fragment
                                 (ir:content-element-property/raw element 'fragment))
                               (target-page-id
                                 (ir:content-element-property/raw element 'target)))
                           (cond
                             ;;     A marked mention: resolution goes
                             ;;     through the definition relation -
                             ;;     the raw anchor registry - never
                             ;;     through rendered DOM ids.
                             (term-mark
                              (list term-mark source-id
                                    (pair? (term-definition-rows
                                             term-definition-relation
                                             term-mark))))
                             ;;     An ordinary fragment link aimed at
                             ;;     an anchor that resolves nowhere.
                             ((and (non-empty-string? fragment)
                                   (not (member fragment
                                                (hash-ref anchor-index
                                                          target-page-id '())
                                                string=?)))
                              (list fragment source-id #f))
                             (else #f))))
                       (append (tree-pass:link-elements
                                 (tree-pass:owned-content
                                   (site-page-document source-page)))
                               (tree-pass:query-link-elements
                                 (tree-pass:owned-content
                                   (site-page-document source-page)))))))
                 pages))))
         (_ (for-each
              (lambda (source-page)
                (let ((source-id (site-page-id source-page))
                      (source-path
                        (or (site-page-source source-page) "<unknown source>")))
                  (for-each
                    (lambda (element)
                      (let ((fragment
                              (ir:content-element-property/raw element 'fragment))
                            (target-page-id
                              (ir:content-element-property/raw element 'target)))
                        (when (and fragment
                                   (not (member fragment
                                                (hash-ref anchor-index
                                                          target-page-id '())
                                                string=?)))
                          (format (current-warning-port)
                            "warning: fragment ~s does not resolve in page ~a (link authored in ~a at ~a)\n"
                            fragment target-page-id source-id source-path))))
                    (tree-pass:link-elements
                      (tree-pass:owned-content
                        (site-page-document source-page))))))
              pages))
         (_ (validate-tree-cycles! pages
              (lambda (page)
                (composition:target-ids composition (site-page-id page))))))
    ;;     Force every column index while still single-threaded: page
    ;;     rendering reads them in parallel and a concurrent force would
    ;;     race inside the promise.
    (for-each
      warm-relation-indexes!
      (list reference-relation page-relation described-entity-relation
            term-definition-relation term-mention-relation
            (car content-node-state)))
    (measure 'queries (resolve-forest-queries
      (%make-forest site pages
                    reference-relation composition page-index page-relation
                    described-entity-relation
                    (car content-node-state)
                    (cadr content-node-state)
                    term-definition-relation term-mention-relation
                    '())
      anchor-index))))

(define (find-page forest id)
  (or (hash-ref (page-index forest) id)
      (error "unknown forest page" id)))

(define (lookup-page forest id)
  "The page carrying ID from the index, or #f when none does.
Backmatter entry rendering resolves relation-sourced ids through this
index instead of scanning the page list per entry."
  (hash-ref (page-index forest) id))

(define (describing-pages forest uri)
  "Return pages that declare they describe URI, in site order."
  (let ((ids (id-member-set
               (described-entity-page-ids
                 (described-entity-relation forest) uri))))
    (filter (lambda (page) (hash-ref ids (site-page-id page)))
            (pages forest))))

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
  (snapshot-data
    (with-mutex %site-lock
      (let ((forest-cache
              (or (hash-ref %query-cache forest)
                  (let ((table (make-hash-table)))
                    (hash-set! %query-cache forest table)
                    table))))
        (or (hash-ref forest-cache key)
            (let ((value (thunk)))
              (hash-set! forest-cache key value)
              value))))))

(define (outgoing-references forest id)
  (find-page forest id)
  (cached-rows forest `(outgoing-references ,id)
    (lambda ()
      (map (lambda (row) (apply make-reference row))
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

;; Context = every page in whose displayed content this page appears:
;; hosts of its transclusions and the page that defines it as a subtree.
(define (context-pages forest id)
  (cached-rows forest `(context-pages ,id)
    (lambda ()
      (let ((source-ids
              (id-member-set
                (delete-duplicates
                  (append
                   (map composition:transclusion-source-tree-id
                        (incoming-transclusions forest id))
                   (map composition:subtree-definition-parent
                        (filter (lambda (definition)
                                  (equal? (composition:subtree-definition-child
                                            definition)
                                          id))
                                (composition:subtree-definitions
                                  (composition forest)))))
                  equal?))))
        (filter (lambda (page)
                  (hash-ref source-ids (site-page-id page)))
                (pages forest))))))

(define (used-codepoints forest)
  "Every codepoint in the site's rendered text — document text and page
titles, as a sorted character list. This is the universe a text font
must cover; site chrome strings stay the caller's concern. Math is
counted through its plain-text rendering, a harmless superset since
MathJax draws it with its own fonts."
  (cached-rows forest 'used-codepoints
    (lambda ()
      (let ((codepoints (make-hash-table)))
        (for-each
          (lambda (page)
            (for-each
              (lambda (text)
                (string-for-each
                  (lambda (char) (hash-set! codepoints char #t))
                  text))
                (append (list (site-page-title page)
                              (content-index:content->plain-text
                                (site-page-document page)))
                        ;;     Analysis strips link text from the document;
                        ;;     the label is what rendering puts back. Rows
                        ;;     come straight from the relation: the cached
                        ;;     query helper would re-take the site lock.
                        (map (lambda (row)
                               (reference-label (apply make-reference row)))
                             (outgoing-reference-rows
                               (reference-relation forest)
                               (site-page-id page))))))
          (pages forest))
        (sort (hash-map->list (lambda (char _) char) codepoints)
              (lambda (a b) (< (char->integer a) (char->integer b))))))))

(define (incoming-references forest id)
  (find-page forest id)
  (cached-rows forest `(incoming-references ,id)
    (lambda ()
      (map (lambda (row) (apply make-reference row))
           (incoming-reference-rows (reference-relation forest) id)))))

(define (backlinks forest id)
  (find-page forest id)
  (cached-rows forest `(backlink-pages ,id)
    (lambda ()
      (let ((source-ids
              (id-member-set
                (backlink-page-ids (reference-relation forest) id))))
        (filter (lambda (page) (hash-ref source-ids (site-page-id page)))
                (pages forest))))))

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

;;     Term queries: one term's defining occurrences for glossary
;;     generation and hover previews; terms whose occurrences disagree;
;;     and the corpus's definition backlog - fragments pointed at but
;;     never defined.  The report projections name the host article -
;;     the root page whose rendered DOM carries each occurrence - next
;;     to the owning subtree, because an in-paragraph definition's
;;     owning id (an anonymous unstable-N) is unreadable in a report
;;     without it.

(define (forest-host-table forest)
  "Memoized owning-page-id to (host-article-id host-article-title):
the root page whose rendered DOM carries each page, walking subtree
definitions up to the roots; transclusion inclusions fill the gaps for
pages that only exist inside another article's DOM."
  (cached-rows forest '(term-host-table)
    (lambda ()
      (let ((parent-of (make-hash-table)))
        (for-each
          (lambda (definition)
            (unless (hash-ref parent-of
                              (composition:subtree-definition-child definition))
              (hash-set! parent-of
                (composition:subtree-definition-child definition)
                (composition:subtree-definition-parent definition))))
          (composition:subtree-definitions (composition forest)))
        (for-each
          (lambda (page)
            (for-each
              (lambda (transclusion)
                (unless (hash-ref parent-of
                                  (composition:transclusion-target-tree-id
                                    transclusion))
                  (hash-set! parent-of
                    (composition:transclusion-target-tree-id transclusion)
                    (composition:transclusion-source-id transclusion))))
              (composition:outgoing-transclusions
                (composition forest) (site-page-id page))))
          (pages forest))
        (let ((hosts (make-hash-table)))
          (define (host-of id)
            (or (hash-ref hosts id)
                (let* ((host
                         (let loop ((current id) (path (list id)))
                           (let ((parent (hash-ref parent-of current)))
                             (cond
                               ((not parent) current)
                               ((member parent path equal?) current)
                               (else (loop parent (cons parent path)))))))
                       (host-page (hash-ref (page-index forest) host)))
                  (hash-set! hosts id
                    (list host (if host-page
                                   (site-page-title host-page)
                                   host)))
                  (hash-ref hosts id))))
          (for-each (lambda (page) (host-of (site-page-id page)))
                    (pages forest))
          hosts)))))

(define (term-definitions forest term-id)
  (relations:term-definition-rows
    (term-definition-relation forest) term-id))

(define (term-definition-conflicts forest)
  (let ((hosts (forest-host-table forest)))
    (map
      (lambda (entry)
        (cons (car entry)
              (map (lambda (occurrence)
                     (let ((host (hash-ref hosts (car occurrence))))
                       (list (car host) (cadr host)
                             (car occurrence) (cadr occurrence)
                             (caddr occurrence))))
                   (cdr entry))))
      (relations:term-definition-conflicts
        (term-definition-relation forest)))))

(define (undefined-term-mentions forest)
  (let ((hosts (forest-host-table forest)))
    (map
      (lambda (entry)
        (list (car entry)
              (map (lambda (page-id)
                     (let ((host (hash-ref hosts page-id)))
                       (list (car host) (cadr host) page-id)))
                   (cadr entry))))
      (relations:undefined-term-mentions
        (term-definition-relation forest)
        (term-mention-relation forest)))))

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
