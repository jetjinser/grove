;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private site queries)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove private snapshot)
  #:use-module (grove ir)
  #:use-module (grove render plain-text)
  #:use-module (grove site)
  #:use-module (forester private site model)
  #:use-module ((forester private trees) #:prefix tree-pass:)
  #:use-module ((forester private query-output) #:prefix query-output:)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module ((forester relations) #:prefix relations:)
  #:use-module (forester relations)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:export (topic-pages
            resolve-forest-queries
            validate-tree-cycles!
            related-pages
            reference-pages
            find-page
            require-page
            content-node
            page-taxon
            page-tags
            tree-occurrences
            tree-occurrence-target
            describing-pages
            subtree-definitions
            links
            outgoing-references
            incoming-references
            outgoing-transclusions
            incoming-transclusions
            transcluding-pages
            context-pages
            used-codepoints
            backlinks
            common-backlinks
            reachable-pages
            term-definitions
            term-definition-conflicts
            undefined-term-mentions
            source-paths
            page-source-paths))

(define (id-member-set ids)
  (let ((table (make-hash-table)))
    (for-each (lambda (id) (hash-set! table id #t)) ids)
    table))

(define (pages-with-ids forest ids)
  "Return the pages identified by IDS in the forest's stable site order."
  (let ((members (id-member-set ids)))
    (filter (lambda (page)
              (hash-ref members (site-page-id page)))
            (pages forest))))

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
  (let ((location (ir:content-element-location/raw request)))
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
              (lambda (target) (require-page forest target))
              known-anchor?)))
        (list content)))
  (make-content-document
    (append-map resolve (ir:content-document-children/raw
                          (site-page-document page)))
    #:source (ir:content-document-source/raw (site-page-document page))))

(define (validate-query-composition! forest resolved-pages)
  ;; Validate the resolved display graph without adding generated facts to the
  ;; authored composition snapshot.
  (validate-tree-cycles!
    resolved-pages
    (lambda (page)
      (append
        (composition:composition-target-ids
          (composition forest) (site-page-id page))
        (map (lambda (element) (ir:content-element-property/raw element 'target))
             (tree-pass:walk-owned-content
               (site-page-document page)
               (tree-pass:element-kind? 'query-transclude)))))))

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
                              #:source (site-page-source page)
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

(define* (topic-pages forest topic-id #:optional kind)
  ;;     Backmatter consults this for every rendered page, so the scan
  ;;     over all pages must run once per topic, not once per page: the
  ;;     memo key carries the topic and the kind, and the id set turns
  ;;     the membership test into a hash lookup instead of a repeated
  ;;     string-list scan.
  (cached-rows forest `(topic-pages ,topic-id ,kind)
    (lambda ()
      (pages-with-ids
        forest
        (topic-page-ids (content-node-relation forest) topic-id kind)))))

(define* (related-pages forest page-id
                        #:optional (reference-taxon-id "reference"))
  "Return direct link targets except pages in the reference taxon."
  (let ((reference-ids
          (id-member-set
            (map site-page-id
                 (topic-pages forest reference-taxon-id 'taxon)))))
    (map (lambda (target-id) (require-page forest target-id))
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
                         (require-page forest target-id)))))
      (append-map
        (lambda (tree-id)
          (map reference-target-page-id
               (outgoing-references forest tree-id)))
        (included-tree-ids forest page-id)))))
(define (tree-occurrences forest id)
  "Return direct tree displays in body order, stopping at tree boundaries."
  (require-page forest id)
  (cached-rows forest `(tree-occurrences ,id)
    (lambda ()
      (define (tree-display? node)
        (and (content-element? node)
             (memq (content-element-kind node)
                   '(subtree transclude query-transclude))))
      (tree-pass:walk-content
        (site-page-document (require-page forest id))
        tree-display? #:stop? tree-display?))))

(define (tree-occurrence-target element)
  (content-element-property element
    (if (eq? (content-element-kind element) 'subtree) 'tree-id 'target)))

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

(define (find-page forest id)
  (hash-ref (page-index forest) id))

(define (require-page forest id)
  "Return the page carrying ID, or raise an error when the ID is unknown."
  (or (find-page forest id)
      (error "unknown forest page" id)))

(define (describing-pages forest uri)
  "Return pages that declare they describe URI, in site order."
  (pages-with-ids
    forest
    (described-entity-page-ids (described-entity-relation forest) uri)))

(define (subtree-definitions forest)
  (composition:composition-subtree-definitions (composition forest)))

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
  (require-page forest id)
  (cached-rows forest `(outgoing-references ,id)
    (lambda ()
      (map (lambda (row) (apply make-reference row))
           (outgoing-reference-rows (reference-relation forest) id)))))

(define (outgoing-transclusions forest id)
  (require-page forest id)
  (composition:composition-outgoing-transclusions
    (composition forest) id))

(define (incoming-transclusions forest id)
  (require-page forest id)
  (composition:composition-incoming-transclusions
    (composition forest) id))

(define (transcluding-pages forest id)
  (cached-rows forest `(transcluding-pages ,id)
    (lambda ()
      (let ((source-ids
              (delete-duplicates
                (map composition:transclusion-source-tree-id
                     (incoming-transclusions forest id))
                equal?)))
        (pages-with-ids forest source-ids)))))

;; Context = every page in whose displayed content this page appears:
;; hosts of its transclusions and the page that defines it as a subtree.
(define (context-pages forest id)
  (cached-rows forest `(context-pages ,id)
    (lambda ()
      (let ((source-ids
              (delete-duplicates
                (append
                 (map composition:transclusion-source-tree-id
                      (incoming-transclusions forest id))
                 (map composition:subtree-definition-parent
                      (filter (lambda (definition)
                                (equal? (composition:subtree-definition-child
                                          definition)
                                        id))
                              (composition:composition-subtree-definitions
                                (composition forest)))))
                equal?)))
        (pages-with-ids forest source-ids)))))

(define (used-codepoints forest)
  "Every codepoint in the site's authored rendered text as a sorted character
list: site and page titles, document text, and reference labels. This is the
universe a text font must cover; built-in interface strings stay the caller's
concern. Math is counted through its plain-text rendering, a harmless superset
since MathJax draws it with its own fonts."
  (cached-rows forest 'used-codepoints
    (lambda ()
      (let ((codepoints (make-hash-table)))
        (for-each
          (lambda (text)
            (string-for-each
              (lambda (char) (hash-set! codepoints char #t))
              text))
          (cons (site-title (site forest))
            (append-map
              (lambda (page)
                (append (list (site-page-title page)
                              (content->plain-text
                                (site-page-document page)))
                        ;;     Analysis strips link text from the document;
                        ;;     the label is what rendering puts back. Rows
                        ;;     come straight from the relation: the cached
                        ;;     query helper would re-take the site lock.
                        (map (lambda (row)
                               (reference-label (apply make-reference row)))
                             (outgoing-reference-rows
                               (reference-relation forest)
                               (site-page-id page)))))
              (pages forest))))
        (sort (hash-map->list (lambda (char _) char) codepoints)
              (lambda (a b) (< (char->integer a) (char->integer b))))))))

(define (incoming-references forest id)
  (require-page forest id)
  (cached-rows forest `(incoming-references ,id)
    (lambda ()
      (map (lambda (row) (apply make-reference row))
           (incoming-reference-rows (reference-relation forest) id)))))

(define (backlinks forest id)
  (require-page forest id)
  (cached-rows forest `(backlink-pages ,id)
    (lambda ()
      (pages-with-ids
        forest (backlink-page-ids (reference-relation forest) id)))))

(define (common-backlinks forest left-id right-id)
  (for-each
    (lambda (id)
      (require-page forest id))
    (list left-id right-id))
  (pages-with-ids
    forest
    (common-backlink-page-ids (reference-relation forest) left-id right-id)))

(define (reachable-pages forest id)
  (require-page forest id)
  (pages-with-ids
    forest (reachable-page-ids (reference-relation forest) id)))

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
          (composition:composition-subtree-definitions (composition forest)))
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
              (composition:composition-outgoing-transclusions
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
                  (content-source-paths (site-page-document page)))
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
                  (let ((page (require-page forest id)))
                    (loop
                      (append
                       (map composition:transclusion-target-tree-id
                            (composition:composition-outgoing-transclusions
                              (composition forest) id))
                       (map composition:subtree-definition-child
                            (filter (lambda (definition)
                                      (equal? (composition:subtree-definition-parent
                                                definition)
                                            id))
                                    (composition:composition-subtree-definitions
                                      (composition forest))))
                       (cdr pending))
                      (cons id seen))))))))))

(define (page-source-paths forest page-id)
  "Return the tracked source closure for PAGE-ID in FOREST."
  (let* ((page (require-page forest page-id))
         (included (included-tree-ids forest page-id)))
    (if (any (lambda (id) (member id (query-page-ids forest) string=?))
             included)
        (source-paths forest)
        (delete-duplicates
          (append (module-paths forest)
                  (filter-map
                    (lambda (id)
                      (site-page-source (require-page forest id)))
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
                            (require-page forest
                              (reference-target-page-id reference))))
                        (outgoing-references forest tree-id)))
                    included))
          string=?))))
