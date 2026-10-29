;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private site builder)
  #:use-module (grove private procedure)
  #:use-module (grove i18n)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove private html-style)
  #:use-module (grove private string-fun)
  #:use-module (grove ir)
  #:use-module (grove render plain-text)
  #:use-module (grove site)
  #:use-module (grove private relation)
  #:use-module (grove private project timing)
  #:use-module ((forester private trees) #:prefix tree-pass:)
  #:use-module (forester private content-index)
  #:use-module (forester private entity-uri)
  #:use-module (forester private site model)
  #:use-module (forester private site queries)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:export (make-forest))

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
             (tree-pass:walk-owned-content document
               (tree-pass:element-kind? 'math) #:postorder? #t)))
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
 marker - dfn for a defining occurrence, say. Later markers override earlier
 ones, matching Grove's HTML renderer and MERGE-CONTENT-STYLE."
  (content-style-html-tag-name (content-element-style element)))


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
               (composition:composition-subtree-definitions composition))
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

A page's scope is the set of anchors it declares. Content pulled in through
transclusions is not scanned, and author anchors may collide with the generated
tree-N position anchors; both are known limitations of global anchors."
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

(define (normalize-page-metadata page)
  (let* ((metadata (site-page-metadata page))
         (locale (assq-ref metadata 'locale))
         (describes (assq-ref metadata 'describes)))
    (when describes
      (unless (and (list? describes)
                   (every absolute-entity-uri? describes))
        (error (format #f
                 "Forester page describes values must be absolute URIs (~a)"
                 (or (site-page-source page) (site-page-id page)))))
      (when (not (= (length describes)
                    (length (delete-duplicates describes string=?))))
        (error (format #f "Forester page repeats a describes URI (~a)"
                       (or (site-page-source page) (site-page-id page))))))
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
                #:source (site-page-source page)
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
  (unless (or (not timings) (procedure-accepts-arguments? timings 2))
    (error "forest timings callback must accept a phase and duration or be #f"
           timings))
  (set! site
    (make-site (map normalize-page-metadata (site-pages site))
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
                        (tree-pass:walk-owned-content
                          (site-page-document source-page)
                          (tree-pass:element-kind? 'link) #:postorder? #t)))
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
                             (content->plain-text element))))
                   elements (iota (length elements) 1))))
             pages)))
         (transclusion-index
           (measure 'transclusions (map
             (lambda (source-tree)
               (let* ((source-id (site-page-id source-tree))
                      (elements
                        (tree-pass:walk-owned-content
                          (site-page-document source-tree)
                          (tree-pass:element-kind? 'transclude)
                          #:postorder? #t)))
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
             (lambda () (build-content-node-index pages))
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
                       (append
                         (tree-pass:walk-owned-content
                           (site-page-document source-page)
                           (tree-pass:element-kind? 'link) #:postorder? #t)
                         (tree-pass:walk-owned-content
                           (site-page-document source-page)
                           (tree-pass:element-kind? 'query-link)
                           #:postorder? #t)))))
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
                    (tree-pass:walk-owned-content
                      (site-page-document source-page)
                      (tree-pass:element-kind? 'link) #:postorder? #t))))
              pages))
         (_ (validate-tree-cycles! pages
              (lambda (page)
                (composition:composition-target-ids
                  composition (site-page-id page))))))
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

