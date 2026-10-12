;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester relations)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-reference-relation
            incoming-reference-rows
            incoming-reference-keys
            outgoing-reference-rows
            outgoing-reference-ids
            backlink-page-ids
            common-backlink-page-ids
            reachable-page-ids
            make-page-relation
            pageo
            page-metadatao
            make-content-node-relation
            nodeo
            node-propertyo
            source-locationo
            childo
            ancestoro
            descendanto
            paragrapho
            plain-text-paragrapho
            math-nodeo
            math-containing-paragrapho
            shorter-than-o
            contains-texto
            topic-membero
            topic-page-ids
            make-term-definition-relation
            term-definitiono
            term-definition-rows
            term-definition-conflicts
            make-term-mention-relation
            term-mentiono
            term-mention-rows
            undefined-term-mentions))

;; Rows are (source-page-id local-reference-id target-page-id label).
(define-relation-schema make-reference-relation referenceo
  (source-page-id reference-id target-page-id label))

;; Rows are (page-id title metadata).
(define-relation-schema make-page-relation pageo
  (page-id title metadata))

;; Rows are (node-id page-id parent-id kind position properties text
;; character-count plain-text?).
(define-relation-schema make-content-node-relation nodeo
  (node-id page-id parent-id kind position properties text
   character-count plain-text?))

;; Rows are (term-id page-id anchor display-text): one defining
;; occurrence of a term, collected from dfn-tagged anchored elements.
;; The term-id is the element's anchor - the corpus normalization
;; derives it from the term text, making the anchor the canonical key.
(define-relation-schema make-term-definition-relation term-definitiono
  (term-id page-id anchor display-text))

;; Rows are (term-id source-page-id): a fragment link aimed at an
;; anchor that resolves nowhere - a term the corpus points at but
;; nothing defines yet.
(define-relation-schema make-term-mention-relation term-mentiono
  (term-id source-page-id))

(define-relation (list-membero value values)
  (either
    ((exists (tail)
       (== `(,value . ,tail) values)))
    ((exists (head tail)
       (== `(,head . ,tail) values)
       (=/= value head)
       (list-membero value tail)))))

(define-relation (node-propertyo relation node-id key value)
  (exists (properties entry)
    (nodeo relation
           (node-id node-id)
           (properties properties))
    (list-membero entry properties)
    (== `(,key . ,value) entry)))

(define-relation (source-locationo relation node-id file line column)
  (exists (properties location)
    (nodeo relation (node-id node-id) (properties properties))
    (list-membero `(source-location . ,location) properties)
    (list-membero `(file . ,file) location)
    (list-membero `(line . ,line) location)
    (list-membero `(column . ,column) location)))

(define-relation (page-metadatao relation page-id key value)
  (exists (metadata entry)
    (pageo relation
           (page-id page-id)
           (metadata metadata))
    (list-membero entry metadata)
    (== `(,key . ,value) entry)))

(define-relation (paragrapho relation node-id)
  (nodeo relation (node-id node-id) (kind 'paragraph)))

(define-relation (plain-text-paragrapho relation node-id)
  (nodeo relation
         (node-id node-id)
         (kind 'paragraph)
         (plain-text? #t)))

(define-relation (math-nodeo relation node-id)
  (nodeo relation (node-id node-id) (kind 'math)))

(define-relation (childo relation parent-id child-id)
  (nodeo relation
         (node-id child-id)
         (parent-id parent-id)))

(define-relation (parento relation child-id parent-id)
  (nodeo relation
         (node-id child-id)
         (parent-id parent-id)))

(define-relation (ancestoro relation ancestor-id descendant-id)
  (either
    ((parento relation descendant-id ancestor-id))
    ((exists (parent-id)
       (parento relation descendant-id parent-id)
       (ancestoro relation ancestor-id parent-id)))))

(define-relation (descendanto relation ancestor-id descendant-id)
  (either
    ((childo relation ancestor-id descendant-id))
    ((exists (parent-id)
       (childo relation ancestor-id parent-id)
       (descendanto relation parent-id descendant-id)))))

(define-relation (math-containing-paragrapho relation paragraph-id)
  (exists (math-id)
    (math-nodeo relation math-id)
    (ancestoro relation paragraph-id math-id)
    (paragrapho relation paragraph-id)))

(define-relation (shorter-than-o relation node-id maximum)
  (exists (character-count)
    (nodeo relation
           (node-id node-id)
           (character-count character-count))
    (project (character-count maximum)
      (if (and (integer? character-count)
               (integer? maximum)
               (< character-count maximum))
          succeed
          fail))))

(define-relation (contains-texto relation node-id needle)
  (exists (text)
    (nodeo relation (node-id node-id) (text text))
    (project (text needle)
      (if (and (string? text)
               (string? needle)
               (string-contains text needle))
          succeed
          fail))))

(define-relation (topic-ido relation node-id topic-id)
  (exists (stored-id)
    (node-propertyo relation node-id 'id stored-id)
    ;; Ground selectors are case-insensitive; enumerated answers are canonical.
    (project (topic-id)
      (== stored-id (if (string? topic-id) (string-downcase topic-id) topic-id)))))

(define-relation (topic-membero relation topic-id page-id label)
  (exists (node-id kind)
    (nodeo relation (node-id node-id) (page-id page-id)
           (kind kind) (text label))
    (either ((== kind 'tag)) ((== kind 'taxon)))
    (topic-ido relation node-id topic-id)))

(define* (topic-page-ids relation topic-id #:optional kind)
  (delete-duplicates
    (if kind
        (query (page-id)
          (exists (node-id)
            (nodeo relation
                   (node-id node-id)
                   (kind kind)
                   (page-id page-id))
            (topic-ido relation node-id topic-id)))
        (query (page-id)
          (exists (label)
            (topic-membero relation topic-id page-id label))))
    equal?))

(define-relation (incoming-reference-rowo relation fixed-target-page-id
                                          source-page-id reference-id
                                          target-page-id label)
  (all
    (referenceo relation
                (source-page-id source-page-id)
                (reference-id reference-id)
                (target-page-id target-page-id)
                (label label))
    (== fixed-target-page-id target-page-id)))

(define-relation (outgoing-reference-rowo relation fixed-source-page-id
                                          source-page-id reference-id
                                          target-page-id label)
  (all
    (referenceo relation
                (source-page-id source-page-id)
                (reference-id reference-id)
                (target-page-id target-page-id)
                (label label))
    (== fixed-source-page-id source-page-id)))

;; Reference queries read the materialized column indexes that
;; make-relation builds lazily per column. Solving the same rows
;; through the solver costs ~0.2s per query on a large site; an index
;; lookup is constant.
(define (reference-column-index relation position)
  (force (list-ref (relation-indexes relation) position)))

(define (incoming-reference-rows relation target-page-id)
  (hash-ref (reference-column-index relation 2) target-page-id '()))

(define (incoming-reference-keys relation target-page-id)
  (map (lambda (row) (list (car row) (cadr row)))
       (incoming-reference-rows relation target-page-id)))

(define (outgoing-reference-rows relation source-page-id)
  (hash-ref (reference-column-index relation 0) source-page-id '()))

(define (outgoing-reference-ids relation source-page-id)
  (map cadr (outgoing-reference-rows relation source-page-id)))

(define (backlink-page-ids relation target-page-id)
  (delete-duplicates
    (map car (hash-ref (reference-column-index relation 2)
                       target-page-id '()))
    equal?))

(define (common-backlink-page-ids relation left-page-id right-page-id)
  (let ((right-sources (make-hash-table)))
    (for-each
      (lambda (row) (hash-set! right-sources (car row) #t))
      (hash-ref (reference-column-index relation 2) right-page-id '()))
    (delete-duplicates
      (filter (lambda (source) (hash-ref right-sources source))
              (map car (hash-ref (reference-column-index relation 2)
                                 left-page-id '())))
      equal?)))

;; Term queries read the same materialized column indexes.
(define (term-column-index relation position)
  (force (list-ref (relation-indexes relation) position)))

;; One term's defining occurrences as (page-id anchor display-text),
;; in document order of collection. The relation's term-id column is
;; the query key and projects out of the rows.
(define (term-definition-rows relation term-id)
  (map cdr (hash-ref (term-column-index relation 0) term-id '())))

;; Terms whose defining occurrences disagree: more than one defining
;; page, or display texts that differ - an in-passing definition that
;; drifted from the canonical one. Returns (term-id (page-id
;; display-text) ...) sorted by term-id.
(define (term-definition-conflicts relation)
  (define (conflict? entries)
    (or (> (length (delete-duplicates (map car entries) string=?)) 1)
        (> (length (delete-duplicates (map cadr entries) string=?)) 1)))
  (let ((by-term (make-hash-table)))
    (for-each
      (lambda (row)
        (hash-set! by-term (car row)
                   ;;     Keep collection order: the glossary's
                   ;;     canonical entry precedes in-passing uses.
                   (reverse (cons (list (cadr row) (cadddr row))
                                  (hash-ref by-term (car row) '())))))
      (relation-facts relation))
    (sort
      (filter (lambda (entry) (conflict? (cdr entry)))
              (hash-map->list cons by-term))
      (lambda (left right) (string<? (car left) (car right))))))

;; One term's mentioning pages.
(define (term-mention-rows relation term-id)
  (map cadr (hash-ref (term-column-index relation 0) term-id '())))

;; Term-ids the corpus points at but nothing defines, each with its
;; mentioning pages - the definition backlog. Sorted by term-id.
(define (undefined-term-mentions definition-relation mention-relation)
  (let ((defined (make-hash-table)))
    (for-each
      (lambda (row) (hash-set! defined (car row) #t))
      (relation-facts definition-relation))
    (let ((by-term (make-hash-table)))
      (for-each
        (lambda (row)
          (unless (hash-ref defined (car row))
            (hash-set! by-term (car row)
                       (cons (cadr row)
                             (hash-ref by-term (car row) '())))))
        (relation-facts mention-relation))
      (sort
        (map (lambda (entry)
               (list (car entry)
                     (sort (delete-duplicates (cdr entry) string=?)
                           string<?)))
             (hash-map->list cons by-term))
        (lambda (left right) (string<? (car left) (car right)))))))

(define-relation (not-membero value values)
  (either
    ((== '() values))
    ((exists (head tail)
       (all
         (== `(,head . ,tail) values)
         (=/= value head)
         (not-membero value tail))))))

(define-relation (page-linko relation source-page-id target-page-id)
  (referenceo relation
              (source-page-id source-page-id)
              (target-page-id target-page-id)))

(define-relation (reachableo relation source-page-id target-page-id)
  (reachable-step relation source-page-id target-page-id
                  (list source-page-id)))

(define-relation (reachable-step relation source-page-id target-page-id seen)
  (either
    ((page-linko relation source-page-id target-page-id)
     (not-membero target-page-id seen))
    ((exists (middle-page-id)
       (all
         (page-linko relation source-page-id middle-page-id)
         (not-membero middle-page-id seen)
         (reachable-step relation middle-page-id target-page-id
                         (cons middle-page-id seen)))))))

(define (reachable-page-ids relation source-page-id)
  ;;     Breadth-first closure over the source index. The solver's
  ;;     path enumeration is exponential in branchy graphs; the target
  ;;     set is what callers want.
  (let ((source-index (reference-column-index relation 0)))
    (let loop ((pending (list source-page-id))
               (seen (list source-page-id))
               (seen-set (let ((table (make-hash-table)))
                           (hash-set! table source-page-id #t)
                           table)))
      (if (null? pending)
          (reverse
            (filter (lambda (id) (not (equal? id source-page-id)))
                    seen))
          (let* ((id (car pending))
                 (targets
                   (filter (lambda (target)
                             (not (hash-ref seen-set target)))
                           (delete-duplicates
                             (map caddr (hash-ref source-index id '()))
                             equal?))))
            (loop (append targets (cdr pending))
                  (append (reverse targets) seen)
                  (begin
                    (for-each
                      (lambda (target) (hash-set! seen-set target #t))
                      targets)
                    seen-set)))))))
