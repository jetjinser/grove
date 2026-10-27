;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester relations)
  #:use-module (grove i18n)
  #:use-module (grove private relation)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-reference-relation
            incoming-reference-rows
            outgoing-reference-rows
            backlink-page-ids
            common-backlink-page-ids
            reachable-page-ids
            make-page-relation
            pageo
            page-metadatao
            make-described-entity-relation
            described-entityo
            described-entity-page-ids
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

;; Rows are (page-id uri): a tree's declared subject entities.
(define-relation-schema make-described-entity-relation described-entityo
  (page-id uri))

;; Rows are (node-id page-id parent-id kind position properties text
;; character-count codepoint-count plain-text?).
(define-relation-schema make-content-node-relation nodeo
  (node-id page-id parent-id kind position properties text
   character-count codepoint-count plain-text?))

;; Rows are (term-id page-id anchor display-text): one defining
;; occurrence of a term, collected from dfn-tagged anchored elements.
;; The term-id is the element's anchor - the corpus normalization
;; derives it from the term text, making the anchor the canonical key.
(define-relation-schema make-term-definition-relation term-definitiono
  (term-id page-id anchor display-text))

;; Rows are (term-id source-page-id resolved?): a fragment link aimed
;; at an anchor that resolves nowhere, or an element marked with a
;; term property.  Marked mentions record whether the definition
;; relation resolved them.
(define-relation-schema make-term-mention-relation term-mentiono
  (term-id source-page-id resolved?))

(defrel (list-membero value values)
  (conde
    ((fresh (tail)
       (== `(,value . ,tail) values)))
    ((fresh (head tail)
       (== `(,head . ,tail) values)
       (=/= value head)
       (list-membero value tail)))))

(defrel (node-propertyo relation node-id key value)
  (fresh (properties entry)
    (nodeo relation
           (node-id node-id)
           (properties properties))
    (list-membero entry properties)
    (== `(,key . ,value) entry)))

(defrel (source-locationo relation node-id file line column)
  (fresh (properties location)
    (nodeo relation (node-id node-id) (properties properties))
    (list-membero `(source-location . ,location) properties)
    (list-membero `(file . ,file) location)
    (list-membero `(line . ,line) location)
    (list-membero `(column . ,column) location)))

(defrel (page-metadatao relation page-id key value)
  (fresh (metadata entry)
    (pageo relation
           (page-id page-id)
           (metadata metadata))
    (list-membero entry metadata)
    (== `(,key . ,value) entry)))

(define (described-entity-page-ids relation uri)
  (delete-duplicates
    (map car (relation-index-rows relation 1 uri))
    string=?))

(defrel (paragrapho relation node-id)
  (nodeo relation (node-id node-id) (kind 'paragraph)))

(defrel (plain-text-paragrapho relation node-id)
  (nodeo relation
         (node-id node-id)
         (kind 'paragraph)
         (plain-text? #t)))

(defrel (math-nodeo relation node-id)
  (nodeo relation (node-id node-id) (kind 'math)))

(defrel (childo relation parent-id child-id)
  (nodeo relation
         (node-id child-id)
         (parent-id parent-id)))

(defrel (parento relation child-id parent-id)
  (nodeo relation
         (node-id child-id)
         (parent-id parent-id)))

(defrel (ancestoro relation ancestor-id descendant-id)
  (conde
    ((parento relation descendant-id ancestor-id))
    ((fresh (parent-id)
       (parento relation descendant-id parent-id)
       (ancestoro relation ancestor-id parent-id)))))

(defrel (descendanto relation ancestor-id descendant-id)
  (conde
    ((childo relation ancestor-id descendant-id))
    ((fresh (parent-id)
       (childo relation ancestor-id parent-id)
       (descendanto relation parent-id descendant-id)))))

(defrel (math-containing-paragrapho relation paragraph-id)
  (fresh (math-id)
    (math-nodeo relation math-id)
    (ancestoro relation paragraph-id math-id)
    (paragrapho relation paragraph-id)))

(defrel (shorter-than-o relation node-id maximum)
  (fresh (character-count)
    (nodeo relation
           (node-id node-id)
           (character-count character-count))
    (project (character-count maximum)
      (if (and (integer? character-count)
               (integer? maximum)
               (< character-count maximum))
          succeed
          fail))))

(defrel (contains-texto relation node-id needle)
  (fresh (text)
    (nodeo relation (node-id node-id) (text text))
    (project (text needle)
      (if (and (string? text)
               (string? needle)
               (string-contains text needle))
          succeed
          fail))))

(defrel (topic-ido relation node-id topic-id)
  (fresh (stored-id)
    (node-propertyo relation node-id 'id stored-id)
    ;; Ground selectors match canonically caseless; enumerated answers
    ;; stay canonical.
    (project (stored-id topic-id)
      (if (string? topic-id)
          (== (match-key stored-id) (match-key topic-id))
          (== stored-id topic-id)))))

(defrel (topic-membero relation topic-id page-id label)
  (fresh (node-id kind)
    (nodeo relation (node-id node-id) (page-id page-id)
           (kind kind) (text label))
    (conde ((== kind 'tag)) ((== kind 'taxon)))
    (topic-ido relation node-id topic-id)))

(define* (topic-page-ids relation topic-id #:optional kind)
  (delete-duplicates
    (if kind
        (run* (page-id)
          (fresh (node-id)
            (nodeo relation
                   (node-id node-id)
                   (kind kind)
                   (page-id page-id))
            (topic-ido relation node-id topic-id)))
        (run* (page-id)
          (fresh (label)
            (topic-membero relation topic-id page-id label))))
    equal?))

(defrel (incoming-reference-rowo relation fixed-target-page-id
                                          source-page-id reference-id
                                          target-page-id label)
  (referenceo relation
              (source-page-id source-page-id)
              (reference-id reference-id)
              (target-page-id target-page-id)
              (label label))
  (== fixed-target-page-id target-page-id))

(defrel (outgoing-reference-rowo relation fixed-source-page-id
                                          source-page-id reference-id
                                          target-page-id label)
  (referenceo relation
              (source-page-id source-page-id)
              (reference-id reference-id)
              (target-page-id target-page-id)
              (label label))
  (== fixed-source-page-id source-page-id))

(define (incoming-reference-rows relation target-page-id)
  (relation-index-rows relation 2 target-page-id))

(define (outgoing-reference-rows relation source-page-id)
  (relation-index-rows relation 0 source-page-id))

(define (backlink-page-ids relation target-page-id)
  (delete-duplicates
    (map car (incoming-reference-rows relation target-page-id))
    equal?))

(define (common-backlink-page-ids relation left-page-id right-page-id)
  (let ((right-sources (make-hash-table)))
    (for-each
      (lambda (row) (hash-set! right-sources (car row) #t))
      (incoming-reference-rows relation right-page-id))
    (delete-duplicates
      (filter (lambda (source) (hash-ref right-sources source))
              (map car (incoming-reference-rows relation left-page-id)))
      equal?)))

;; One term's defining occurrences as (page-id anchor display-text),
;; in document order of collection. The relation's term-id column is
;; the query key and projects out of the rows.
(define (term-definition-rows relation term-id)
  (map cdr (relation-index-rows relation 0 term-id)))

;; Terms whose defining occurrences disagree: more than one defining
;; page, or display texts that differ - an in-passing definition that
;; drifted from the canonical one.  Returns (term-id (page-id anchor
;; display-text) ...) in collection order; the anchor stays in the
;; projection so reports can deep-link to each occurrence.
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
                   (reverse (cons (list (cadr row) (caddr row) (cadddr row))
                                  (hash-ref by-term (car row) '())))))
      (relation-facts relation))
    (sort
      (filter (lambda (entry) (conflict? (cdr entry)))
              (hash-map->list cons by-term))
      (lambda (left right) (string<? (car left) (car right))))))

;; One term's mentioning pages as (source-page-id resolved?).
(define (term-mention-rows relation term-id)
  (map cdr (relation-index-rows relation 0 term-id)))

;; Term-ids the corpus points at but nothing defines, each with its
;; mentioning pages - the definition backlog.  A mention counts as
;; resolved only when it is marked with a term property AND the
;; definition relation resolves it; unresolved fragment links and
;; unresolved marked mentions both stay out of this report, sorted by
;; term-id.
(define (undefined-term-mentions definition-relation mention-relation)
  (let ((defined (make-hash-table)))
    (for-each
      (lambda (row) (hash-set! defined (car row) #t))
      (relation-facts definition-relation))
    (let ((by-term (make-hash-table)))
      (for-each
        (lambda (row)
          (unless (or (hash-ref defined (car row)) (caddr row))
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

(defrel (not-membero value values)
  (conde
    ((== '() values))
    ((fresh (head tail)
       (== `(,head . ,tail) values)
       (=/= value head)
       (not-membero value tail)))))

(defrel (page-linko relation source-page-id target-page-id)
  (referenceo relation
              (source-page-id source-page-id)
              (target-page-id target-page-id)))

(defrel (reachableo relation source-page-id target-page-id)
  (reachable-step relation source-page-id target-page-id
                  (list source-page-id)))

(defrel (reachable-step relation source-page-id target-page-id seen)
  (conde
    ((page-linko relation source-page-id target-page-id)
     (not-membero target-page-id seen))
    ((fresh (middle-page-id)
       (page-linko relation source-page-id middle-page-id)
       (not-membero middle-page-id seen)
       (reachable-step relation middle-page-id target-page-id
                       (cons middle-page-id seen))))))

(define (reachable-page-ids relation source-page-id)
  ;;     Breadth-first closure over the source index. The solver's
  ;;     path enumeration is exponential in branchy graphs; the target
  ;;     set is what callers want.
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
                             (map caddr (outgoing-reference-rows relation id))
                             equal?))))
            (loop (append targets (cdr pending))
                  (append (reverse targets) seen)
                  (begin
                    (for-each
                      (lambda (target) (hash-set! seen-set target #t))
                      targets)
                    seen-set))))))
