;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester relations)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-reference-relation
            incoming-reference-keys
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
            childo
            descendanto
            paragrapho
            plain-text-paragrapho
            math-nodeo
            math-containing-paragrapho
            shorter-than-o
            contains-texto
            topic-membero
            topic-page-ids))

;; Rows are (source-page-id local-reference-id target-page-id fragment label).
(define-relation-schema make-reference-relation referenceo
  (source-page-id reference-id target-page-id fragment label))

;; Rows are (page-id title metadata).
(define-relation-schema make-page-relation pageo
  (page-id title metadata))

;; Rows are (node-id page-id parent-id kind position properties text
;; character-count plain-text?).
(define-relation-schema make-content-node-relation nodeo
  (node-id page-id parent-id kind position properties text
   character-count plain-text?))

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

(define-relation (descendanto relation ancestor-id descendant-id)
  (either
    ((childo relation ancestor-id descendant-id))
    ((exists (parent-id)
       (childo relation ancestor-id parent-id)
       (descendanto relation parent-id descendant-id)))))

(define-relation (math-containing-paragrapho relation paragraph-id)
  (paragrapho relation paragraph-id)
  (exists (math-id)
    (descendanto relation paragraph-id math-id)
    (math-nodeo relation math-id)))

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

(define-relation (topic-membero relation topic-id page-id label)
  (exists (node-id kind)
    (nodeo relation
           (node-id node-id)
           (page-id page-id)
           (kind kind)
           (text label))
    (either
      ((== 'tag kind)
       (node-propertyo relation node-id 'id topic-id))
      ((== 'taxon kind)
       (node-propertyo relation node-id 'id topic-id)))))

(define* (topic-page-ids relation topic-id #:optional kind)
  (delete-duplicates
    (if kind
        (query (page-id)
          (exists (node-id)
            (nodeo relation
                   (node-id node-id)
                   (kind kind)
                   (page-id page-id))
            (node-propertyo relation node-id 'id topic-id)))
        (query (page-id)
          (exists (label)
            (topic-membero relation topic-id page-id label))))
    equal?))

(define-relation (incoming-referenceo relation target-page-id
                                       source-page-id reference-id)
  (referenceo relation
              (source-page-id source-page-id)
              (reference-id reference-id)
              (target-page-id target-page-id)))

(define (incoming-page-reference-keys relation target-page-id)
  (query (source-page-id reference-id)
    (incoming-referenceo relation target-page-id
                         source-page-id reference-id)))

(define (incoming-fragment-reference-keys
         relation target-page-id fragment-id)
  (query (source-page-id reference-id)
    (referenceo relation
                (source-page-id source-page-id)
                (reference-id reference-id)
                (target-page-id target-page-id)
                (fragment fragment-id))))

(define* (incoming-reference-keys relation target-page-id
                                         #:optional fragment-id)
  (if fragment-id
      (incoming-fragment-reference-keys
        relation target-page-id fragment-id)
      (incoming-page-reference-keys relation target-page-id)))

(define (outgoing-reference-ids relation source-page-id)
  (query (reference-id)
    (referenceo relation
                (source-page-id source-page-id)
                (reference-id reference-id))))

(define (backlink-page-ids relation target-page-id)
  (delete-duplicates
    (query (source-page-id)
      (referenceo relation
                  (source-page-id source-page-id)
                  (target-page-id target-page-id)))
    equal?))

(define (common-backlink-page-ids relation left-page-id right-page-id)
  (delete-duplicates
    (query (source-page-id)
      (all
        (referenceo relation
                    (source-page-id source-page-id)
                    (target-page-id left-page-id))
        (referenceo relation
                    (source-page-id source-page-id)
                    (target-page-id right-page-id))))
    equal?))

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
  (delete-duplicates
    (query (target-page-id)
      (reachableo relation source-page-id target-page-id))
    equal?))
