;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester relations)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:export (make-forest-reference-relation
            forest-incoming-reference-keys
            forest-outgoing-reference-ids
            forest-backlink-page-ids
            forest-common-backlink-page-ids
            forest-reachable-page-ids))

;; Rows are (source-page-id local-reference-id target-page-id fragment label).
(define-relation-schema make-forest-reference-relation referenceo
  (source-page-id reference-id target-page-id fragment label))

(define-relation (incoming-referenceo relation target-page-id
                                       source-page-id reference-id)
  (referenceo relation
              (source-page-id source-page-id)
              (reference-id reference-id)
              (target-page-id target-page-id)))

(define (forest-incoming-page-reference-keys relation target-page-id)
  (query (source-page-id reference-id)
    (incoming-referenceo relation target-page-id
                         source-page-id reference-id)))

(define (forest-incoming-fragment-reference-keys
         relation target-page-id fragment-id)
  (query (source-page-id reference-id)
    (referenceo relation
                (source-page-id source-page-id)
                (reference-id reference-id)
                (target-page-id target-page-id)
                (fragment fragment-id))))

(define* (forest-incoming-reference-keys relation target-page-id
                                         #:optional fragment-id)
  (if fragment-id
      (forest-incoming-fragment-reference-keys
        relation target-page-id fragment-id)
      (forest-incoming-page-reference-keys relation target-page-id)))

(define (forest-outgoing-reference-ids relation source-page-id)
  (query (reference-id)
    (referenceo relation
                (source-page-id source-page-id)
                (reference-id reference-id))))

(define (forest-backlink-page-ids relation target-page-id)
  (delete-duplicates
    (query (source-page-id)
      (referenceo relation
                  (source-page-id source-page-id)
                  (target-page-id target-page-id)))
    equal?))

(define (forest-common-backlink-page-ids relation left-page-id right-page-id)
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

(define (forest-reachable-page-ids relation source-page-id)
  (delete-duplicates
    (query (target-page-id)
      (reachableo relation source-page-id target-page-id))
    equal?))
