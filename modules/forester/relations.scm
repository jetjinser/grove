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
(define (make-forest-reference-relation rows)
  (make-relation rows #:arity 5))

(define-relation (referenceo relation source-page-id reference-id
                             target-page-id fragment label)
  (relationo relation source-page-id reference-id target-page-id
             fragment label))

(define-relation (incoming-referenceo relation target-page-id
                                       source-page-id reference-id)
  (exists (fragment label)
    (referenceo relation source-page-id reference-id target-page-id
                fragment label)))

(define (forest-incoming-page-reference-keys relation target-page-id)
  (query (source-page-id reference-id)
    (incoming-referenceo relation target-page-id
                         source-page-id reference-id)))

(define (forest-incoming-fragment-reference-keys
         relation target-page-id fragment-id)
  (query (source-page-id reference-id)
    (exists (label)
      (referenceo relation source-page-id reference-id target-page-id
                  fragment-id label))))

(define* (forest-incoming-reference-keys relation target-page-id
                                         #:optional fragment-id)
  (if fragment-id
      (forest-incoming-fragment-reference-keys
        relation target-page-id fragment-id)
      (forest-incoming-page-reference-keys relation target-page-id)))

(define (forest-outgoing-reference-ids relation source-page-id)
  (query (reference-id)
    (exists (target-page-id fragment label)
      (referenceo relation source-page-id reference-id target-page-id
                  fragment label))))

(define (forest-backlink-page-ids relation target-page-id)
  (delete-duplicates
    (query (source-page-id)
      (exists (reference-id fragment label)
        (referenceo relation source-page-id reference-id target-page-id
                    fragment label)))
    equal?))

(define (forest-common-backlink-page-ids relation left-page-id right-page-id)
  (delete-duplicates
    (query (source-page-id)
      (exists (left-reference-id left-fragment left-label
               right-reference-id right-fragment right-label)
        (all
          (referenceo relation source-page-id left-reference-id left-page-id
                      left-fragment left-label)
          (referenceo relation source-page-id right-reference-id right-page-id
                      right-fragment right-label))))
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
  (exists (reference-id fragment label)
    (referenceo relation source-page-id reference-id target-page-id
                fragment label)))

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
