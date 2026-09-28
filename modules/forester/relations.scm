;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester relations)
  #:use-module (grove relation)
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

(define (forest-incoming-page-reference-keys relation target-page-id)
  (run* (source-page-id reference-id)
    (fresh (fragment label)
      (relationo relation
                 source-page-id reference-id target-page-id fragment label))))

(define (forest-incoming-fragment-reference-keys
         relation target-page-id fragment-id)
  (run* (source-page-id reference-id)
    (fresh (label)
      (relationo relation
                 source-page-id reference-id target-page-id fragment-id label))))

(define* (forest-incoming-reference-keys relation target-page-id
                                         #:optional fragment-id)
  (if fragment-id
      (forest-incoming-fragment-reference-keys
        relation target-page-id fragment-id)
      (forest-incoming-page-reference-keys relation target-page-id)))

(define (forest-outgoing-reference-ids relation source-page-id)
  (run* (reference-id)
    (fresh (target-page-id fragment label)
      (relationo relation
                 source-page-id reference-id target-page-id fragment label))))

(define (forest-backlink-page-ids relation target-page-id)
  (delete-duplicates
    (run* (source-page-id)
      (fresh (reference-id fragment label)
        (relationo relation source-page-id reference-id target-page-id
                   fragment label)))
    equal?))

(define (forest-common-backlink-page-ids relation left-page-id right-page-id)
  (delete-duplicates
    (run* (source-page-id)
      (fresh (left-reference-id left-fragment left-label
              right-reference-id right-fragment right-label)
        (relationo relation source-page-id left-reference-id left-page-id
                   left-fragment left-label)
        (relationo relation source-page-id right-reference-id right-page-id
                   right-fragment right-label)))
    equal?))

(define (not-membero value values)
  (conde
    ((== '() values))
    ((fresh (head tail)
       (== `(,head . ,tail) values)
       (=/= value head)
       (not-membero value tail)))))

(define (referenceo relation source-page-id target-page-id)
  (fresh (reference-id fragment label)
    (relationo relation source-page-id reference-id target-page-id
               fragment label)))

(define (reachableo relation source-page-id target-page-id)
  (reachable-step relation source-page-id target-page-id
                  (list source-page-id)))

(define (reachable-step relation source-page-id target-page-id seen)
  (conde
    ((referenceo relation source-page-id target-page-id)
     (not-membero target-page-id seen))
    ((fresh (middle-page-id)
       (referenceo relation source-page-id middle-page-id)
       (not-membero middle-page-id seen)
       (reachable-step relation middle-page-id target-page-id
                       (cons middle-page-id seen))))))

(define (forest-reachable-page-ids relation source-page-id)
  (delete-duplicates
    (run* (target-page-id)
      (reachableo relation source-page-id target-page-id))
    equal?))
