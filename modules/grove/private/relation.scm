;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private relation)
  #:use-module (grove relation)
  #:export (relation-index-rows
            warm-relation-indexes!))

;; Keep materialized indexes behind the public relation abstraction. Domain
;; modules may use these helpers for measured hot paths without depending on
;; the indexes' representation.
(define %relation-indexes
  (@@ (grove relation) %relation-indexes))

(define (relation-index-rows relation column key)
  (unless (relation? relation)
    (error "not a relation" relation))
  (unless (and (integer? column)
               (<= 0 column)
               (< column (relation-arity relation)))
    (error "relation index column is out of range" column))
  (hash-ref (force (list-ref (%relation-indexes relation) column)) key '()))

(define (warm-relation-indexes! relation)
  (unless (relation? relation)
    (error "not a relation" relation))
  (for-each force (%relation-indexes relation)))
