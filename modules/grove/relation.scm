;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove relation)
  #:use-module (faster-minikanren mk-guile)
  #:use-module (grove private minikanren)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:re-export (run
               run*
               defrel
               ==
               =/=
               fresh
               conde
               symbolo
               numbero
               stringo
               absento
               succeed
               fail
               matche)
  #:export (relation?
            make-relation
            relation-arity
            relation-facts
            relationo))

(define-record-type <relation>
  (%make-relation arity facts indexes)
  relation?
  (arity relation-arity)
  (facts relation-facts)
  (indexes relation-indexes))

(define (make-index facts position)
  (let ((index (make-hash-table)))
    (for-each
      (lambda (fact)
        (let ((key (list-ref fact position)))
          (hash-set! index key (cons fact (hash-ref index key '())))))
      facts)
    index))

(define (unique-facts facts)
  (let ((seen (make-hash-table)))
    (let loop ((facts facts) (unique '()))
      (if (null? facts)
          (reverse unique)
          (let ((fact (car facts)))
            (if (hash-ref seen fact #f)
                (loop (cdr facts) unique)
                (begin
                  (hash-set! seen fact #t)
                  (loop (cdr facts) (cons fact unique)))))))))

(define (vector-free-term? term)
  (cond
    ((vector? term) #f)
    ((pair? term)
     (and (vector-free-term? (car term))
          (vector-free-term? (cdr term))))
    (else #t)))

(define* (make-relation facts #:key (arity 2))
  (unless (and (integer? arity) (positive? arity))
    (error "relation arity must be a positive integer" arity))
  (unless (and (list? facts)
               (every (lambda (fact)
                        (and (list? fact) (= (length fact) arity)))
                      facts))
    (error "relation facts must be a list of rows matching the relation arity"
           arity facts))
  (unless (every (lambda (fact) (every vector-free-term? fact)) facts)
    (error "relation facts cannot contain vectors; miniKanren uses them for logic variables" facts))
  (let* ((facts (unique-facts facts))
         ;; A content relation has nine columns; most queries bind only one or
         ;; two. Build each index on first use and share it within this snapshot.
         (indexes (map (lambda (position) (delay (make-index facts position)))
                       (iota arity))))
    (%make-relation arity facts indexes)))

(define (membero value values)
  (conde
    ((fresh (rest)
       (== `(,value . ,rest) values)))
    ((fresh (head rest)
       (== `(,head . ,rest) values)
      (membero value rest)))))

(define (indexed-values index key)
  (reverse (hash-ref index key '())))

(define (relationo/ground relation terms)
  (unless (relation? relation)
    (error "relationo requires a relation" relation))
  (unless (= (length terms) (relation-arity relation))
    (error "relation goal arity does not match relation"
           (relation-arity relation) (length terms)))
  (let find-candidates ((remaining terms)
                        (indexes (relation-indexes relation)))
    (if (null? remaining)
        (membero terms (relation-facts relation))
        (if (ground-term? (car remaining))
            (membero terms
                     (indexed-values (force (car indexes)) (car remaining)))
            (find-candidates (cdr remaining)
                             (cdr indexes))))))

(define-syntax relationo
  (syntax-rules ()
    ((_ relation argument ...)
     (fresh (row)
       (== row (list argument ...))
       (with-projected-terms (row)
         (relationo/ground relation row))))))
