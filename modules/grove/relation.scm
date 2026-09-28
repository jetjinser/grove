;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove relation)
  #:use-module (faster-minikanren mk-guile)
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
            relation-facts
            relationo))

(define-record-type <relation>
  (%make-relation facts by-source by-target)
  relation?
  (facts relation-facts)
  (by-source relation-by-source)
  (by-target relation-by-target))

(define (make-index facts key-procedure value-procedure)
  (let ((index (make-hash-table)))
    (for-each
      (lambda (fact)
        (let ((key (key-procedure fact))
              (value (value-procedure fact)))
          (hash-set! index key (cons value (hash-ref index key '())))))
      facts)
    index))

(define (make-relation facts)
  (unless (and (list? facts) (every pair? facts))
    (error "relation facts must be a list of pairs" facts))
  (let ((facts (delete-duplicates facts equal?)))
    (%make-relation facts
                    (make-index facts car cdr)
                    (make-index facts cdr car))))

(define (ground-term? term)
  (cond
    (((@@ (faster-minikanren mk-guile) var?) term) #f)
    ((pair? term)
     (and (ground-term? (car term))
          (ground-term? (cdr term))))
    (else #t)))

(define (membero value values)
  (conde
    ((fresh (rest)
       (== `(,value . ,rest) values)))
    ((fresh (head rest)
       (== `(,head . ,rest) values)
       (membero value rest)))))

(define (indexed-values index key)
  (reverse (hash-ref index key '())))

(define (relationo relation source target)
  (unless (relation? relation)
    (error "relationo requires a relation" relation))
  ((@@ (faster-minikanren mk-guile) project) (source target)
    (cond
      ((and (ground-term? source) (ground-term? target))
       (if (member target
                   (indexed-values (relation-by-source relation) source)
                   equal?)
           succeed
           fail))
      ((ground-term? source)
       (membero target
                (indexed-values (relation-by-source relation) source)))
      ((ground-term? target)
       (membero source
                (indexed-values (relation-by-target relation) target)))
      (else
       (membero (cons source target) (relation-facts relation))))))
