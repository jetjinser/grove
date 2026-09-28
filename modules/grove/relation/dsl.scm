;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove relation dsl)
  #:use-module (grove relation)
  #:re-export (relation?
               make-relation
               relation-arity
               relation-facts
               relationo
               run
               run*
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
  #:export (define-relation
            exists
            all
            either
            query))

;; Define named, possibly recursive relations using faster-minikanren's
;; standard suspension behavior for multi-goal relations.
(define-syntax define-relation
  (syntax-rules ()
    ((_ (name argument ...) goal ...)
     (defrel (name argument ...) goal ...))))

;; Readable aliases for the standard miniKanren goal combinators.
(define-syntax exists
  (syntax-rules ()
    ((_ (variable ...) goal ...)
     (fresh (variable ...) goal ...))))

(define-syntax all
  (syntax-rules ()
    ((_ goal ...)
     (fresh () goal ...))))

(define-syntax either
  (syntax-rules ()
    ((_ (goal ...) ...)
     (conde (goal ...) ...))))

(define-syntax query
  (syntax-rules ()
    ((_ (answer ...) goal ...)
     (run* (answer ...) goal ...))))
