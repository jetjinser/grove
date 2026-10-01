;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math expression)
  #:use-module (grove math term)
  #:use-module (grove math source)
  #:use-module (srfi srfi-9)
  #:export (make-math-value math-value? math-value-term math-value-origin
            math-value-properties math-value-with-properties))

;; Mathematical equality and rewriting concern only the term. Origins and
;; presentation preferences travel beside it, never inside relation patterns.
(define-record-type <math-value>
  (%make-math-value term origin properties)
  math-value?
  (term math-value-term)
  (origin math-value-origin)
  (properties math-value-properties))

(define* (make-math-value term #:key (origin #f) (properties '()))
  (unless (math-term? term) (error "expected a mathematical term" term))
  (unless (or (not origin) (math-origin? origin))
    (error "expected a mathematical source origin" origin))
  (unless (list? properties) (error "expected math presentation properties" properties))
  (%make-math-value term origin properties))

(define (math-value-with-properties value properties)
  (unless (math-value? value) (error "expected a mathematical expression" value))
  (make-math-value (math-value-term value) #:origin (math-value-origin value)
    #:properties properties))
