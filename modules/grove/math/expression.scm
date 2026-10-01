;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math expression)
  #:use-module (grove math term)
  #:use-module (grove math source)
  #:use-module (grove math premise)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-math-value math-value? math-value-term math-value-origin
            math-value-properties math-value-declarations math-value-requirements math-value-with-properties))

;; Mathematical equality and rewriting concern only the term. Origins and
;; presentation preferences travel beside it, never inside relation patterns.
(define-record-type <math-value>
  (%make-math-value term origin properties declarations requirements)
  math-value?
  (term math-value-term)
  (origin math-value-origin)
  (properties math-value-properties)
  (declarations math-value-declarations)
  (requirements math-value-requirements))

(define* (make-math-value term #:key (origin #f) (properties '()) (declarations '()) (requirements '()))
  (unless (math-term? term) (error "expected a mathematical term" term))
  (unless (or (not origin) (math-origin? origin))
    (error "expected a mathematical source origin" origin))
  (unless (list? properties) (error "expected math presentation properties" properties))
  (unless (and (list? declarations) (every math-term? declarations))
    (error "expected mathematical declarations" declarations))
  (unless (and (list? requirements) (every math-term? requirements))
    (error "expected mathematical domain requirements" requirements))
  (%make-math-value term origin properties declarations
    (delete-duplicates (append requirements (math-domain-premises term)) equal?)))

(define (math-value-with-properties value properties)
  (unless (math-value? value) (error "expected a mathematical expression" value))
  (make-math-value (math-value-term value) #:origin (math-value-origin value)
    #:properties properties #:declarations (math-value-declarations value)
    #:requirements (math-value-requirements value)))
