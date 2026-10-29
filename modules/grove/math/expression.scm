;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math expression)
  #:use-module (grove private snapshot)
  #:use-module (grove math term)
  #:use-module (grove math private source)
  #:use-module (grove math private premise)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-math-value math-value? math-value-term math-value-origin
            math-value-properties math-value-declarations math-value-requirements math-value-with-properties))

;; Mathematical equality and rewriting concern only the term. Origins and
;; presentation preferences travel beside it, never inside relation patterns.
(define-record-type <math-value>
  (%make-math-value term origin properties declarations requirements)
  math-value?
  (term math-value-term/raw)
  (origin math-value-origin)
  (properties math-value-properties/raw)
  (declarations math-value-declarations/raw)
  (requirements math-value-requirements/raw))

(define (math-value-term value)
  (snapshot-data (math-value-term/raw value)))

(define (math-value-properties value)
  (snapshot-data (math-value-properties/raw value)))

(define (math-value-declarations value)
  (snapshot-data (math-value-declarations/raw value)))

(define (math-value-requirements value)
  (snapshot-data (math-value-requirements/raw value)))

(define* (make-math-value term #:key (origin #f) (properties '()) (declarations '()) (requirements '()))
  "Construct a Semantic value for mathematical TERM.
ORIGIN carries optional source provenance; PROPERTIES carry presentation
data; DECLARATIONS and REQUIREMENTS attach mathematical facts. Domain premises
implied by TERM are included automatically in the resulting requirements."
  (unless (math-term? term) (error "expected a mathematical term" term))
  (unless (or (not origin) (math-origin? origin))
    (error "expected a mathematical source origin" origin))
  (unless (list? properties) (error "expected math presentation properties" properties))
  (unless (and (list? declarations) (every math-term? declarations))
    (error "expected mathematical declarations" declarations))
  (unless (and (list? requirements) (every math-term? requirements))
    (error "expected mathematical domain requirements" requirements))
  (%make-math-value (snapshot-data term) origin
    (snapshot-data properties) (snapshot-data declarations)
    (snapshot-data
      (delete-duplicates (append requirements (math-domain-premises term)) equal?))))

(define (math-value-with-properties value properties)
  "Return VALUE with replacement presentation PROPERTIES.
Its term, origin, declarations, and domain requirements are preserved."
  (unless (math-value? value) (error "expected a mathematical expression" value))
  (make-math-value (math-value-term/raw value) #:origin (math-value-origin value)
    #:properties properties #:declarations (math-value-declarations/raw value)
    #:requirements (math-value-requirements/raw value)))
