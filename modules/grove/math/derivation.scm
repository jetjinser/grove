;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math derivation)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (grove math term)
  #:export (make-math-derivation-step math-derivation-step?
            math-derivation-step-from math-derivation-step-to
            math-derivation-step-rule math-derivation-step-path
            make-math-derivation math-derivation?
            math-derivation-terms math-derivation-steps
            math-derivation-assumptions math-derivation-sources math-derivation-requirements))

;; Both generated candidates and authored chains share the same certificate.
(define-record-type <math-derivation-step>
  (%make-math-derivation-step from to rule path)
  math-derivation-step?
  (from derivation-step-from/raw)
  (to derivation-step-to/raw)
  (rule math-derivation-step-rule)
  (path derivation-step-path/raw))

(define (make-math-derivation-step from to rule path)
  (%make-math-derivation-step (snapshot-data from) (snapshot-data to)
                              rule (snapshot-data path)))

(define (math-derivation-step-from step)
  (snapshot-data (derivation-step-from/raw step)))

(define (math-derivation-step-to step)
  (snapshot-data (derivation-step-to/raw step)))

(define (math-derivation-step-path step)
  (snapshot-data (derivation-step-path/raw step)))

(define-record-type <math-derivation>
  (%make-math-derivation terms steps assumptions sources requirements)
  math-derivation?
  (terms derivation-terms/raw)
  (steps derivation-steps/raw)
  (assumptions derivation-assumptions/raw)
  (sources derivation-sources/raw)
  (requirements derivation-requirements/raw))

(define (math-derivation-terms derivation)
  (snapshot-data (derivation-terms/raw derivation)))

(define (math-derivation-steps derivation)
  (snapshot-data (derivation-steps/raw derivation)))

(define (math-derivation-assumptions derivation)
  (snapshot-data (derivation-assumptions/raw derivation)))

(define (math-derivation-sources derivation)
  (snapshot-data (derivation-sources/raw derivation)))

(define (math-derivation-requirements derivation)
  (snapshot-data (derivation-requirements/raw derivation)))

(define* (make-math-derivation terms steps assumptions #:key (sources '()) (requirements '()))
  (unless (and (list? requirements) (every math-term? requirements))
    (error "expected mathematical domain requirements" requirements))
  (%make-math-derivation (snapshot-data terms)
                         (snapshot-data steps)
                         (snapshot-data assumptions)
                         (snapshot-data sources)
                         (snapshot-data requirements)))
