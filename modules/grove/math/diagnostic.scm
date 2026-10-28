;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math diagnostic)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-math-diagnostic math-diagnostic? math-diagnostic-step
            math-diagnostic-rule math-diagnostic-path math-diagnostic-code
            math-diagnostic-premises math-diagnostic-message math-diagnostic-source math-diagnostic-origin))

(define-record-type <math-diagnostic>
  (%make-math-diagnostic step rule path code premises message source origin)
  math-diagnostic?
  (step math-diagnostic-step)
  (rule math-diagnostic-rule)
  (path diagnostic-path/raw)
  (code math-diagnostic-code)
  (premises diagnostic-premises/raw)
  (message diagnostic-message/raw)
  (source math-diagnostic-source)
  (origin math-diagnostic-origin))

(define (math-diagnostic-path diagnostic)
  (snapshot-data (diagnostic-path/raw diagnostic)))

(define (math-diagnostic-premises diagnostic)
  (snapshot-data (diagnostic-premises/raw diagnostic)))

(define (math-diagnostic-message diagnostic)
  (snapshot-data (diagnostic-message/raw diagnostic)))

(define* (make-math-diagnostic step rule path code premises message
                              #:key (source #f) (origin #f))
  "Construct a diagnostic with an optional one-based STEP, RULE, PATH, CODE,
PREMISES, and MESSAGE. PATH contains non-negative child indexes; SOURCE and
ORIGIN are optional opaque location values."
  (unless (or (not step) (and (exact-integer? step) (> step 0)))
    (error "math diagnostic step must be a positive integer or #f" step))
  (unless (or (not rule) (symbol? rule))
    (error "math diagnostic rule must be a symbol or #f" rule))
  (unless (and (list? path)
               (every (lambda (index)
                        (and (exact-integer? index) (>= index 0)))
                      path))
    (error "math diagnostic path must contain non-negative indexes" path))
  (unless (symbol? code)
    (error "math diagnostic code must be a symbol" code))
  (unless (list? premises)
    (error "math diagnostic premises must be a list" premises))
  (unless (string? message)
    (error "math diagnostic message must be a string" message))
  (%make-math-diagnostic step rule (snapshot-data path) code
                         (snapshot-data premises)
                         (snapshot-data message) source origin))
