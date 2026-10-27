;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math diagnostic)
  #:use-module (grove private snapshot)
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

(define* (make-math-diagnostic step rule path code premises message #:key (source #f) (origin #f))
  (%make-math-diagnostic step rule (snapshot-data path) code
                         (snapshot-data premises)
                         (snapshot-data message) source origin))
