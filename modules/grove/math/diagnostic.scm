;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math diagnostic)
  #:use-module (srfi srfi-9)
  #:export (make-math-diagnostic math-diagnostic? math-diagnostic-step
            math-diagnostic-rule math-diagnostic-path math-diagnostic-code
            math-diagnostic-premises math-diagnostic-message math-diagnostic-source))

(define-record-type <math-diagnostic>
  (%make-math-diagnostic step rule path code premises message source)
  math-diagnostic?
  (step math-diagnostic-step)
  (rule math-diagnostic-rule)
  (path math-diagnostic-path)
  (code math-diagnostic-code)
  (premises math-diagnostic-premises)
  (message math-diagnostic-message)
  (source math-diagnostic-source))

(define* (make-math-diagnostic step rule path code premises message #:key (source #f))
  (%make-math-diagnostic step rule path code premises message source))
