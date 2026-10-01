;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math derivation)
  #:use-module (srfi srfi-9)
  #:export (make-math-derivation-step math-derivation-step?
            math-derivation-step-from math-derivation-step-to
            math-derivation-step-rule math-derivation-step-path
            make-math-derivation math-derivation?
            math-derivation-terms math-derivation-steps
            math-derivation-assumptions math-derivation-sources))

;; Both generated candidates and authored chains share the same certificate.
(define-record-type <math-derivation-step>
  (make-math-derivation-step from to rule path)
  math-derivation-step?
  (from math-derivation-step-from)
  (to math-derivation-step-to)
  (rule math-derivation-step-rule)
  (path math-derivation-step-path))

(define-record-type <math-derivation>
  (%make-math-derivation terms steps assumptions sources)
  math-derivation?
  (terms math-derivation-terms)
  (steps math-derivation-steps)
  (assumptions math-derivation-assumptions)
  (sources math-derivation-sources))

(define* (make-math-derivation terms steps assumptions #:key (sources '()))
  (%make-math-derivation terms steps assumptions sources))
