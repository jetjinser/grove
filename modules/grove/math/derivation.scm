;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math derivation)
  #:use-module (srfi srfi-9)
  #:export (make-math-derivation-step math-derivation-step?
            math-derivation-step-from math-derivation-step-to
            math-derivation-step-rule math-derivation-step-path
            make-math-search-result math-search-result?
            math-search-result-terms math-search-result-steps
            math-search-result-assumptions))

;; Both generated candidates and authored chains share the same certificate.
(define-record-type <math-derivation-step>
  (make-math-derivation-step from to rule path)
  math-derivation-step?
  (from math-derivation-step-from)
  (to math-derivation-step-to)
  (rule math-derivation-step-rule)
  (path math-derivation-step-path))

(define-record-type <math-search-result>
  (make-math-search-result terms steps assumptions)
  math-search-result?
  (terms math-search-result-terms)
  (steps math-search-result-steps)
  (assumptions math-search-result-assumptions))
