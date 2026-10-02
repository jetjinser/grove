;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math polynomial contract)
  #:use-module (srfi srfi-9)
  #:export (math-derivative-evidence? make-math-derivative-evidence
            math-derivative-evidence-case math-derivative-evidence-expression
            math-derivative-evidence-coordinate math-derivative-evidence-result
            math-derivative-evidence-children
            math-integral-monomial-evidence? make-math-integral-monomial-evidence
            math-integral-monomial-evidence-input math-integral-monomial-evidence-output
            math-antiderivative-evidence? make-math-antiderivative-evidence
            math-antiderivative-evidence-expression math-antiderivative-evidence-coordinate
            math-antiderivative-evidence-input math-antiderivative-evidence-result
            math-antiderivative-evidence-monomials math-antiderivative-evidence-derivative))

;; This is algorithm-contract evidence, not an equality derivation. Its data
;; shapes mirror the recursive program cases and can be checked independently.
(define-record-type <math-derivative-evidence>
  (make-math-derivative-evidence case expression coordinate result children)
  math-derivative-evidence?
  (case math-derivative-evidence-case)
  (expression math-derivative-evidence-expression)
  (coordinate math-derivative-evidence-coordinate)
  (result math-derivative-evidence-result)
  (children math-derivative-evidence-children))

(define-record-type <math-integral-monomial-evidence>
  (make-math-integral-monomial-evidence input output)
  math-integral-monomial-evidence?
  (input math-integral-monomial-evidence-input)
  (output math-integral-monomial-evidence-output))

(define-record-type <math-antiderivative-evidence>
  (make-math-antiderivative-evidence expression coordinate input result monomials derivative)
  math-antiderivative-evidence?
  (expression math-antiderivative-evidence-expression)
  (coordinate math-antiderivative-evidence-coordinate)
  (input math-antiderivative-evidence-input)
  (result math-antiderivative-evidence-result)
  (monomials math-antiderivative-evidence-monomials)
  (derivative math-antiderivative-evidence-derivative))
