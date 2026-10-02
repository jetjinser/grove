;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math audit)
  #:use-module (grove math derivation)
  #:use-module (grove math proof)
  #:use-module (grove math semantic-check)
  #:use-module (grove math verification)
  #:use-module (srfi srfi-9)
  #:export (math-calculation-audit? math-audit-calculation
            math-calculation-audit-replay
            math-calculation-audit-replay-valid?
            math-calculation-audit-replay-errors
            math-calculation-audit-contract
            math-calculation-audit-contract-status
            math-calculation-audit-contract-diagnostics
            math-calculation-audit-proof
            math-calculation-audit-proof-status
            math-calculation-audit-proof-errors))

;; A calculation audit brings independent reports together without merging
;; their evidence languages: replay checks execution edges, the contract
;; checker checks algorithm semantics, and Proof IR checks rule justifications.
(define-record-type <math-calculation-audit>
  (make-math-calculation-audit replay contract proof)
  math-calculation-audit?
  (replay math-calculation-audit-replay)
  (contract math-calculation-audit-contract)
  (proof math-calculation-audit-proof))

(define (math-calculation-audit-replay-valid? audit)
  (math-verification-valid? (math-calculation-audit-replay audit)))

(define (math-calculation-audit-replay-errors audit)
  (math-verification-errors (math-calculation-audit-replay audit)))

(define (math-calculation-audit-contract-status audit)
  (math-semantic-check-status (math-calculation-audit-contract audit)))

(define (math-calculation-audit-contract-diagnostics audit)
  (math-semantic-check-diagnostics (math-calculation-audit-contract audit)))

(define (math-calculation-audit-proof-status audit)
  (math-proof-derivation-report-status (math-calculation-audit-proof audit)))

(define (math-calculation-audit-proof-errors audit)
  (math-proof-derivation-report-errors (math-calculation-audit-proof audit)))

(define (math-audit-calculation derivation)
  "Audit a calculation certificate through replay, contract, and proof IR."
  (unless (math-derivation? derivation)
    (error "expected a math derivation" derivation))
  (make-math-calculation-audit
    (math-verify-derivation derivation)
    (math-polynomial-check-derivation derivation)
    (math-proof-elaborate-derivation derivation)))
