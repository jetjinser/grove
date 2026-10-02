;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math proof)
             ((grove math formula) #:prefix φ:)
             (grove math derivation)
             (grove math rewrite)
             (grove math semantic-check)
             (grove math verification)
             (srfi srfi-64))

(test-begin "math proof IR and checker")

(define scalar-x '((scalar (var x))))
(define add-zero-proof
  (math-proof-rule-instance 'add-zero
    '((term . (var x)) (zero . (num "0"))) '()))
(define add-zero-check (math-proof-check add-zero-proof scalar-x))

(test-assert "proof checks a rule application independently of rewrite search"
  (and (math-semantic-check-valid? (math-polynomial-check-rule 'add-zero))
       (math-proof-result-valid? add-zero-check)
       (equal? (math-proof-result-left add-zero-check) '(+ (var x) (num "0")))
       (equal? (math-proof-result-right add-zero-check) '(var x))))
(test-equal "a rule instance requires the assumptions needed by its type guard"
  'rejected (math-proof-result-status (math-proof-check add-zero-proof '())))
(test-equal "proof nodes reject missing premise references"
  'rejected (math-proof-result-status (math-proof-check (math-proof-premise 0) '())))
(test-assert "equality assumptions can serve as equality proof leaves"
  (let ((result (math-proof-check (math-proof-premise 0)
                  '((= (var x) (var y))))))
    (and (math-proof-result-valid? result)
         (equal? (math-proof-result-left result) '(var x))
         (equal? (math-proof-result-right result) '(var y)))))
(test-equal "rule instances reject missing or forged metavariable substitutions"
  'rejected
  (math-proof-result-status
    (math-proof-check
      (math-proof-rule-instance 'add-zero '((term . (var x))) '()) scalar-x)))
(test-equal "the checker refuses rules with no verified universal theorem"
  'unsupported
  (math-proof-result-status
    (math-proof-check
      (math-proof-rule-instance 'differentiate-power '() '()) scalar-x)))

(let* ((whole-from '(+ (+ (var x) (num "0")) (var y)))
       (whole-to '(+ (var x) (var y)))
       (context-proof (math-proof-congruence whole-from whole-to '(0) add-zero-proof))
       (context-check (math-proof-check context-proof
                        '((scalar (var x)) (scalar (var y)))))
       (chain-proof
         (math-proof-transitivity add-zero-proof
           (math-proof-symmetry add-zero-proof)))
       (chain-check (math-proof-check chain-proof scalar-x)))
  (test-assert "congruence checks an exact operand context"
    (and (math-proof-result-valid? context-check)
         (equal? (math-proof-result-left context-check) whole-from)
         (equal? (math-proof-result-right context-check) whole-to)))
  (test-assert "symmetry and transitivity compose checked equalities"
    (and (math-proof-result-valid? chain-check)
         (equal? (math-proof-result-left chain-check) '(+ (var x) (num "0")))
         (equal? (math-proof-result-right chain-check) '(+ (var x) (num "0")))))
  (test-equal "forged congruence contexts are rejected"
    'rejected
    (math-proof-result-status
      (math-proof-check
        (math-proof-congruence whole-from '(+ (var y) (var y)) '(0) add-zero-proof)
        '((scalar (var x)) (scalar (var y)))))))

(test-equal "transitivity rejects a discontinuous proof chain"
  'rejected
  (math-proof-result-status
    (math-proof-check
      (math-proof-transitivity add-zero-proof
      (math-proof-reflexivity '(var y))) scalar-x)))

(test-assert "a checked rewrite trace elaborates into an independently checked proof"
  (let* ((verification (φ:check-chain '("x+0" "x")
                        #:assumptions '("scalar(x)") #:steps '(add-zero)))
         (derivation (math-verification-derivation verification))
         (proof (math-proof-from-derivation derivation))
         (checked (and proof
                       (math-proof-check proof
                         (math-derivation-assumptions derivation)))))
    (and (math-verification-valid? verification)
         (math-proof-result-valid? checked)
         (equal? (math-proof-result-left checked) '(+ (var x) (num "0")))
         (equal? (math-proof-result-right checked) '(var x)))))

(test-end "math proof IR and checker")
