;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math context)
             (grove math term) (srfi srfi-64))

(test-begin "local math assumptions")
(define saved #f)
(with-math-assumptions '("scalar(x)")
  (test-equal "context makes scalar declarations available to computation"
    (math-source->term "2*x") (math-report-final (math-differentiate "x*x" "x")))
  (set! saved (math-report-derivation (math-simplify "x+x")))
  (with-math-assumptions '("scalar(y); scalar(x)")
    (test-equal "nested contexts merge and deduplicate premises"
      '((scalar (var x)) (scalar (var y))) (current-math-assumptions))
    (test-equal "one command may add premises to its context"
      '((scalar (var x)) (scalar (var y)) (vector (var E)))
      (math-derivation-assumptions
        (math-report-derivation (math-simplify "E+zero(vector)" #:assumptions '("vector(E)"))))))
  (test-equal "leaving a nested scope restores its parent"
    '((scalar (var x))) (current-math-assumptions))
  (with-math-assumptions '("vector(x)")
    (test-equal "contradictory inner declarations are diagnosed, not overwritten"
      'type-error (math-report-reason (math-simplify "x+x"))))
  (call-with-math-assumptions '("scalar(y)")
    (lambda ()
      (test-equal "a replacement scope starts with only its own premises"
        '((scalar (var y))) (current-math-assumptions))) #:replace? #t)
  (catch #t
    (lambda () (with-math-assumptions '("scalar(y)") (error "scope failure")))
    (lambda args #t))
  (test-equal "exceptional exit restores the previous context"
    '((scalar (var x))) (current-math-assumptions)))
(test-equal "the context does not escape its scope" '() (current-math-assumptions))
(test-equal "a certificate retains its effective premise snapshot"
  '((scalar (var x))) (math-derivation-assumptions saved))
(test-assert "a certificate can be verified after its context has ended"
  (math-verification-valid? (math-verify-derivation saved)))
(test-equal "unscoped operations do not inherit the preceding calculation"
  'type-error (math-report-reason (math-simplify "x+x")))
(test-error "malformed context inputs are rejected" #t
  (with-math-assumptions '(3) (math-simplify "1")))
(test-end "local math assumptions")
