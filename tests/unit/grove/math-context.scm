;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (grove math context)
             (grove math term) (grove math types) (srfi srfi-64))

(test-begin "local math assumptions")
(define saved #f)
(with-math-assumptions (φ:assumptions "scalar(x)")
  (test-equal "context makes scalar declarations available to computation"
    (φ:term "2*x") (math-report-final (φ:differentiate "x*x" "x")))
  (set! saved (math-report-derivation (φ:simplify "x+x")))
  (with-math-assumptions (φ:assumptions "scalar(y); scalar(x)")
    (test-equal "nested contexts merge and deduplicate premises"
      '((scalar (var x)) (scalar (var y))) (current-math-assumptions))
    (test-equal "one command may add premises to its context"
      '((scalar (var x)) (scalar (var y)) (vector (var E)))
      (math-derivation-assumptions
        (math-report-derivation (φ:simplify "E+zero(vector)" #:assumptions '("vector(E)"))))))
  (test-equal "leaving a nested scope restores its parent"
    '((scalar (var x))) (current-math-assumptions))
  (with-math-assumptions (φ:assumptions "vector(x)")
    (test-equal "contradictory inner declarations are diagnosed, not overwritten"
      'type-error (math-report-reason (φ:simplify "x+x"))))
  (call-with-math-assumptions (φ:assumptions "scalar(y)")
    (lambda ()
      (test-equal "a replacement scope starts with only its own premises"
        '((scalar (var y))) (current-math-assumptions))) #:replace? #t)
  (catch #t
    (lambda () (with-math-assumptions (φ:assumptions "scalar(y)") (error "scope failure")))
    (lambda args #t))
  (test-equal "exceptional exit restores the previous context"
    '((scalar (var x))) (current-math-assumptions)))
(test-equal "the context does not escape its scope" '() (current-math-assumptions))
(test-error "context requires Semantic facts" #t
  (with-math-assumptions '("scalar(x)") (current-math-assumptions)))
(test-equal "a certificate retains its effective premise snapshot"
  '((scalar (var x))) (math-derivation-assumptions saved))
(test-assert "a certificate can be verified after its context has ended"
  (math-verification-valid? (math-verify-derivation saved)))
(test-equal "unscoped operations do not inherit the preceding calculation"
  'type-error (math-report-reason (φ:simplify "x+x")))
(test-equal "an unused conflicting declaration invalidates a computation"
  'type-error
  (math-report-reason (φ:simplify "1+1" #:assumptions '("scalar(y); vector(y)"))))
(test-equal "inference cannot choose one branch of contradictory type declarations"
  #f (math-infer-type '((scalar (var y)) (vector (var y)) (scalar (var x)))
                      (φ:term "x+y")))
(test-error "malformed context inputs are rejected" #t
  (with-math-assumptions '(3) (φ:simplify "1")))
(test-end "local math assumptions")
