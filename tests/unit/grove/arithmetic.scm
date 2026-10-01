;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math term) (grove math arithmetic)
             (grove math rewrite) (grove relation) (ice-9 match)
             (srfi srfi-1) (srfi srfi-64))

(define (expansion report) (math-report-derivation (car (math-report-passes report))))
(define (simplification report) (math-report-derivation (cadr (math-report-passes report))))

(define facts '("scalar(x); scalar(y)"))
(define (certificate report) (car (math-report-candidates report)))
(define (final report) (last (math-derivation-terms (certificate report))))
(define (value term environment)
  (match term
    (('num n) (string->number n))
    (('var name) (assq-ref environment name))
    (('- a) (- (value a environment)))
    ((operator a b)
     ((case operator ((+) +) ((-) -) ((*) *) ((/) /) ((^) expt))
      (value a environment) (value b environment)))))

(test-begin "exact arithmetic and collection")
(for-each
  (lambda (pair)
    (let ((report (math-calculate (car pair))))
      (test-equal (string-append "exact calculation: " (car pair))
        (math-source->term (cdr pair)) (final report))
      (test-assert "constant arithmetic certificate replays"
        (math-verification-valid? (math-verify-derivation (certificate report))))))
  '(("2*3" . "6") ("(1+2)*3" . "9") ("2/4" . "1/2")
    ("1/3+1/6" . "1/2") ("2-5" . "-3") ("(-2)/(-4)" . "1/2")
    ("1/2-3/4" . "-(1/4)") ("12345678901234567890*3" . "37037036703703703670")))
(test-equal "zero division is never given a value"
  (math-source->term "1/0") (final (math-calculate "1/0")))
(test-equal "constant pass leaves symbolic grouping alone"
  (math-source->term "x+x") (final (math-calculate "x+x" #:assumptions facts)))
(for-each
  (lambda (pair)
    (let ((report (math-collect (car pair) #:assumptions facts)))
      (test-equal (string-append "collection: " (car pair))
        (math-source->term (cdr pair)) (final report))
      (test-assert "collection certificate replays"
        (math-verification-valid? (math-verify-derivation (certificate report))))
      (for-each
        (lambda (x)
          (test-equal "independent evaluation agrees before and after collection"
            (value (math-source->term (car pair)) `((x . ,x) (y . 7)))
            (value (final report) `((x . ,x) (y . 7)))))
        '(-3 0 2))))
  '(("x+x" . "2*x") ("2*x+3*x" . "5*x") ("x*2+3*x" . "5*x")
    ("x+y-x" . "y") ("x-x" . "0") ("x^2+x^2" . "2*x^2")
    ("(1/2)*x+(1/3)*x" . "(5/6)*x") ("x-3*x" . "(-2)*x")))
(test-equal "collection leaves constant-only calculations to the constant pass"
  (math-source->term "2*3") (final (math-collect "2*3")))
(test-equal "constant-only sums also belong to the constant pass"
  (math-source->term "2+3") (final (math-collect "2+3")))
(test-equal "vector terms cannot enter scalar collection"
  (math-source->term "E+E")
  (final (math-collect "E+E" #:assumptions '("vector(E)"))))
(let ((result (math-differentiate "(1/2)*x^2" "x" #:assumptions facts)))
  (test-equal "rational coefficients participate in polynomial differentiation"
    (math-source->term "x") (math-report-final result))
  (test-assert "rational differentiation expansion replays"
    (math-verification-valid?
      (math-verify-derivation (expansion result)))))
(test-equal "differentiate and collect x*x"
  (math-source->term "2*x")
  (math-report-final (math-differentiate "x*x" "x" #:assumptions facts)))
(test-assert "search can use constant computation"
  (pair? (math-report-candidates (math-search "2*3" "6"))))
(test-assert "authored exact rational computation is verified"
  (math-verification-valid? (math-check-chain '("1/3+1/6" "1/2"))))
(test-assert "incorrect arithmetic is rejected"
  (not (math-verification-valid? (math-check-chain '("2*3" "7")))))
(test-end "exact arithmetic and collection")
