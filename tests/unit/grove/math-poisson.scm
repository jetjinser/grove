;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math context) (grove math types) (grove relation)
             (tests unit grove polynomial-oracle) (tests unit grove cartesian-fixtures)
             ((grove math semantic) #:prefix ◆:) (srfi srfi-1) (srfi srfi-64))
(test-begin "independent polynomial Poisson verification")
(for-each
  (lambda (entry)
    (let* ((source (car entry)) (axis (cadr entry)) (index (caddr entry))
           (report (φ:solve-poisson source #:coordinate axis #:assumptions (list basis)))
           (certificate (math-report-derivation report))
           (result (math-report-final report))
           (polynomial (cdr (evaluate result coordinates))))
      (test-assert "polynomial particular solution completes" (math-report-complete? report))
      (test-assert "particular solution certificate replays" (math-verification-valid? (math-verify-derivation certificate)))
      (test-equal "independent Laplacian establishes the PDE"
        (evaluate (φ:term source) coordinates) (evaluate `(laplacian ,result) coordinates))
      (test-equal "zero Cauchy value anchors selected solution" '() (at-origin polynomial index))
      (test-equal "zero first Cauchy derivative anchors selected solution" '()
        (at-origin (partial polynomial index) index))
      ;; The solver edge is checked by its defining PDE plus both anchors,
      ;; rather than reproducing the solver algorithm in the oracle.
      (for-each
        (lambda (step)
          (unless (eq? (math-derivation-step-rule step) 'solve-polynomial-poisson)
            (test-equal "post-solve algebra edges preserve independent semantics"
              (evaluate (math-derivation-step-from step) coordinates)
              (evaluate (math-derivation-step-to step) coordinates))))
        (math-derivation-steps certificate))))
  '(("0" "x" 0) ("6" "x" 0) ("y^2" "x" 0) ("x*y+z^2" "x" 0)
    ("x^2+z^2" "y" 1) ("x*y" "z" 2) ("y^4+y^2*z^2" "x" 0)))
(test-equal "default solver axis is the first basis axis"
  (φ:term "3*x^2")
  (math-report-final (φ:solve-poisson "6" #:assumptions (list basis))))
(test-equal "solver requires space" 'missing-premise
  (math-report-reason (φ:solve-poisson "6" #:assumptions '("coordinate(x)"))))
(test-equal "solver coordinate must belong to its space" 'unsupported-expression
  (math-report-reason (φ:solve-poisson "6" #:coordinate "t" #:assumptions (list basis "coordinate(t)"))))
(test-equal "nonpolynomial PDE is rejected" 'unsupported-expression
  (math-report-reason (φ:solve-poisson "sin(x)" #:assumptions (list basis))))
(test-equal "solver output remains subject to degree budget" 'polynomial-limit
  (math-report-reason (φ:solve-poisson "x^63" #:assumptions (list basis))))
(test-equal "a renamed ordered space chooses its own first axis"
  (φ:term "3*u^2")
  (math-report-final (φ:solve-poisson "6" #:assumptions (list (◆:space 'u 'v 'w)))))
(test-equal "constant parameter coefficients remain symbolic"
  (φ:term "(1/6)*a*x^3")
  (math-report-final (φ:solve-poisson "a*x" #:assumptions (list basis "parameter(a)"))))
(test-equal "operator type agrees with relational typing"
  (run* (type) (math-typeo facts (φ:term "poisson(x,x)") type))
  (list (math-typed-term-type (math-infer-type facts (φ:term "poisson(x,x)")))))
(test-end "independent polynomial Poisson verification")
