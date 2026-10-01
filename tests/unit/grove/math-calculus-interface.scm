;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (grove math pipeline) (grove math expression) (grove ir) (grove render html)
             (tests unit grove cartesian-fixtures) (srfi srfi-13) (srfi srfi-64)
             (language scribble spec) (language scribble parser) (system base language))
(test-begin "polynomial calculus composition and writing interface")
(let ((report (math-compute "partial(antiderivative(x^3+2*x,x),x)" #:assumptions (list basis))))
  (test-equal "a primitive composes with differentiation" (math-source->term "x^3+2*x") (math-report-final report))
  (test-equal "composed primitive uses an explicit extra pass"
    '(polynomial-calculus components partial simplify) (map math-report-name (math-report-passes report)))
  (test-assert "composed certificate replays" (math-verification-valid? (math-verify-derivation (math-report-derivation report)))))
(for-each (lambda (execute)
            (test-equal "unsupported primitive domain cannot disappear through cancellation" 'unsupported-expression
              (math-report-reason (execute "antiderivative(1/x,x)-antiderivative(1/x,x)"
                                    #:assumptions (list basis "nonzero(x)")))))
  (list math-compute math-simplify))
(let ((report (math-check-equation "partial(antiderivative((x+y)^2,x),x)" "(x+y)^2" #:assumptions (list basis))))
  (test-assert "equation checker computes embedded primitives" (math-report-complete? report)))
(let* ((source "@use-modules[((grove math semantic) #:prefix ◆:)]
@(define r3 (◆:space 'x 'y 'z))
@with-assumptions[r3]{
  @antiderivative[\"x\"]{x^3+2*x}
  @solve-poisson{6}
  @check-equation[\"6\"]{laplacian(3*x^2)}
  @check-equation[\"3\" #:on-error 'show]{laplacian(x^2)}
}")
       (document (scribble-evaluate-document (open-input-string source)
                     (default-environment scribble) "calculus.plant"))
       (html (sxml->html (content->sxml document))))
  (test-assert "calculus operation notation uses native subscripts" (string-contains html "<msub>"))
  (test-assert "primitive step is visible" (string-contains html "integrate-polynomial"))
  (test-assert "Poisson step is visible" (string-contains html "solve-polynomial-poisson"))
  (test-assert "calculus retains authored source" (string-contains html "calculus.plant:"))
  (test-assert "failed equation is displayed" (string-contains html "equality was not established")))
(test-end "polynomial calculus composition and writing interface")
