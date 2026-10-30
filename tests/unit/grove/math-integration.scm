;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math calculation)
             (grove math semantic-check) (grove math expression)
             (grove math private polynomial)
             (grove math types) (grove math context) (grove relation)
             (grove math derivation)
             (grove math semantic-check)
             ((grove math semantic) #:prefix ◆:)
             (tests unit grove polynomial-oracle) (tests unit grove cartesian-fixtures)
             (srfi srfi-1) (srfi srfi-64))
(test-begin "exact polynomial antiderivatives")
(for-each
  (lambda (entry)
    (let* ((source (car entry)) (coordinate (cadr entry))
           (report (φ:antiderivative source coordinate #:assumptions (list basis)))
           (certificate (math-report-derivation report))
           (result (math-report-final report))
           (index (list-index (lambda (name) (string=? (symbol->string name) coordinate)) '(x y z))))
      (test-assert "primitive completes" (math-report-finished? report))
      (test-equal "explicit primitive and simplification passes" '(antiderivative simplify)
        (map math-report-name (math-report-passes report)))
      (test-assert "primitive certificate replays" (math-verification-valid? (math-verify-derivation certificate)))
      (test-assert "primitive certificate checks against the monomial integration contract"
        (math-semantic-check-valid? (math-polynomial-check-derivation certificate)))
      (test-equal "independent derivative recovers integrand"
        (evaluate (φ:term source) coordinates)
        (evaluate `(partial ,result (var ,(string->symbol coordinate))) coordinates))
      (test-equal "primitive is zero on the coordinate origin" '()
        (at-origin (cdr (evaluate result coordinates)) index))
      (for-each (lambda (step)
                  (test-equal "every primitive edge has independent semantics"
                    (evaluate (math-derivation-step-from step) coordinates)
                    (evaluate (math-derivation-step-to step) coordinates)))
                (math-derivation-steps certificate))))
  '(("0" "x") ("7" "x") ("x^3+2*x" "x") ("x*y+z" "x")
    ("(x+y)^3" "x") ("x^2*y^2-z/3" "y") ("x^2*y^2-z/3" "z")
    ("x^3/3+2*x*y^2-z/7+(x+y)^2*(x-z)" "x")
    ("x^3/3+2*x*y^2-z/7+(x+y)^2*(x-z)" "y")
    ("x^3/3+2*x*y^2-z/7+(x+y)^2*(x-z)" "z")))
(let* ((report (φ:antiderivative "x^3+2*x" "x" #:assumptions (list basis)))
       (check (φ:check-polynomial-antiderivative
                "x^3+2*x" 'x (math-report-result report)))
       (proof (math-semantic-check-subject check)))
  (test-assert "integration proof records each monomial recurrence and its normalization checks"
    (and (math-antiderivative-evidence? proof)
         (= (length (math-antiderivative-evidence-monomials proof)) 2)
         (math-derivative-evidence? (math-antiderivative-evidence-derivative proof))))
  (test-assert "integration checker accepts the actual pass result"
    (math-semantic-check-valid? check))
  (let* ((step (car (math-antiderivative-evidence-monomials proof)))
         (bad-step
           (make-math-integral-monomial-evidence
             (math-integral-monomial-evidence-input step)
             '(((x . 4)) . 99)))
         (bad-proof
           (make-math-antiderivative-evidence
             (math-antiderivative-evidence-expression proof)
             (math-antiderivative-evidence-coordinate proof)
             (math-antiderivative-evidence-input proof)
             (math-antiderivative-evidence-result proof)
             (cons bad-step (cdr (math-antiderivative-evidence-monomials proof)))
             (math-antiderivative-evidence-derivative proof))))
    (test-eq "integration evidence rejects a forged monomial result" 'rejected
      (math-semantic-check-status (math-check-antiderivative-evidence bad-proof))))
  (test-eq "integration evidence rejects an untyped list" 'rejected
    (math-semantic-check-status (math-check-antiderivative-evidence '(primitive))))
  (test-equal "a wrong monomial coefficient is diagnosed by differentiating the candidate"
    'rejected
    (math-semantic-check-status
      (φ:check-polynomial-antiderivative "x^3+2*x" 'x "x^4/4+x")))
  (let ((check (φ:check-polynomial-antiderivative "x^3+2*x" 'x "x^4/4+x^2+7")))
    (test-equal "a nonzero integration constant violates the canonical origin condition"
      'rejected (math-semantic-check-status check))
    (test-assert "the diagnostic identifies the violated normalization contract"
      (string-contains (car (math-semantic-check-diagnostics check))
        "not normalized to zero")))
  (test-equal "non-polynomial integrands are outside the integration checker"
    'unsupported
    (math-semantic-check-status
      (φ:check-polynomial-antiderivative "sin(x)" 'x "-cos(x)")))
  (let* ((certificate (math-report-derivation report))
         (terms (math-derivation-terms certificate))
         (forged (make-math-derivation
                   (append (drop-right terms 1) (list (φ:term "x^4/4+x")))
                   (math-derivation-steps certificate)
                   (math-derivation-assumptions certificate))))
    (test-equal "integration certificate rejects a forged endpoint"
      'rejected
      (math-semantic-check-status (math-polynomial-check-derivation forged)))))
(let ((report (φ:antiderivative "a*x" "x" #:assumptions (list basis "parameter(a)"))))
  (test-equal "constant parameter coefficient is retained" (φ:term "(1/2)*a*x^2") (math-report-final report))
  (test-assert "parameter role remains a result requirement"
    (member '(parameter (var a)) (math-value-requirements (math-report-result report)))))
(test-equal "generic scalar is not an independent parameter" 'unsupported-expression
  (math-report-reason (φ:antiderivative "a*x" "x" #:assumptions (list basis "scalar(a)"))))
(for-each (lambda (source)
            (test-equal "nonpolynomial integrals are rejected" 'unsupported-expression
              (math-report-reason (φ:antiderivative source "x" #:assumptions (list basis "nonzero(x)")))))
  '("sin(x)" "1/x" "x^x"))
(test-equal "parameters cannot be integration coordinates" 'unsupported-expression
  (math-report-reason (φ:antiderivative "x" "a" #:assumptions (list basis "parameter(a)"))))
(test-equal "absent coordinate declaration is diagnosed" 'missing-premise
  (math-report-reason (φ:antiderivative "x" "x" #:assumptions '("scalar(x)"))))
(test-equal "degree limit is explicit" 'polynomial-limit
  (math-report-reason (φ:antiderivative "x^65" "x" #:assumptions (list basis))))
(test-equal "sparse expansion size is bounded" 'polynomial-limit
  (math-report-reason (φ:antiderivative "(x+y+z)^25" "x" #:assumptions (list basis))))
(test-equal "step budget cannot claim a primitive" 'step-limit
  (math-report-reason (φ:antiderivative "x" "x" #:max-steps 0 #:assumptions (list basis))))
(test-equal "term-size budget cannot discard a prefix" 'term-size-limit
  (math-report-reason (φ:antiderivative "x^3+2*x" "x" #:max-term-size 3 #:assumptions (list basis))))
(test-equal "the primitive operator has a scalar type" 'scalar
  (math-typed-term-type (math-infer-type facts (φ:term "antiderivative(x,x)"))))
(test-equal "semantic constructor has the same IR" (φ:term "antiderivative(x,x)")
  (math-value-term (◆:antiderivative (◆:variable 'x) (◆:variable 'x))))
(test-end "exact polynomial antiderivatives")
