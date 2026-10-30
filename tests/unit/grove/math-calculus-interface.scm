;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (grove math expression) (grove ir) (grove render html)
             (grove math derivation) (srfi srfi-1)
             (tests unit grove cartesian-fixtures) (srfi srfi-13) (srfi srfi-64)
             (language scribble spec) (language scribble parser) (system base language))
(test-begin "polynomial calculus composition and writing interface")
(let* ((derivative (φ:differentiate "x^3+2*x" "x" #:assumptions (list basis)))
       (partial (φ:partial "x^3+2*x*y" "x" #:assumptions (list basis)))
       (primitive (φ:antiderivative "x^3+2*x" "x" #:assumptions (list basis)))
       (derivative-audit (math-report-audit derivative))
       (partial-audit (math-report-audit partial))
       (primitive-audit (math-report-audit primitive)))
  (test-assert "derivative report exposes separate replay, contract, and proof outcomes"
    (and (math-calculation-audit? derivative-audit)
         (math-calculation-audit-replay-valid? derivative-audit)
         (eq? 'semantically-checked
              (math-calculation-audit-contract-status derivative-audit))
         (eq? 'unsupported (math-calculation-audit-proof-status derivative-audit))
         (pair? (math-calculation-audit-proof-errors derivative-audit))))
  (test-assert "partial report exposes the same audit layers and polynomial contract"
    (and (math-calculation-audit? partial-audit)
         (math-calculation-audit-replay-valid? partial-audit)
         (eq? 'semantically-checked
              (math-calculation-audit-contract-status partial-audit))
         (eq? 'unsupported (math-calculation-audit-proof-status partial-audit))))
  (test-assert "primitive report exposes the same three audit layers"
    (and (math-calculation-audit-replay-valid? primitive-audit)
         (eq? 'semantically-checked
              (math-calculation-audit-contract-status primitive-audit))
         (eq? 'unsupported (math-calculation-audit-proof-status primitive-audit)))))
(let* ((report (φ:partial "f(x,y)" "x"
                 #:assumptions ' ("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)")))
       (audit (math-report-audit report)))
  (test-assert "symbolic function partials keep replay evidence without overclaiming polynomial semantics"
    (and (math-report-finished? report)
         (math-calculation-audit? audit)
         (math-calculation-audit-replay-valid? audit)
         (eq? 'unsupported (math-calculation-audit-contract-status audit)))))
(let* ((report (φ:differentiate "x^3+2*x" "x" #:assumptions (list basis)))
       (certificate (math-report-derivation report))
       (terms (math-derivation-terms certificate))
       (steps (math-derivation-steps certificate))
       (assumptions (math-derivation-assumptions certificate))
       (forged (make-math-derivation
                 (append (drop-right terms 1) (list (φ:term "0"))) steps assumptions))
       (prefix (make-math-derivation (list (car terms)) '() assumptions))
       (forged-audit (math-audit-calculation forged))
       (prefix-audit (math-audit-calculation prefix)))
  (test-assert "audit reports reject a forged certificate endpoint at replay and contract layers"
    (and (not (math-calculation-audit-replay-valid? forged-audit))
         (eq? 'rejected (math-calculation-audit-contract-status forged-audit))
         (eq? 'rejected (math-calculation-audit-proof-status forged-audit))
         (pair? (math-calculation-audit-replay-errors forged-audit))
         (pair? (math-calculation-audit-contract-diagnostics forged-audit))
         (pair? (math-calculation-audit-proof-errors forged-audit))))
  (test-assert "a replayable non-result prefix is outside the whole algorithm contract"
    (and (math-calculation-audit-replay-valid? prefix-audit)
         (eq? 'unsupported (math-calculation-audit-contract-status prefix-audit)))))
(let* ((report (φ:differentiate "x^3+2*x" "x" #:assumptions (list basis)
                  #:max-steps 0))
       (audit (math-report-audit report)))
  (test-assert "cutoff reports retain replay evidence while leaving the operation contract unsupported"
    (and (eq? 'cutoff (math-report-status report))
         (math-calculation-audit? audit)
         (math-calculation-audit-replay-valid? audit)
         (eq? 'unsupported (math-calculation-audit-contract-status audit)))))
(let ((report (φ:compute "partial(antiderivative(x^3+2*x,x),x)" #:assumptions (list basis))))
  (test-equal "a primitive composes with differentiation" (φ:term "x^3+2*x") (math-report-final report))
  (test-equal "composed primitive uses an explicit extra pass"
    '(polynomial-calculus components partial simplify) (map math-report-name (math-report-passes report)))
  (test-assert "composed certificate replays" (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
  (test-assert "composed calculations expose audits without claiming unsupported solver contracts"
    (let ((audit (math-report-audit report)))
      (and (math-calculation-audit? audit)
           (math-calculation-audit-replay-valid? audit)
           (eq? 'unsupported (math-calculation-audit-contract-status audit))))))
(let* ((report (φ:compute "partial(x^3,x)" #:assumptions (list basis)))
       (audit (math-report-audit report)))
  (test-assert "compute independently checks supported scalar polynomial steps"
    (and (math-report-finished? report)
         (math-calculation-audit? audit)
         (math-calculation-audit-replay-valid? audit)
         (eq? 'semantically-checked (math-calculation-audit-contract-status audit)))))
(for-each
  (lambda (source)
    (let* ((report (φ:compute source #:assumptions (list basis)))
           (audit (math-report-audit report)))
      (test-assert (string-append "compute independently checks Cartesian contract: " source)
        (and (math-report-finished? report)
             (math-calculation-audit? audit)
             (math-calculation-audit-replay-valid? audit)
             (eq? 'semantically-checked
                  (math-calculation-audit-contract-status audit))))))
  '("grad(x^2+y*z)"
    "div(components(x^2,y^2,z^2))"
    "curl(components(x*y,y*z,z*x))"
    "curl(grad(x*y))"
    "div(curl(components(x^2*y,y^2*z,z^2*x)))"
    "grad(x*y)+components(1,2,3)"
    "laplacian(x^2*y^2)"
    "laplacian(components(x^2,y^2,z^2))"))
(for-each (lambda (execute)
            (test-equal "unsupported primitive domain cannot disappear through cancellation" 'unsupported-expression
              (math-report-reason (execute "antiderivative(1/x,x)-antiderivative(1/x,x)"
                                    #:assumptions (list basis "nonzero(x)")))))
  (list φ:compute φ:simplify))
(let ((report (φ:check-equation "partial(antiderivative((x+y)^2,x),x)" "(x+y)^2" #:assumptions (list basis))))
  (test-assert "equation checker computes embedded primitives" (math-report-finished? report)))
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
