;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math expression)
             ((grove math semantic) #:prefix ◆:)
             (tests unit grove polynomial-oracle) (srfi srfi-1) (srfi srfi-64))
(test-begin "rational parameter coefficients")
(define basis (◆:space 'x 'y 'z))
(define facts (list basis "parameter(a); parameter(b); nonzero(a); nonzero(b); nonzero(a+b)"))
(define (environment a b)
  (append coordinates (list (cons 'a (scalar (list (list a 0 0 0))))
                            (cons 'b (scalar (list (list b 0 0 0)))))))
(for-each
  (lambda (source)
    (let ((primitive (φ:antiderivative source "x" #:assumptions facts))
          (solution (φ:solve-poisson source #:assumptions facts)))
      (for-each (lambda (report)
                  (test-assert "parameter polynomial calculus completes" (math-report-complete? report))
                  (test-assert "parameter certificate replays" (math-verification-valid?
                    (math-verify-derivation (math-report-derivation report)))))
                (list primitive solution))
      (for-each
        (lambda (values)
          (let ((env (apply environment values)))
            (test-equal "independent specialization differentiates the primitive"
              (evaluate (φ:term source) env)
              (evaluate `(partial ,(math-report-final primitive) (var x)) env))
            (test-equal "independent specialization verifies the Poisson residual"
              (evaluate (φ:term source) env)
              (evaluate `(laplacian ,(math-report-final solution)) env))
            (test-equal "parameter primitive retains its origin anchor" '()
              (at-origin (cdr (evaluate (math-report-final primitive) env)) 0))
            (test-equal "parameter solution retains its value anchor" '()
              (at-origin (cdr (evaluate (math-report-final solution) env)) 0))
            (test-equal "parameter solution retains its derivative anchor" '()
              (at-origin (partial (cdr (evaluate (math-report-final solution) env)) 0) 0))
            (for-each (lambda (step)
                        (test-equal "each primitive edge agrees with independent specialized arithmetic"
                          (evaluate (math-derivation-step-from step) env)
                          (evaluate (math-derivation-step-to step) env)))
                      (math-derivation-steps (math-report-derivation primitive)))))
        '((2 3) (-2 5) (1/2 -3/4)))))
  '("x^2/a" "y^2/(a+b)" "x/a+y/b" "x/(a/b)" "(x/a+y/b)^2" "(x/(a+b))^2"))
(test-equal "parameter rational equality verifies by exact residual" 'complete
  (math-report-status (φ:check-equation "x/a+x/b" "x*(a+b)/(a*b)" #:assumptions facts)))
(for-each (lambda (source)
            (test-equal "coordinate denominators remain outside polynomial calculus" 'unsupported-expression
              (math-report-reason (φ:antiderivative source "x"
                #:assumptions (append facts '("nonzero(x); nonzero(a+x)"))))))
          '("1/x" "1/(a+x)"))
(test-equal "parameter denominator requires a nonzero premise" 'missing-premise
  (math-report-reason (φ:antiderivative "x/a" "x" #:assumptions (list basis "parameter(a)"))))
(let ((result (math-report-result (φ:antiderivative "a/a" "x" #:assumptions facts))))
  (test-equal "cancelled coefficient retains its nonzero premise" 'missing-premise
    (math-report-reason (φ:compute result #:assumptions (list basis "parameter(a); parameter(b)"))))
  (test-equal "cancelled coefficient retains its parameter role" 'missing-premise
    (math-report-reason (φ:compute result #:assumptions (list basis "scalar(a); smooth(a); nonzero(a); parameter(b)")))))
(test-end "rational parameter coefficients")
