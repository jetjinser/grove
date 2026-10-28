;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math expression) (grove math private premise) (grove math context)
             ((grove math semantic) #:prefix ◆:) (tests unit grove polynomial-oracle)
             (srfi srfi-1) (srfi srfi-64))
(test-begin "primitive rational parameter coefficients")
(define basis (◆:space 'x 'y 'z))
(define facts (list basis "parameter(a); parameter(b); nonzero(a); nonzero(b); nonzero(a+b); nonzero(a^2+a*b)"))
(define (reduce-coefficients source)
  (φ:rewrite source #:assumptions facts #:rules '(expand-polynomial)))
(define generated-cases
  (append-map
    (lambda (factor)
      (map (lambda (body)
             (format #f "((~a)^2*(~a))/(~a)^3" factor body factor))
           '("x" "y" "x+y")))
    '("a" "b" "a+b")))
(for-each
  (lambda (source)
    (let* ((report (reduce-coefficients source)) (result (math-report-result report)))
      (test-assert (format #f "reduction completes: ~a" source) (math-report-complete? report))
      (test-assert "coefficient reduction certificate replays"
        (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
      (test-equal "reduction has a stable normal form" (math-report-final report)
        (math-report-final (reduce-coefficients result)))
      (for-each
        (lambda (values)
          (let ((env (append coordinates
                       (list (cons 'a (scalar (list (list (car values) 0 0 0))))
                             (cons 'b (scalar (list (list (cadr values) 0 0 0))))))))
            (test-equal "independent specialization checks coefficient reduction"
              (evaluate (φ:term source) env) (evaluate (math-report-final report) env))
            (for-each (lambda (step)
                        (test-equal "each coefficient edge preserves independent arithmetic"
                          (evaluate (math-derivation-step-from step) env)
                          (evaluate (math-derivation-step-to step) env)))
                      (math-derivation-steps (math-report-derivation report)))))
        '((2 3) (-2 5) (1/2 -3/4)))))
  (append generated-cases
    '("(a^2*x+a^2*y)/a^3" "(6*a*x)/(9*a^2)" "(a*x+a*y)/(-2*a^2)"
      "a*x/(a^2+a*b)" "(x/(a+b))^2" "x/((a+b)^2/2)" "x/(-a)" "a/a" "a/a-a/a")))
(test-equal "bounded coefficient grammar produces the expected nine cases" 9
  (length generated-cases))
(test-equal "common monomial factors disappear from a sum numerator"
  (math-report-final (reduce-coefficients "(x+y)/a"))
  (math-report-final (reduce-coefficients "(a^2*x+a^2*y)/a^3")))
(test-equal "expanded parameter denominator can reduce to a primitive sum"
  (math-report-final (reduce-coefficients "x/(a+b)"))
  (math-report-final (reduce-coefficients "a*x/(a^2+a*b)")))
(test-equal "denominator sign is moved to the numerator"
  (math-report-final (reduce-coefficients "(-x)/a"))
  (math-report-final (reduce-coefficients "x/(-a)")))
(let ((result (math-report-result (reduce-coefficients "a/a"))))
  (test-equal "a cancelled denominator still requires the author's domain" 'missing-premise
    (math-report-reason (φ:simplify result #:assumptions (list basis "parameter(a); parameter(b)")))))
(test-assert "a nonzero expanded denominator entails its reduced primitive factor"
  (math-premise-known? (math-assumptions (append (list basis) (φ:assumptions "parameter(a); parameter(b); nonzero(a^2+a*b)")))
                      (φ:term "nonzero(a+b)")))
(test-assert "nonzero sum never implies nonzero individual summands"
  (not (math-premise-known? (math-assumptions (append (list basis) (φ:assumptions "parameter(a); parameter(b); nonzero(a+b)")))
                           (φ:term "nonzero(a)"))))
(test-assert "positive product entails nonzero factors but not their signs"
  (let ((facts (math-assumptions (append (list basis) (φ:assumptions "parameter(a); parameter(b); positive(a*b)")))))
    (and (math-premise-known? facts (φ:term "nonzero(a)"))
         (not (math-premise-known? facts (φ:term "positive(a)"))))))
(test-end "primitive rational parameter coefficients")
