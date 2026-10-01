;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (grove math pipeline) (grove math expression)
             (tests unit grove polynomial-oracle) ((grove math semantic) #:prefix ◆:)
             (grove math layout) (grove render html) (grove ir)
             (srfi srfi-1) (srfi srfi-64))
(test-begin "multivariate function chain rules")
(define facts (list (◆:space 'x 'y 'z) "function(f,scalar,scalar,scalar); function(g,scalar,scalar,scalar); c2(f); c2(g)"))
(define environment (append coordinates
  (list (cons 'f (scalar '((1 2 1 0) (2 0 2 0))))
        (cons 'g (scalar '((1 1 1 0) (1 2 0 0)))))))
(for-each
  (lambda (source)
    (let* ((report (math-compute source #:assumptions facts))
           (certificate (math-report-derivation report)))
      (test-assert "multivariate derivative completes symbolically" (math-report-complete? report))
      (test-equal "function derivatives remain explicit" 'symbolic (math-report-status report))
      (test-assert "chain-rule certificate replays" (math-verification-valid? (math-verify-derivation certificate)))
      (for-each (lambda (step)
                  (test-equal "independent polynomial templates verify each chain-rule edge"
                    (evaluate (math-derivation-step-from step) environment)
                    (evaluate (math-derivation-step-to step) environment)))
                (math-derivation-steps certificate))))
  '("partial(f(x^2,y),x)" "partial(f(y,x),x)" "partial(f(x,x),x)"
    "partial(f(g(x,y),y^2),x)" "partial(partial(f(x,y),x),y)"))
(test-equal "first argument index is not a variable subscript"
  '(darg (f (var x) (var y)) (num "1"))
  (math-value-term (◆:function-partial "f(x,y)" 1)))
(for-each (lambda (source)
            (test-equal "invalid argument position is rejected" 'type-error
              (math-report-reason (math-compute source #:assumptions facts))))
          '("darg(f(x,y),0)" "darg(f(x,y),3)" "darg(x,1)"))
(test-equal "C1 alone authorizes a single function derivative" 'symbolic
  (math-report-status (math-partial "f(x,y)" "x"
    #:assumptions '("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)"))))
(test-equal "C1 does not authorize a mixed second derivative" 'missing-premise
  (math-report-reason (math-compute "partial(partial(f(x,y),x),y)"
    #:assumptions '("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)"))))
(let* ((report (math-compute "darg(f(x,y),1)*0" #:assumptions facts))
       (result (math-report-result report)))
  (test-equal "cancelling a derivative preserves its regularity premise" 'missing-premise
    (math-report-reason (math-simplify result
      #:assumptions (list (◆:space 'x 'y 'z) "function(f,scalar,scalar,scalar)")))))
(test-end "multivariate function chain rules")
