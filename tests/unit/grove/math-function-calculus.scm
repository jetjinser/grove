;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (grove math expression) (grove math layout)
             ((grove math semantic) #:prefix ◆:) (grove ir) (grove render html)
             (tests unit grove polynomial-oracle) (srfi srfi-1) (srfi srfi-64))
(test-begin "shared ordinary and partial function calculus")
(define basis (◆:space 'x 'y 'z))
(define facts (list basis "function(f,scalar,scalar,scalar); function(g,scalar,scalar,scalar); c2(f); c2(g)"))
(define env (append coordinates (list (cons 'f (scalar '((1 2 1 0) (2 0 2 0))))
                                    (cons 'g (scalar '((1 1 1 0) (1 2 0 0)))))))
(for-each
  (lambda (source)
    (let ((ordinary (φ:differentiate source "x" #:assumptions facts))
          (partial (φ:partial source "x" #:assumptions facts)))
      (test-assert "ordinary function derivative completes" (math-report-complete? ordinary))
      (test-equal "both entrances have the same normal form" (math-report-final partial) (math-report-final ordinary))
      (test-assert "ordinary certificate replays" (math-verification-valid? (math-verify-derivation (math-report-derivation ordinary))))
      (for-each (lambda (step)
                  (test-equal "independent templates verify every ordinary chain edge"
                    (evaluate (math-derivation-step-from step) env)
                    (evaluate (math-derivation-step-to step) env)))
                (math-derivation-steps (math-report-derivation ordinary)))))
  '("f(x,x)" "f(y,x)" "f(x^2,y)" "f(g(x,y),y^2)" "darg(f(x,y),2)"))
(define unary (list basis "function(h,scalar,scalar); c2(h)"))
(let* ((first (φ:differentiate "h(x^2)" "x" #:assumptions unary))
       (second (φ:differentiate (math-report-result first) "x" #:assumptions unary)))
  (test-assert "C2 permits differentiating the first result again" (math-report-complete? second))
  (let ((env (cons (cons 'h (scalar '((1 3 0 0) (1 1 0 0)))) coordinates)))
    (test-equal "independent oracle checks the second composite derivative"
      (evaluate (φ:term "differentiate(differentiate(h(x^2),x),x)") env)
      (evaluate (math-report-final second) env))
    (for-each (lambda (step)
                (test-equal "independent oracle checks every second derivative edge"
                  (evaluate (math-derivation-step-from step) env)
                  (evaluate (math-derivation-step-to step) env)))
              (math-derivation-steps (math-report-derivation second))))
  (test-equal "C2 does not authorize a third derivative" 'missing-premise
    (math-report-reason (φ:differentiate (math-report-result second) "x" #:assumptions unary)))
  (test-equal "C1 result cannot be reused for a second derivative" 'missing-premise
    (math-report-reason (φ:differentiate (math-report-result first) "x"
      #:assumptions (list basis "function(h,scalar,scalar); differentiable(h)"))))
  (test-assert "unary derivatives render with prime notation"
    (string-contains (sxml->html (content->sxml
      (make-content-element 'math (list (math-value->layout (math-report-result first)))))) "′")))
(test-assert "C1 is sufficient for an ordinary first derivative"
  (math-report-complete? (φ:differentiate "h(x)" "x"
    #:assumptions '("scalar(x); function(h,scalar,scalar); differentiable(h)"))))
(test-equal "pending second derivative requires C2 before execution" 'missing-premise
  (math-report-reason (φ:differentiate "differentiate(h(x),x)" "x"
    #:assumptions (list basis "function(h,scalar,scalar); differentiable(h)"))))
(test-assert "prime and argument-indexed notation have the same semantics"
  (math-report-complete? (φ:check-equation "prime(h,x)" "darg(h(x),1)" #:assumptions unary)))
(test-assert "C2 permits differentiating an authored prime"
  (math-report-complete? (φ:partial "prime(h,x)" "x" #:assumptions unary)))
(let* ((facts (list basis "function(h,scalar,scalar); smooth(h)"))
       (source "differentiate(differentiate(h(x),x),x)")
       (report (φ:differentiate source "x" #:assumptions facts))
       (env (cons (cons 'h (scalar '((1 3 0 0) (1 1 0 0)))) coordinates)))
  (test-assert "smooth authorizes the third derivative" (math-report-complete? report))
  (test-equal "independent oracle checks the third derivative"
    (evaluate (φ:term "differentiate(differentiate(differentiate(h(x),x),x),x)") env)
    (evaluate (math-report-final report) env)))
(test-equal "constant parameters cannot be ordinary differentiation coordinates" 'unsupported-expression
  (math-report-reason (φ:differentiate "h(x)" "a"
    #:assumptions (append unary '("parameter(a)")))))
(test-equal "partial coordinates still require an explicit role" 'missing-premise
  (math-report-reason (φ:partial "h(x)" "x"
    #:assumptions '("scalar(x); function(h,scalar,scalar); differentiable(h)"))))
(test-end "shared ordinary and partial function calculus")
