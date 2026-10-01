;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math term)
             (grove math differentiation) (grove relation)
             (grove math layout) (grove ir) (grove render html)
             (grove math source)
             (ice-9 match) (srfi srfi-1) (srfi srfi-64))

(define (expansion report) (math-report-derivation (car (math-report-passes report))))
(define (simplification report) (math-report-derivation (cadr (math-report-passes report))))

(define facts '("scalar(x); scalar(y)"))
(define (differentiate source)
  (φ:differentiate source "x" #:assumptions facts))

;; Independent oracle: evaluate the original polynomial over dual numbers.
;; Compare its tangent with ordinary evaluation of the generated derivative.
(define (dual term x y)
  (define (go term) (dual term x y))
  (match term
    (('num n) (cons (string->number n) 0))
    (('var 'x) (cons x 1))
    (('var 'y) (cons y 0))
    (((or 'differentiate 'partial) operand ('var 'x)) (cons (cdr (go operand)) 0))
    (('f operand)
     (let ((v (go operand)))
       (cons (+ (expt (car v) 3) (car v)) (* (cdr v) (+ (* 3 (expt (car v) 2)) 1)))))
    (('prime ('var 'f) operand)
     (let ((v (go operand))) (cons (+ (* 3 (expt (car v) 2)) 1) (* 6 (car v) (cdr v)))))
    (('darg ('f operand) ('num "1"))
     (let ((v (go operand))) (cons (+ (* 3 (expt (car v) 2)) 1) (* 6 (car v) (cdr v)))))
    (('- a) (let ((v (go a))) (cons (- (car v)) (- (cdr v)))))
    (((or 'sin 'cos 'exp 'log) a)
     (let* ((v (go a)) (n (car v)) (d (cdr v)))
       (case (car term)
         ((sin) (cons (sin n) (* d (cos n))))
         ((cos) (cons (cos n) (- (* d (sin n)))))
         ((exp) (cons (exp n) (* d (exp n))))
         ((log) (cons (log n) (/ d n))))))
    (('/ a b)
     (let ((u (go a)) (v (go b)))
       (cons (/ (car u) (car v))
             (/ (- (* (cdr u) (car v)) (* (car u) (cdr v))) (* (car v) (car v))))))
    (((or '+ '- '*) a b)
     (let ((u (go a)) (v (go b)))
       (case (car term)
         ((+) (cons (+ (car u) (car v)) (+ (cdr u) (cdr v))))
         ((-) (cons (- (car u) (car v)) (- (cdr u) (cdr v))))
         ((*) (cons (* (car u) (car v))
                    (+ (* (cdr u) (car v)) (* (car u) (cdr v))))))))
    (('^ base ('num n))
     (let ((v (go base)) (n (string->number n)))
       (cons (expt (car v) n)
             (if (= n 0) 0 (* n (expt (car v) (- n 1)) (cdr v))))))))

(test-begin "polynomial differentiation")
(for-each
  (lambda (fixture)
    (let ((report (φ:differentiate (car fixture) "x"
                    #:assumptions '("coordinate(x); parameter(a); positive(x)"))))
      (test-equal "elementary acceptance derivative"
        (φ:term (cadr fixture)) (math-report-final report))
      (test-assert "elementary derivation certificate replays"
        (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
      (test-assert "generated elementary steps retain their source origins"
        (any math-origin-generated? (math-derivation-sources (math-report-derivation report))))))
  '(("sin(x^2)" "2*x*cos(x^2)") ("exp(a*x)" "a*exp(a*x)") ("log(x)" "1/x")))
(for-each
  (lambda (source)
    (let ((report (φ:differentiate source "x" #:assumptions '("scalar(x); positive(x)"))))
      (test-assert "elementary derivative completes" (math-report-complete? report))
      (for-each
        (lambda (point)
          (test-assert (string-append "independent dual-number calculus oracle: " source)
            (< (abs (- (cdr (dual (φ:term source) point 3))
                       (car (dual (math-report-final report) point 3)))) 1e-8)))
        '(1/2 1 2))
      (for-each
        (lambda (step)
          (test-assert "independent calculus oracle checks every raw and simplified edge"
            (< (abs (- (car (dual (math-derivation-step-from step) 1 3))
                       (car (dual (math-derivation-step-to step) 1 3)))) 1e-8)))
        (math-derivation-steps (math-report-derivation report)))))
  '("sin(x^2)" "cos(x^3)" "exp(2*x)" "log(x)" "x/x^2"
    "sin(exp(x))/x" "log(exp(x))"))
(for-each
  (lambda (source)
    (let ((report (φ:partial source "x"
                    #:assumptions '("coordinate(x); coordinate(y); positive(x); function(f,scalar,scalar); smooth(f)"))))
      (test-assert "elementary partial derivative completes" (math-report-complete? report))
      (test-assert "elementary partial certificate independently replays"
        (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
      (for-each
        (lambda (point)
          (test-assert "independent dual-number oracle checks partial results"
            (< (abs (- (cdr (dual (φ:term source) point 3))
                       (car (dual (math-report-final report) point 3)))) 1e-8)))
        '(1/2 1 2))
      (for-each
        (lambda (step)
          (test-assert "independent dual-number oracle checks every partial edge"
            (< (abs (- (car (dual (math-derivation-step-from step) 1 3))
                       (car (dual (math-derivation-step-to step) 1 3)))) 1e-8)))
        (math-derivation-steps (math-report-derivation report)))))
  '("sin(x*y)" "cos(x^3)" "exp(2x)" "log(x)" "x/x^2" "f(x^2)"))
(test-equal "logarithm requires positivity" 'missing-premise
  (math-report-reason (differentiate "log(x)")))
(test-equal "quotient requires nonzero denominator" 'missing-premise
  (math-report-reason (differentiate "x/y")))
(test-equal "logarithm rejects a nonpositive exact argument" 'domain-error
  (math-report-reason (differentiate "log(-1)")))
(test-equal "undefined factors cannot be simplified away" 'missing-premise
  (math-report-reason (φ:simplify "0*(1/x)" #:assumptions '("scalar(x)"))))
(test-equal "false constant sign premises are rejected" 'invalid-assumption
  (math-report-reason (φ:simplify "1" #:assumptions '("positive(-1)"))))
(test-equal "vector sign premises are rejected" 'type-error
  (math-report-reason (φ:simplify "1" #:assumptions '("vector(E); positive(E)"))))
(let ((report (φ:differentiate "f(x^2)" "x"
                #:assumptions '("coordinate(x); function(f,scalar,scalar); smooth(f)"))))
  (test-equal "declared unary functions use the chain rule"
    (φ:term "2*x*darg(f(x^2),1)") (math-report-final report))
  (test-assert "function chain certificate replays"
    (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
  (for-each
    (lambda (step)
      (test-assert "independent function fixture checks every chain-rule edge"
        (< (abs (- (car (dual (math-derivation-step-from step) 2 3))
                   (car (dual (math-derivation-step-to step) 2 3)))) 1e-8)))
    (math-derivation-steps (math-report-derivation report)))
  ;; Independent fixture f(t)=t^3+t, so f'(t)=3*t^2+1.
  (test-equal "function chain matches a separately computed derivative"
    196
    (match (math-report-final report)
      (('* ('* ('num coefficient) ('var 'x)) ('darg ('f ('^ ('var 'x) ('num exponent))) ('num "1")))
       (* (string->number coefficient) 2 (+ (* 3 (expt (expt 2 (string->number exponent)) 2)) 1))))))
(test-equal "undeclared smoothness is diagnosed before differentiation" 'missing-premise
  (math-report-reason (φ:differentiate "f(x)" "x"
                       #:assumptions '("coordinate(x); function(f,scalar,scalar)"))))
(test-equal "prime participates in the shared higher derivative semantics"
  (φ:term "darg(darg(f(x),1),1)")
  (math-report-final (φ:differentiate "prime(f,x)" "x"
                       #:assumptions '("coordinate(x); function(f,scalar,scalar); c2(f)"))))
(test-equal "prime requires a unary scalar function" 'type-error
  (math-report-reason (φ:simplify "prime(f,x)"
                       #:assumptions '("coordinate(x); function(f,scalar,vector); smooth(f)"))))
(for-each
  (lambda (chain)
    (test-assert "authored elementary differentiation steps verify"
      (math-verification-valid? (φ:check-chain chain
        #:assumptions '("scalar(x); positive(x)")))))
  '(("differentiate(sin(x),x)" "differentiate(x,x)*cos(x)" "1*cos(x)" "cos(x)")
    ("differentiate(log(x),x)" "differentiate(x,x)/x" "1/x")
    ("differentiate(x/x,x)" "(differentiate(x,x)*x-x*differentiate(x,x))/x^2")))
(test-assert "bounded search can find an elementary differentiation step"
  (pair? (math-report-candidates (φ:search "differentiate(sin(x),x)"
    "differentiate(x,x)*cos(x)" #:assumptions '("scalar(x)") #:max-depth 1))))
(test-assert "polynomial layout preserves grouping of product and power bases"
  (let ((html (sxml->html (content->sxml
                (make-content-element 'math
                  (list (math-term->layout (φ:term "(x+1)*(x-1)^2"))))))))
    (and (string-contains html "<msup>")
         (string-contains html "<mo>(</mo>"))))
(let ((result (differentiate "x^3 + 2*x")))
  (test-equal "acceptance polynomial has the expected simplified derivative"
    (φ:term "3*x^2 + 2") (math-report-final result))
  (for-each
    (lambda (pass)
      (test-assert "each pass independently replays its certificate"
        (math-verification-valid? (math-verify-derivation pass))))
    (list (expansion result)
          (simplification result)))
  (test-assert "expansion contains only differentiation provenance"
    (every (lambda (step) (memq (math-derivation-step-rule step) math-differentiation-rules))
           (math-derivation-steps (expansion result))))
  (test-equal "passes share exactly their boundary term"
    (last (math-derivation-terms (expansion result)))
    (car (math-derivation-terms (simplification result)))))
(for-each
  (lambda (source)
    (let ((answer (math-report-final (differentiate source))))
      (for-each
        (lambda (point)
          (test-equal (string-append "dual-number oracle: " source)
            (cdr (dual (φ:term source) point 3))
            (car (dual answer point 3))))
        '(-2 0 1 4))))
  '("7" "y" "x" "-x" "x-y" "x*x" "(x+1)^3" "x^0" "x^1"
    "(x-y)*(x+y)" "-(x^2+x)" "x^3+2*x"))
(for-each
  (lambda (source)
    (test-equal "unsupported domain is diagnosed before expansion"
      'unsupported-expression (math-report-reason (differentiate source))))
  '("opaque(x)" "x^y" "x^-1"))
(test-equal "vector coordinates cannot be differentiated as scalar polynomials"
  'type-error (math-report-reason (φ:differentiate "E" "E" #:assumptions '("vector(E)"))))
(test-equal "undeclared variables are diagnosed" 'type-error
  (math-report-reason (φ:differentiate "x" "x")))
(test-equal "expansion budget is enforced" 'step-limit
  (math-report-reason (φ:differentiate "x^3" "x" #:assumptions facts #:max-steps 0)))
(test-equal "term-size budget is enforced" 'term-size-limit
  (math-report-reason (φ:differentiate "x^3" "x" #:assumptions facts #:max-term-size 2)))
(test-equal "simplification budget is enforced" 'step-limit
  (math-report-reason (φ:differentiate "x^1" "x" #:assumptions facts #:max-steps 2)))
(test-equal "relation can verify a proposed derivative step"
  '(differentiate-product)
  (run* (rule)
    (differentiation-stepo '((scalar (var x)))
      '(differentiate (* (var x) (var x)) (var x))
      '(+ (* (differentiate (var x) (var x)) (var x))
          (* (var x) (differentiate (var x) (var x)))) rule)))
(test-assert "handwritten differentiation uses the common verifier"
  (math-verification-valid?
    (φ:check-chain '("differentiate(x^3,x)" "3*x^2*differentiate(x,x)"
                        "3*x^2*1" "3*x^2") #:assumptions facts)))
(test-assert "incorrect handwritten derivative fails verification"
  (not (math-verification-valid?
         (φ:check-chain '("differentiate(x,x)" "2") #:assumptions facts))))
(test-equal "incorrect variable derivative diagnoses the structurally matching rule"
  'differentiate-variable
  (math-diagnostic-rule
    (car (math-verification-diagnostics
           (φ:check-chain '("differentiate(x,x)" "2") #:assumptions facts)))))
(test-assert "target search finds and verifies derivative steps"
  (let ((report (φ:search "differentiate(x,x)" "1" #:assumptions facts)))
    (and (pair? (math-report-candidates report))
         (math-verification-valid?
           (math-verify-derivation (car (math-report-candidates report)))))))
(test-end "polynomial differentiation")
