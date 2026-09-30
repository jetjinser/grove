;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math term)
             (grove math differentiation) (grove relation)
             (grove math layout) (grove ir) (grove render html)
             (ice-9 match) (srfi srfi-1) (srfi srfi-64))

(define facts '("scalar(x); scalar(y)"))
(define (differentiate source)
  (math-differentiate source "x" #:assumptions facts))

;; Independent oracle: evaluate the original polynomial over dual numbers.
;; Compare its tangent with ordinary evaluation of the generated derivative.
(define (dual term x y)
  (define (go term) (dual term x y))
  (match term
    (('num n) (cons (string->number n) 0))
    (('var 'x) (cons x 1))
    (('var 'y) (cons y 0))
    (('- a) (let ((v (go a))) (cons (- (car v)) (- (cdr v)))))
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
(test-assert "polynomial layout preserves grouping of product and power bases"
  (let ((html (sxml->html (content->sxml
                (make-content-element 'math
                  (list (math-term->layout (math-source->term "(x+1)*(x-1)^2"))))))))
    (and (string-contains html "<msup>")
         (string-contains html "<mo>(</mo>"))))
(let ((result (differentiate "x^3 + 2*x")))
  (test-equal "acceptance polynomial has the expected simplified derivative"
    (math-source->term "3*x^2 + 2") (math-differentiation-final result))
  (for-each
    (lambda (pass)
      (test-assert "each pass independently replays its certificate"
        (math-verification-valid? (math-verify-derivation pass))))
    (list (math-differentiation-expansion result)
          (math-differentiation-simplification result)))
  (test-assert "expansion contains only differentiation provenance"
    (every (lambda (step) (memq (math-derivation-step-rule step) math-differentiation-rules))
           (math-derivation-steps (math-differentiation-expansion result))))
  (test-equal "passes share exactly their boundary term"
    (last (math-derivation-terms (math-differentiation-expansion result)))
    (car (math-derivation-terms (math-differentiation-simplification result)))))
(for-each
  (lambda (source)
    (let ((answer (math-differentiation-final (differentiate source))))
      (for-each
        (lambda (point)
          (test-equal (string-append "dual-number oracle: " source)
            (cdr (dual (math-source->term source) point 3))
            (car (dual answer point 3))))
        '(-2 0 1 4))))
  '("7" "y" "x" "-x" "x-y" "x*x" "(x+1)^3" "x^0" "x^1"
    "(x-y)*(x+y)" "-(x^2+x)" "x^3+2*x"))
(for-each
  (lambda (source)
    (test-error "unsupported domain is rejected before expansion" #t (differentiate source)))
  '("sin(x)" "x/y" "x^y" "x^-1"))
(test-error "vector coordinates cannot be differentiated as scalar polynomials" #t
  (math-differentiate "E" "E" #:assumptions '("vector(E)")))
(test-error "undeclared variables are diagnosed" #t (math-differentiate "x" "x"))
(test-error "expansion budget is enforced" #t
  (math-differentiate "x^3" "x" #:assumptions facts #:max-steps 0))
(test-error "term-size budget is enforced" #t
  (math-differentiate "x^3" "x" #:assumptions facts #:max-term-size 2))
(test-error "simplification budget is enforced" #t
  (math-differentiate "x^1" "x" #:assumptions facts #:max-steps 2))
(test-equal "relation can verify a proposed derivative step"
  '(differentiate-product)
  (run* (rule)
    (differentiation-stepo '((scalar (var x)))
      '(differentiate (* (var x) (var x)) (var x))
      '(+ (* (differentiate (var x) (var x)) (var x))
          (* (var x) (differentiate (var x) (var x)))) rule)))
(test-assert "handwritten differentiation uses the common verifier"
  (math-verification-valid?
    (math-check-chain '("differentiate(x^3,x)" "3*x^2*differentiate(x,x)"
                        "3*x^2*1" "3*x^2") #:assumptions facts)))
(test-assert "incorrect handwritten derivative fails verification"
  (not (math-verification-valid?
         (math-check-chain '("differentiate(x,x)" "2") #:assumptions facts))))
(test-equal "incorrect variable derivative diagnoses the structurally matching rule"
  'differentiate-variable
  (math-diagnostic-rule
    (car (math-verification-diagnostics
           (math-check-chain '("differentiate(x,x)" "2") #:assumptions facts)))))
(test-assert "target search finds and verifies derivative steps"
  (let ((report (math-search "differentiate(x,x)" "1" #:assumptions facts)))
    (and (pair? (math-search-report-candidates report))
         (math-verification-valid?
           (math-verify-derivation (car (math-search-report-candidates report)))))))
(test-end "polynomial differentiation")
