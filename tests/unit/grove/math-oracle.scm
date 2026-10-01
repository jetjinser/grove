;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math rewrite) (grove math term)
             (srfi srfi-1) (srfi srfi-64) (ice-9 match))

;; Independent sparse polynomials in three coordinates. Coefficients are exact
;; rationals; exponent vectors are ordinary lists, not Grove's expression IR.
;; Neither differentiation nor normalization below calls any Grove math rule.
(define (normalize monomials)
  (let ((exponents (delete-duplicates (map cdr monomials) equal?)))
    (sort
      (filter-map (lambda (powers)
                    (let ((coefficient (apply + (map car (filter (lambda (m) (equal? powers (cdr m))) monomials)))))
                      (and (not (zero? coefficient)) (cons coefficient powers)))) exponents)
      (lambda (left right) (string<? (format #f "~s" (cdr left)) (format #f "~s" (cdr right)))))))
(define (constant n) (normalize (list (list n 0 0 0))))
(define (add a b) (normalize (append a b)))
(define (scale n a) (normalize (map (lambda (m) (cons (* n (car m)) (cdr m))) a)))
(define (multiply a b)
  (normalize (append-map (lambda (u) (map (lambda (v) (cons (* (car u) (car v)) (map + (cdr u) (cdr v)))) b)) a)))
(define (partial polynomial coordinate)
  (normalize (filter-map
    (lambda (m)
      (let ((exponent (list-ref (cdr m) coordinate)))
        (and (> exponent 0)
             (cons (* (car m) exponent)
                   (map (lambda (power index) (if (= index coordinate) (- power 1) power))
                        (cdr m) '(0 1 2)))))) polynomial)))
(define (gradient scalar) (map (lambda (index) (partial scalar index)) '(0 1 2)))
(define (divergence vector) (fold add '() (map partial vector '(0 1 2))))
(define (curl vector)
  (map (lambda (i j) (add (partial (list-ref vector j) i) (scale -1 (partial (list-ref vector i) j))))
       '(1 2 0) '(2 0 1)))
(define (laplacian scalar) (divergence (gradient scalar)))
(define (substitute polynomial arguments)
  (define (power polynomial exponent)
    (if (= exponent 0) (constant 1) (multiply polynomial (power polynomial (- exponent 1)))))
  (fold add '()
    (map (lambda (monomial)
           (fold multiply (constant (car monomial)) (map power arguments (cdr monomial)))) polynomial)))
(define (scalar polynomial) (cons 'scalar polynomial))
(define (vector field) (cons 'vector field))
(define (map-value procedure value)
  (if (eq? (car value) 'scalar) (scalar (procedure (cdr value)))
      (vector (map procedure (cdr value)))))
(define (combine a b)
  (unless (eq? (car a) (car b)) (error "oracle type mismatch" a b))
  (if (eq? (car a) 'scalar) (scalar (add (cdr a) (cdr b)))
      (vector (map add (cdr a) (cdr b)))))
(define (evaluate term environment)
  (define (go t) (evaluate t environment))
  (match term
    (('num n) (scalar (constant (string->number n))))
    (('var name) (or (assq-ref environment name) (error "oracle variable" name)))
    (('zero 'vector) (vector '(() () ())))
    (('- a) (map-value (lambda (p) (scale -1 p)) (go a)))
    (('+ a b) (combine (go a) (go b)))
    (('- a b) (combine (go a) (map-value (lambda (p) (scale -1 p)) (go b))))
    (('* a b)
     (let ((u (go a)) (v (go b)))
       (if (eq? (car u) 'scalar) (map-value (lambda (p) (multiply (cdr u) p)) v)
           (map-value (lambda (p) (multiply p (cdr v))) u))))
    (('/ a b)
     (let* ((denominator (cdr (go b)))
            (n (if (equal? denominator '()) 0
                   (if (and (= (length denominator) 1) (equal? (cdar denominator) '(0 0 0)))
                       (caar denominator) (error "oracle only accepts constant division" term)))))
       (map-value (lambda (p) (scale (/ n) p)) (go a))))
    (('dot a b)
     (scalar (fold add '() (map multiply (cdr (go a)) (cdr (go b))))))
    (('cross a b)
     (let ((u (cdr (go a))) (v (cdr (go b))))
       (vector (map (lambda (i j)
                      (add (multiply (list-ref u i) (list-ref v j))
                           (scale -1 (multiply (list-ref u j) (list-ref v i)))))
                    '(1 2 0) '(2 0 1)))))
    (('^ a ('num n))
     (let ((base (cdr (go a))))
       (scalar (let power ((n (string->number n)))
                 (if (zero? n) (constant 1) (multiply base (power (- n 1))))))))
    (('grad a) (vector (gradient (cdr (go a)))))
    (('div a) (scalar (divergence (cdr (go a)))))
    (('curl a) (vector (curl (cdr (go a)))))
    (('laplacian a) (map-value laplacian (go a)))
    (('differentiate a ('var name))
     (map-value (lambda (p) (partial p (or (list-index (lambda (v) (eq? v name)) '(x y z))
                                         (error "oracle coordinate" name)))) (go a)))
    ((name a b c)
     (let ((template (assq-ref environment name)) (arguments (map (lambda (arg) (cdr (go arg))) (list a b c))))
       (unless template (error "oracle unknown field" name))
       (map-value (lambda (polynomial) (substitute polynomial arguments)) template)))
    (_ (error "oracle unsupported expression" term))))

(define facts '((scalar (var x)) (scalar (var y)) (scalar (var z))
                (scalar (var f)) (smooth (var f)) (vector (var E)) (smooth (var E))))
(define coordinates
  (map (lambda (name index) (cons name (scalar (list (cons 1 (map (lambda (i) (if (= i index) 1 0)) '(0 1 2)))))))
       '(x y z) '(0 1 2)))
(define checked '())
(define (check-edges source environment facts)
  (let ((input (math-source->term source)))
    (for-each
      (lambda (answer)
        (set! checked (cons (cadr answer) checked))
        (test-equal (format #f "independent polynomial semantics: ~a via ~a" source (cadr answer))
          (evaluate input environment) (evaluate (car answer) environment)))
      (math-rewrite-answers facts input (math-rule-names) #f))))

(test-begin "independent mathematical rule oracle")
(for-each
  (lambda (seed)
    (let* ((f (normalize `((,seed 3 1 0) (2 0 2 1) (-1 1 0 2) (1 0 0 0))))
           (field (list f (normalize `((2 1 1 1) (,(- seed) 0 3 0)))
                          (normalize '((3 2 0 1) (-2 0 1 2)))))
           (environment (append coordinates (list (cons 'f (scalar f)) (cons 'E (vector field))))))
      (for-each (lambda (source) (check-edges source environment facts))
        '("curl(curl(E))" "div(curl(E))" "curl(grad(f))" "div(grad(f))"
          "grad(f*f)" "div(f*E)" "curl(f*E)"
          "E+zero(vector)" "zero(vector)-E" "E*1" "-(-E)"
          "x+0" "0+x" "x-0" "0-x" "x*1" "1*x" "x*0" "0*x"
          "x^0" "x^1" "-(-x)" "2*3" "1/2+1/3" "2-5" "2*(3*x)"
          "x+x" "2*x+3*x" "x-y+x" "(1/2)*x+(1/3)*x" "(x*2)/2" "(x+y)*x"
          "differentiate(7,x)" "differentiate(x,x)" "differentiate(y,x)"
          "differentiate(-x,x)" "differentiate(x+y,x)" "differentiate(x-y,x)"
          "differentiate(x*y,x)" "differentiate((x+y)^3,x)"))
      (let ((field-facts
              '((coordinate (var x)) (coordinate (var y)) (coordinate (var z))
                (space (var x) (var y) (var z))
                (field (var f) (var scalar) (var x) (var y) (var z))
                (field (var E) (var vector) (var x) (var y) (var z))
                (smooth (var f)) (smooth (var E)))))
        (for-each (lambda (source) (check-edges source environment field-facts))
          '("grad(f(x,y,z)*f(y,x,z))" "div(f(x,y,z)*E(x,y,z))"
            "curl(f(x,y,z)*E(x,y,z))" "curl(curl(E(x,y,z)))"
            "curl(grad(f(x,y,z)))" "div(curl(E(x,y,z)))")))
      (let* ((source (format #f "(x+y)^~a+(1/2)*x*y-x*x" (abs seed)))
             (report (math-differentiate source "x" #:assumptions '("scalar(x); scalar(y)"))))
        (test-equal "composed differentiation and simplification agree with independent exact polynomials"
          (evaluate (math-source->term (format #f "differentiate(~a,x)" source)) environment)
          (evaluate (math-report-final report) environment)))
      (let* ((solenoidal (curl field))
             (environment (append coordinates (list (cons 'E (vector solenoidal)))))
             (premises (cons '(= (div (var E)) (num "0")) facts)))
        (test-equal "oracle independently establishes the divergence-free premise"
          '() (divergence solenoidal))
        (check-edges "grad(div(E))" environment premises))))
  '(-3 0 2))
(for-each
  (lambda (rule)
    (test-assert (format #f "independent oracle exercised registered rule ~a" rule) (memq rule checked)))
  ;; Elementary real calculus is independently checked by the dual-number
  ;; interpreter in differentiation.scm; this interpreter is exact polynomial.
  (lset-difference eq? (math-rule-names)
    '(differentiate-quotient differentiate-sin differentiate-cos differentiate-exp differentiate-log differentiate-function)))
(for-each
  (lambda (fixture)
    (test-equal "missing regularity or equation premises cannot produce a vector identity"
      '() (math-rewrite-answers (car fixture) (math-source->term (cadr fixture)) (list (caddr fixture)) #f)))
  '((((vector (var E))) "curl(curl(E))" curl-curl-identity)
    (((vector (var E))) "div(curl(E))" div-curl-zero)
    (((scalar (var f))) "curl(grad(f))" curl-grad-zero)
    (((scalar (var f))) "div(grad(f))" div-grad-laplacian)
    (((vector (var E)) (smooth (var E))) "grad(div(E))" divergence-free)))
(test-end "independent mathematical rule oracle")
