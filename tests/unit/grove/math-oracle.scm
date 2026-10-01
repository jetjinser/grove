;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math rewrite) (grove math term)
             (tests unit grove polynomial-oracle)
             (srfi srfi-1) (srfi srfi-64))

(define facts '((scalar (var x)) (scalar (var y)) (scalar (var z))
                (scalar (var f)) (smooth (var f)) (vector (var E)) (smooth (var E))))
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
            "curl(grad(f(x,y,z)))" "div(curl(E(x,y,z)))"))
        (for-each (lambda (source) (check-edges source environment field-facts))
          '("partial(7,x)" "partial(x,x)" "partial(y,x)" "partial(-x,x)"
            "partial(x+y,x)" "partial(x-y,x)" "partial(x*y,x)"
            "partial((x+y)^3,x)" "partial(x/2,x)"
            "components(x+0,y*1,z^2)")))
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
    '(differentiate-quotient differentiate-sin differentiate-cos differentiate-exp differentiate-log differentiate-function
      partial-sin partial-cos partial-exp partial-log partial-function)))
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
