;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math vector)
  #:use-module (grove relation)
  #:use-module (grove math rule)
  #:use-module (grove math types)
  #:export (math-vector-rules curl-curlo))

(define math-vector-rules
  (list
    (math-rule 'curl-curl-identity "curl(curl(E)) = grad(div(E)) - laplacian(E)"
      "assumptions declare vector(E) and smooth(E)" 'vector-calculus
      (facts field) (curl (curl ,field)) (- (grad (div ,field)) (laplacian ,field))
      ((smooth ,field)) ((math-typeo facts field 'vector))
      #:backward #t #:backward-default? #t)
    (math-rule 'divergence-free "grad(div(E)) = zero(vector)"
      "vector(E) and the matching fact div(E) = 0" 'vector-calculus
      (facts field) (grad (div ,field)) (zero vector)
      ((= (div ,field) (num "0"))) ((math-typeo facts field 'vector))
      #:backward #t #:backward-default? #t)
    (math-rule 'div-curl-zero "div(curl(E)) = 0"
      "assumptions declare vector(E) and smooth(E)" 'vector-calculus
      (facts field) (div (curl ,field)) (num "0")
      ((smooth ,field)) ((math-typeo facts field 'vector))
      #:backward #t #:backward-default? #t)
    (math-rule 'curl-grad-zero "curl(grad(f)) = zero(vector)"
      "assumptions declare scalar(f) and smooth(f)" 'vector-calculus
      (facts field) (curl (grad ,field)) (zero vector)
      ((smooth ,field)) ((math-typeo facts field 'scalar))
      #:backward #t #:backward-default? #t)
    (math-rule 'div-grad-laplacian "div(grad(f)) = laplacian(f)"
      "assumptions declare scalar(f) and smooth(f)" 'vector-calculus
      (facts field) (div (grad ,field)) (laplacian ,field)
      ((smooth ,field)) ((math-typeo facts field 'scalar))
      #:backward #t #:backward-default? #t)
    (math-rule 'grad-product "grad(f*g) = g*grad(f) + f*grad(g)"
      "scalar C1 fields in Cartesian space" 'vector-calculus
      (facts f g) (grad (* ,f ,g)) (+ (* ,g (grad ,f)) (* ,f (grad ,g)))
      ((differentiable ,f) (differentiable ,g))
      ((math-typeo facts f 'scalar) (math-typeo facts g 'scalar)))
    (math-rule 'div-product "div(f*E) = dot(grad(f),E) + f*div(E)"
      "scalar and vector C1 fields in Cartesian space" 'vector-calculus
      (facts f e) (div (* ,f ,e)) (+ (dot (grad ,f) ,e) (* ,f (div ,e)))
      ((differentiable ,f) (differentiable ,e))
      ((math-typeo facts f 'scalar) (math-typeo facts e 'vector)))
    (math-rule 'curl-product "curl(f*E) = cross(grad(f),E) + f*curl(E)"
      "scalar and vector C1 fields in oriented Cartesian space" 'vector-calculus
      (facts f e) (curl (* ,f ,e)) (+ (cross (grad ,f) ,e) (* ,f (curl ,e)))
      ((differentiable ,f) (differentiable ,e))
      ((math-typeo facts f 'scalar) (math-typeo facts e 'vector)))))

(define (curl-curlo assumptions input output)
  ((math-rewrite-rule-relation (car math-vector-rules) 'check) assumptions input output))
