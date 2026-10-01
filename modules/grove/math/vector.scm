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
      ((vector ,field) (smooth ,field)) ((math-typeo facts field 'vector))
      #:backward #t #:backward-default? #t)
    (math-rule 'curl-grad-zero "curl(grad(f)) = zero(vector)"
      "assumptions declare scalar(f) and smooth(f)" 'vector-calculus
      (facts field) (curl (grad ,field)) (zero vector)
      ((scalar ,field) (smooth ,field)) ((math-typeo facts field 'scalar))
      #:backward #t #:backward-default? #t)
    (math-rule 'div-grad-laplacian "div(grad(f)) = laplacian(f)"
      "assumptions declare scalar(f) and smooth(f)" 'vector-calculus
      (facts field) (div (grad ,field)) (laplacian ,field)
      ((scalar ,field) (smooth ,field)) ((math-typeo facts field 'scalar))
      #:backward #t #:backward-default? #t)))

(define (curl-curlo assumptions input output)
  ((math-rewrite-rule-relation (car math-vector-rules) 'check) assumptions input output))
