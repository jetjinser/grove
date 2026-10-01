;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math normalization)
  #:use-module (grove math scalar)
  #:use-module (grove math types)
  #:use-module (grove math rule)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:export (math-normalization-rules))

(define (scalar-rule name group transform premises)
  (make-math-rewrite-rule name (symbol->string name) "real scalar algebra" group
    (lambda (facts input output)
      (project (input)
        (let ((result (transform input)))
          (if result
              (fresh () (math-typeo facts input 'scalar)
                (math-factso facts (premises input)) (== output result)) fail))))
    #:matcher (lambda (input) (and (transform input) (premises input)))))

(define math-normalization-rules
  (list (scalar-rule 'normalize-scalar 'normalize scalar-normalize (lambda (input) '()))
        (scalar-rule 'cancel-scalar-factors 'cancel scalar-cancel
          (lambda (input) `((nonzero ,(caddr input)))))
        (scalar-rule 'expand-scalar-product 'expand scalar-expand (lambda (input) '()))
        (math-rule 'normalize-function-prime "f'(u) = partial_1 f(u)"
          "Declared unary scalar function with one derivative" 'normalize
          (facts name argument)
          (prime (var ,name) ,argument) (darg (,name ,argument) (num "1"))
          ((differentiable (var ,name)))
          ((math-typeo facts `(prime (var ,name) ,argument) 'scalar)))))
