;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math normalization)
  #:use-module (grove math private scalar)
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
        ;; The general coefficient-reduction pass is procedural; this exact
        ;; conditional identity is the checkable theorem it can expose.
        (math-rule 'cancel-common-factor "(X * A) / A = X"
          "real scalar terms; A is nonzero" 'cancel
          (facts value factor)
          (/ (* ,value ,factor) ,factor) ,value
          ((nonzero ,factor))
          ((math-typeo facts value 'scalar) (math-typeo facts factor 'scalar))
          #:forward? #f
          #:documentation "A check-only cancellation theorem. Its denominator side condition is explicit in the premise and must remain in the surrounding domain requirements.")
        (scalar-rule 'expand-scalar-product 'expand scalar-expand (lambda (input) '()))
        (math-rule 'normalize-function-prime "f'(u) = partial_1 f(u)"
          "Declared unary scalar function with one derivative" 'normalize
          (facts name argument)
          (prime (var ,name) ,argument) (darg (,name ,argument) (num "1"))
          ((differentiable (var ,name)))
          ((math-typeo facts `(prime (var ,name) ,argument) 'scalar)))))
