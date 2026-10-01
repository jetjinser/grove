;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math differentiation)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math types)
  #:use-module (grove math arithmetic)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-13)
  #:export (polynomial-term? differentiation-stepo math-differentiation-rules))

(define math-differentiation-rules
  '(differentiate-constant differentiate-variable differentiate-independent
    differentiate-negation differentiate-sum differentiate-difference
    differentiate-product differentiate-power))

(define (natural-value value)
  (and (string? value) (> (string-length value) 0)
       (string-every (lambda (c) (char<=? #\0 c #\9)) value)
       (string->number value)))

(define (polynomial-term? term)
  (or (and (math-number-value term) #t)
      (match term
    (('var (? symbol?)) #t)
    (('num value) (and (natural-value value) #t))
    (('- operand) (polynomial-term? operand))
    (((or '+ '- '*) left right)
     (and (polynomial-term? left) (polynomial-term? right)))
    (('^ base ('num exponent))
     (and (polynomial-term? base) (natural-value exponent) #t))
    (_ #f))))

;; Constructive forward relation and result checking. The numeric bridge is
;; deliberately ground: arbitrary inverse generation/integration is not a mode
;; of this polynomial pass. All scalar variables are independent coordinates.
(define (differentiation-stepo assumptions input output rule)
  (fresh (expression variable)
    (== input `(differentiate ,expression (var ,variable)))
    (project (expression variable)
      (if (and (symbol? variable) (polynomial-term? expression)) succeed fail))
    (math-typeo assumptions expression 'scalar)
    (math-typeo assumptions `(var ,variable) 'scalar)
    (conde
      ((== rule 'differentiate-constant)
       (project (expression)
         (if (math-number-value expression) (== output '(num "0")) fail)))
      ((== expression `(var ,variable)) (== output '(num "1"))
       (== rule 'differentiate-variable))
      ((fresh (other)
         (== expression `(var ,other)) (=/= variable other)
         (== output '(num "0")) (== rule 'differentiate-independent)))
      ((fresh (operand)
         (== expression `(- ,operand))
         (== output `(- (differentiate ,operand (var ,variable))))
         (== rule 'differentiate-negation)))
      ((fresh (left right operator)
         (conde ((== operator '+) (== rule 'differentiate-sum))
                ((== operator '-) (== rule 'differentiate-difference)))
         (== expression `(,operator ,left ,right))
         (== output `(,operator (differentiate ,left (var ,variable))
                               (differentiate ,right (var ,variable))))))
      ((fresh (left right)
         (== expression `(* ,left ,right))
         (== output `(+ (* (differentiate ,left (var ,variable)) ,right)
                        (* ,left (differentiate ,right (var ,variable)))))
         (== rule 'differentiate-product)))
      ((fresh (base exponent)
         (== expression `(^ ,base (num ,exponent)))
         (== rule 'differentiate-power)
         (project (exponent)
           (let ((n (natural-value exponent)))
             (if (= n 0)
                 (== output '(num "0"))
                 (== output `(* (* (num ,exponent)
                                    (^ ,base (num ,(number->string (- n 1)))))
                                 (differentiate ,base (var ,variable))))))))))))
