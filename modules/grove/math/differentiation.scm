;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math differentiation)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math types)
  #:use-module (grove math arithmetic)
  #:use-module (grove math term)
  #:use-module (grove math diagnostic)
  #:use-module (grove math rule)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-1)
  #:export (polynomial-term? differentiation-input-diagnostics differentiation-stepo math-differentiation-rules
            math-differentiation-rule-definitions))

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

(define (differentiation-input-diagnostics expression coordinate)
  (cond
    ((not (eq? (car coordinate) 'var))
     (list (make-math-diagnostic #f #f '(1) 'unsupported-expression '()
             "differentiation requires a coordinate variable")))
    ((polynomial-term? expression) '())
    (else
     (let locate ((term expression) (path '(0)))
       (let ((child (and (memq (car term) '(+ - * ^))
                         (find (lambda (index)
                                 (not (polynomial-term? (list-ref (cdr term) index))))
                               (iota (length (cdr term)))))))
         (if child
             (locate (list-ref (cdr term) child) (append path (list child)))
             (list (make-math-diagnostic #f #f path 'unsupported-expression '()
                     (format #f "polynomial differentiation does not support ~a; expected exact constants, variables, sums, differences, products, and natural-number powers"
                             (math-term->source term))))))))))

;; This family has a constructive forward/check mode. Independent coordinates
;; and natural powers are its domain; arbitrary inverse integration is absent.
(define-syntax polynomial-rule
  (syntax-rules ()
    ((_ name equation (facts coordinate item ...) expression result (goal ...) option ...)
     (math-rule name equation "scalar polynomial in independent scalar coordinates" 'differentiate
       (facts item ... coordinate)
       (differentiate expression (var ,coordinate)) result ()
       ((project (item ... coordinate)
          (if (polynomial-term? (quasiquote expression)) succeed fail))
        (math-typeo facts (quasiquote expression) 'scalar)
        (math-typeo facts `(var ,coordinate) 'scalar)
        goal ...) option ...))))

(define math-differentiation-rule-definitions
  (list
    (polynomial-rule 'differentiate-constant "d(c)/dx = 0"
      (facts coordinate expression) ,expression (num "0")
      ((project (expression) (if (math-number-value expression) succeed fail)))
      #:matcher (lambda (input)
                  (match input
                    (('differentiate expression ('var name))
                     (and (math-number-value expression) '())) (_ #f))))
    (polynomial-rule 'differentiate-variable "d(x)/dx = 1"
      (facts coordinate) (var ,coordinate) (num "1") ())
    (polynomial-rule 'differentiate-independent "d(y)/dx = 0 for independent y"
      (facts coordinate other) (var ,other) (num "0") ((=/= other coordinate))
      #:matcher (lambda (input)
                  (match input
                    (('differentiate ('var other) ('var coordinate))
                     (and (not (eq? other coordinate)) '())) (_ #f))))
    (polynomial-rule 'differentiate-negation "d(-f)/dx = -d(f)/dx"
      (facts coordinate operand) (- ,operand) (- (differentiate ,operand (var ,coordinate))) ())
    (polynomial-rule 'differentiate-sum "d(f+g)/dx = d(f)/dx+d(g)/dx"
      (facts coordinate left right) (+ ,left ,right)
      (+ (differentiate ,left (var ,coordinate)) (differentiate ,right (var ,coordinate))) ())
    (polynomial-rule 'differentiate-difference "d(f-g)/dx = d(f)/dx-d(g)/dx"
      (facts coordinate left right) (- ,left ,right)
      (- (differentiate ,left (var ,coordinate)) (differentiate ,right (var ,coordinate))) ())
    (polynomial-rule 'differentiate-product "d(f*g)/dx = d(f)/dx*g+f*d(g)/dx"
      (facts coordinate left right) (* ,left ,right)
      (+ (* (differentiate ,left (var ,coordinate)) ,right)
         (* ,left (differentiate ,right (var ,coordinate)))) ()
      #:label "Product rule")
    (polynomial-rule 'differentiate-power "d(f^n)/dx = n*f^(n-1)*d(f)/dx"
      (facts coordinate base exponent result) (^ ,base (num ,exponent)) ,result
      ((project (exponent base coordinate)
         (let ((n (natural-value exponent)))
           (if (= n 0) (== result '(num "0"))
               (== result `(* (* (num ,exponent) (^ ,base (num ,(number->string (- n 1)))))
                              (differentiate ,base (var ,coordinate))))))))
      #:label "Power rule")))

(define math-differentiation-rules
  (map math-rewrite-rule-name math-differentiation-rule-definitions))

(define (differentiation-stepo assumptions input output name)
  (math-rules-stepo math-differentiation-rule-definitions assumptions input output name 'check))
