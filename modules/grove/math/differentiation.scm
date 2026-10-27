;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math differentiation)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math types)
  #:use-module (grove math private number)
  #:use-module (grove math term)
  #:use-module (grove math diagnostic)
  #:use-module (grove math private declaration)
  #:use-module (grove math private function-derivative)
  #:use-module (grove math premise)
  #:use-module (grove math rule)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (polynomial-term? differentiation-input-diagnostics differentiation-stepo math-differentiation-rules
            math-differentiation-rule-definitions make-function-chain-rule))

(define natural-value math-natural-value)

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

(define* (differentiable-term? term #:optional (facts '()))
  (define (supported? child) (differentiable-term? child facts))
  (or (polynomial-term? term)
      (match term
        (((or 'sin 'cos 'exp 'log) operand) (supported? operand))
        (('- operand) (supported? operand))
        (((or '+ '- '* '/) left right)
         (and (supported? left) (supported? right)))
        (((or 'darg 'prime) application index)
         (let ((info (math-function-derivative-info facts term)))
           (and info (every supported? (cadr info)))))
        (('differentiate application ('var name)) (supported? application))
        (('^ base ('num exponent))
         (and (natural-value exponent) (supported? base)))
        ((name operand)
         (and (equal? (math-function-signature facts name) '((scalar) scalar))
              (supported? operand)))
        ((name . operands)
         (let ((signature (math-function-signature facts name)))
           (and signature (eq? (cadr signature) 'scalar)
                (equal? (car signature) (make-list (length operands) 'scalar))
                (every supported? operands))))
        (_ #f))))

(define* (differentiation-input-diagnostics expression coordinate #:optional (facts '()))
  (cond
    ((not (eq? (car coordinate) 'var))
     (list (make-math-diagnostic #f #f '(1) 'unsupported-expression '()
             "differentiation requires a coordinate variable")))
    ((eq? (math-variable-role facts coordinate) 'parameter)
     (list (make-math-diagnostic #f #f '(1) 'unsupported-expression '()
             "a constant parameter cannot be used as a differentiation coordinate")))
    ((differentiable-term? expression facts)
     (filter-map
       (lambda (premise)
         (and (not (math-premise-known? facts premise))
              (make-math-diagnostic #f #f '(0) 'missing-premise (list premise)
                "function differentiation requires sufficient regularity")))
       (delete-duplicates (math-function-differentiation-premises expression) equal?)))
    (else
     (let locate ((term expression) (path '(0)))
       (let ((child (and (memq (car term) '(+ - * / ^ sin cos exp log))
                         (find (lambda (index)
                                 (not (differentiable-term? (list-ref (cdr term) index) facts)))
                               (iota (length (cdr term)))))))
         (if child
             (locate (list-ref (cdr term) child) (append path (list child)))
             (list (make-math-diagnostic #f #f path 'unsupported-expression '()
                     (format #f "scalar differentiation does not support ~a; expected arithmetic, elementary functions, and natural-number powers"
                             (math-term->source term))))))))))

;; This family has a constructive forward/check mode. Independent coordinates
;; and real-domain premises constrain it; inverse integration is absent.
(define-syntax scalar-derivative-rule
  (syntax-rules ()
    ((_ name equation (facts coordinate item ...) expression result (goal ...) option ...)
     (math-rule name equation "real scalar expression in independent scalar coordinates" 'differentiate
       (facts item ... coordinate)
       (differentiate expression (var ,coordinate)) result ()
       ((project (facts item ... coordinate)
          (if (differentiable-term? (quasiquote expression) facts) succeed fail))
        (math-typeo facts (quasiquote expression) 'scalar)
        (math-typeo facts `(var ,coordinate) 'scalar)
        (project (facts coordinate)
          (if (eq? (math-variable-role facts `(var ,coordinate)) 'parameter) fail succeed))
        goal ...) option ...))))

(define (make-function-chain-rule operator)
  (define (required input)
    (let ((info (and (eq? (car input) operator) (math-function-derivative-shape (cadr input)))))
      (and info
           (append (list (math-function-derivative-premise info 1))
                   (if (eq? operator 'partial)
                       `((coordinate ,(caddr input)) (differentiable ,(cadr input))) '())))))
  (make-math-rewrite-rule
    (if (eq? operator 'partial) 'partial-function-chain 'differentiate-function)
    "Derivative of f(u_1,...,u_n) = sum_i partial_i f(u_1,...,u_n) derivative of u_i"
    "Declared scalar function and sufficient regularity for the next derivative" operator
    (lambda (facts input output)
      (fresh (application coordinate)
        (== input `(,operator ,application ,coordinate))
        (project (facts input application coordinate)
          (let ((info (math-function-derivative-info facts application)))
            (if info
                (let* ((base (cons (car info) (cadr info)))
                       (derivative (fold (lambda (index term) `(darg ,term (num ,(number->string index))))
                                         base (caddr info)))
                       (terms (map (lambda (arg index)
                                     `(* (darg ,derivative (num ,(number->string index)))
                                         (,operator ,arg ,coordinate)))
                                   (cadr info) (iota (length (cadr info)) 1))))
                  (fresh ()
                    (math-typeo facts input 'scalar)
                    (math-factso facts (required input))
                    (== output (if (null? terms) '(num "0")
                                   (fold (lambda (term sum) `(+ ,sum ,term)) (car terms) (cdr terms)))))) fail)))))
    #:matcher required #:label "Function chain rule"
    #:documentation "Apply the shared function chain rule using one-based argument positions. C1 permits one derivative, C2 permits two, and smooth permits higher derivatives."))

(define math-differentiation-rule-definitions
  (list
    (scalar-derivative-rule 'differentiate-constant "d(c)/dx = 0"
      (facts coordinate expression) ,expression (num "0")
      ((project (expression) (if (math-number-value expression) succeed fail)))
      #:matcher (lambda (input)
                  (match input
                    (('differentiate expression ('var name))
                     (and (math-number-value expression) '())) (_ #f))))
    (scalar-derivative-rule 'differentiate-variable "d(x)/dx = 1"
      (facts coordinate) (var ,coordinate) (num "1") ())
    (scalar-derivative-rule 'differentiate-independent "d(y)/dx = 0 for independent y"
      (facts coordinate other) (var ,other) (num "0") ((=/= other coordinate))
      #:matcher (lambda (input)
                  (match input
                    (('differentiate ('var other) ('var coordinate))
                     (and (not (eq? other coordinate)) '())) (_ #f))))
    (scalar-derivative-rule 'differentiate-negation "d(-f)/dx = -d(f)/dx"
      (facts coordinate operand) (- ,operand) (- (differentiate ,operand (var ,coordinate))) ())
    (scalar-derivative-rule 'differentiate-sum "d(f+g)/dx = d(f)/dx+d(g)/dx"
      (facts coordinate left right) (+ ,left ,right)
      (+ (differentiate ,left (var ,coordinate)) (differentiate ,right (var ,coordinate))) ())
    (scalar-derivative-rule 'differentiate-difference "d(f-g)/dx = d(f)/dx-d(g)/dx"
      (facts coordinate left right) (- ,left ,right)
      (- (differentiate ,left (var ,coordinate)) (differentiate ,right (var ,coordinate))) ())
    (scalar-derivative-rule 'differentiate-product "d(f*g)/dx = d(f)/dx*g+f*d(g)/dx"
      (facts coordinate left right) (* ,left ,right)
      (+ (* (differentiate ,left (var ,coordinate)) ,right)
         (* ,left (differentiate ,right (var ,coordinate)))) ()
      #:label "Product rule"
      #:documentation "Generate the derivative of each factor separately and sum the two products. This rule constructs derivative terms; subsequent differentiation and simplification steps compute and reduce them.")
    (scalar-derivative-rule 'differentiate-power "d(f^n)/dx = n*f^(n-1)*d(f)/dx"
      (facts coordinate base exponent result) (^ ,base (num ,exponent)) ,result
      ((project (exponent base coordinate)
         (let ((n (natural-value exponent)))
           (if (= n 0) (== result '(num "0"))
               (== result `(* (* (num ,exponent) (^ ,base (num ,(number->string (- n 1)))))
                              (differentiate ,base (var ,coordinate))))))))
      #:label "Power rule")
    (scalar-derivative-rule 'differentiate-quotient "d(u/v)/dx = (u'v-uv')/v^2"
      (facts coordinate left right) (/ ,left ,right)
      (/ (- (* (differentiate ,left (var ,coordinate)) ,right)
            (* ,left (differentiate ,right (var ,coordinate))))
         (^ ,right (num "2")))
      ((math-factso facts `((nonzero ,right)))) #:label "Quotient rule"
      #:documentation "Apply the quotient rule on the domain where the denominator is nonzero. The derivative remains a symbolic term until later passes process its operands and simplify the result."
      #:matcher (lambda (input)
                  (match input
                    (('differentiate ('/ left right) ('var coordinate)) `((nonzero ,right)))
                    (_ #f))))
    (scalar-derivative-rule 'differentiate-sin "d(sin(u))/dx = u'*cos(u)"
      (facts coordinate operand) (sin ,operand)
      (* (differentiate ,operand (var ,coordinate)) (cos ,operand)) () #:label "Sine chain rule")
    (scalar-derivative-rule 'differentiate-cos "d(cos(u))/dx = -u'*sin(u)"
      (facts coordinate operand) (cos ,operand)
      (- (* (differentiate ,operand (var ,coordinate)) (sin ,operand))) () #:label "Cosine chain rule")
    (scalar-derivative-rule 'differentiate-exp "d(exp(u))/dx = u'*exp(u)"
      (facts coordinate operand) (exp ,operand)
      (* (differentiate ,operand (var ,coordinate)) (exp ,operand)) () #:label "Exponential chain rule")
    (scalar-derivative-rule 'differentiate-log "d(log(u))/dx = u'/u"
      (facts coordinate operand) (log ,operand)
      (/ (differentiate ,operand (var ,coordinate)) ,operand)
      ((math-factso facts `((positive ,operand)))) #:label "Logarithm chain rule"
      #:matcher (lambda (input)
                  (match input
                    (('differentiate ('log operand) ('var coordinate)) `((positive ,operand)))
                    (_ #f))))
    (make-function-chain-rule 'differentiate)))

(define math-differentiation-rules
  (map math-rewrite-rule-name math-differentiation-rule-definitions))

(define (differentiation-stepo assumptions input output name)
  (math-rules-stepo math-differentiation-rule-definitions assumptions input output name 'check))
