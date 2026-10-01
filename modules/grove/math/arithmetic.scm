;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math arithmetic)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math types)
  #:use-module (grove math rule)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-number-value math-number->term constant-rewrite
             collect-rewrite coefficient-rewrite math-arithmetic-rules))

;; Numbers retain the source IR: unsigned integers, unary signs and fractions.
;; Guile's exact rationals do the computation; no floating point is introduced.
(define (math-number-value term)
  (match term
    (('num value)
     (and (string? value) (not (string-null? value))
          (string-every (lambda (c) (char<=? #\0 c #\9)) value)
          (string->number value)))
    (('- operand)
     (let ((value (math-number-value operand))) (and value (- value))))
    (('/ numerator denominator)
     (let ((n (math-number-value numerator)) (d (math-number-value denominator)))
       (and n d (not (= d 0)) (/ n d))))
    (_ #f)))

(define (math-number->term value)
  (unless (and (rational? value) (exact? value))
    (error "expected an exact rational" value))
  (cond ((< value 0) `(- ,(math-number->term (- value))))
        ((integer? value) `(num ,(number->string value)))
        (else `(/ (num ,(number->string (numerator value)))
                  (num ,(number->string (denominator value)))))))

(define (constant-rewrite input)
  (define (binary operator left right)
    (let ((a (math-number-value left)) (b (math-number-value right)))
      (and a b
           (let ((value (case operator
                          ((+) (+ a b)) ((-) (- a b)) ((*) (* a b))
                          ((/) (and (not (= b 0)) (/ a b))))))
             (and value (math-number->term value))))))
  (let ((result
          (match input
            (('- operand)
             (let ((value (math-number-value operand)))
               (and value (math-number->term (- value)))))
            (((or '+ '- '* '/) left right) (binary (car input) left right))
            (_ #f))))
    (and result (not (equal? result input)) result)))

;; A bounded algebraic scope: combine structurally identical scalar terms in
;; sums/differences, retaining their first occurrence order. No distribution or
;; commutative multiplication is implicit in the equality of monomials.
(define (collect-rewrite input)
  (define (summands term sign)
    (match term
      (('+ a b) (append (summands a sign) (summands b sign)))
      (('- a b) (append (summands a sign) (summands b (- sign))))
      (('- a) (summands a (- sign)))
      (_ (let ((number (math-number-value term)))
           (if number (list (cons #f (* sign number)))
               (match term
                 (('* left right)
                  (let ((a (math-number-value left)) (b (math-number-value right)))
                    (list (cond (a (cons right (* sign a)))
                                (b (cons left (* sign b)))
                                (else (cons term sign))))))
                 (_ (list (cons term sign)))))))))
  (and (pair? input) (memq (car input) '(+ -))
       (let* ((terms (summands input 1))
              (keys (delete-duplicates (map car terms) equal?)))
         (and (any car terms) (< (length keys) (length terms))
              (let* ((result-terms
                       (filter-map
                         (lambda (key)
                           (let ((coefficient
                                   (fold (lambda (entry sum)
                                           (if (equal? (car entry) key)
                                               (+ sum (cdr entry)) sum)) 0 terms)))
                             (cond ((= coefficient 0) #f)
                                   ((not key) (math-number->term coefficient))
                                   ((= coefficient 1) key)
                                   ((= coefficient -1) `(- ,key))
                                   (else `(* ,(math-number->term coefficient) ,key)))))
                         keys))
                     (result (if (null? result-terms) '(num "0")
                                 (fold (lambda (term sum) `(+ ,sum ,term))
                                       (car result-terms) (cdr result-terms)))))
                (and (not (equal? input result)) result))))))

(define (coefficient-rewrite input)
  (define (factors term)
    (match term
      (('* a b) (append (factors a) (factors b)))
      (_ (list term))))
  (and (pair? input) (eq? (car input) '*)
       (let* ((items (factors input))
              (constants (filter-map math-number-value items))
              (symbolic (remove math-number-value items)))
         (and (>= (length constants) 2) (pair? symbolic)
              (let* ((coefficient (apply * constants))
                     (body (fold (lambda (term product) `(* ,product ,term))
                                 (car symbolic) (cdr symbolic)))
                     (result (cond ((= coefficient 0) '(num "0"))
                                   ((= coefficient 1) body)
                                   ((= coefficient -1) `(- ,body))
                                   (else `(* ,(math-number->term coefficient) ,body)))))
                (and (not (equal? input result)) result))))))

;; Computed rules derive their diagnostic matcher from the same transform
;; used by the relation, and explicitly support forward/check modes only.
(define (computed-rule name equation group transform)
  (make-math-rewrite-rule name equation "exact scalar arithmetic" group
    (lambda (assumptions input output)
      (project (input)
        (let ((result (transform input)))
          (if result
              (fresh () (== output result) (math-typeo assumptions input 'scalar)) fail))))
    #:matcher (lambda (input) (and (transform input) '()))))

(define math-arithmetic-rules
  (list (computed-rule 'evaluate-constant "exact constant arithmetic" 'calculate constant-rewrite)
        (computed-rule 'collect-like-terms "a*X + b*X = (a+b)*X" 'collect collect-rewrite)
        (computed-rule 'combine-coefficients "a*(b*X) = (a*b)*X" 'collect coefficient-rewrite)))
