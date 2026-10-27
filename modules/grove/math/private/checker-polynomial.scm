;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private checker-polynomial)
  #:use-module (grove math private number)
  #:use-module (grove math term)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (math-polynomial-normal-form
            math-polynomial-equivalent?
            math-rational-normal-form
            canonical-polynomial
            constant-polynomial
            polynomial-add
            polynomial-scale
            monomial-multiply
            polynomial-multiply
            polynomial-power
            polynomial-constant-value
            parse-polynomial
            parse-rational))

;; Keep this checker model independent from the calculation engine's polynomial IR.
(define max-polynomial-monomials 256)
(define max-polynomial-input-terms (* max-polynomial-monomials max-polynomial-monomials))
(define max-polynomial-degree 64)

(define (monomial<? left right)
  (string<? (format #f "~s" left) (format #f "~s" right)))

(define (canonical-polynomial terms)
  (when (> (length terms) max-polynomial-input-terms)
    (throw 'semantic-polynomial-limit))
  (let* ((sorted (sort terms
                  (lambda (left right)
                    (monomial<? (car left) (car right)))))
         (combined
           (let combine ((remaining sorted) (monomial #f) (coefficient 0) (result '()))
             (cond
               ((null? remaining)
                (if (and monomial (not (zero? coefficient)))
                    (cons (cons monomial coefficient) result)
                    result))
               ((and monomial (equal? monomial (caar remaining)))
                (combine (cdr remaining) monomial
                         (+ coefficient (cdar remaining)) result))
               (else
                (combine (cdr remaining) (caar remaining) (cdar remaining)
                  (if (and monomial (not (zero? coefficient)))
                      (cons (cons monomial coefficient) result)
                      result))))))
         (normal (reverse combined)))
    (when (or (> (length normal) max-polynomial-monomials)
              (any (lambda (term)
                     (> (apply + (map cdr (car term))) max-polynomial-degree))
                   normal))
      (throw 'semantic-polynomial-limit))
    normal))

(define (constant-polynomial value)
  (if (zero? value) '() (list (cons '() value))))

(define (polynomial-add left right)
  (canonical-polynomial (append left right)))

(define (polynomial-scale polynomial value)
  (canonical-polynomial
    (map (lambda (term) (cons (car term) (* value (cdr term)))) polynomial)))

(define (monomial-multiply left right)
  (sort
    (fold (lambda (factor result)
            (let ((old (assq (car factor) result)))
              (if old
                  (map (lambda (entry)
                         (if (eq? entry old)
                             (cons (car factor) (+ (cdr entry) (cdr factor)))
                             entry))
                       result)
                  (cons factor result))))
          '() (append left right))
    (lambda (left right) (string<? (symbol->string (car left))
                                   (symbol->string (car right))))))

(define (polynomial-multiply left right)
  (when (> (* (length left) (length right)) max-polynomial-input-terms)
    (throw 'semantic-polynomial-limit))
  (canonical-polynomial
    (append-map
      (lambda (left-term)
        (map (lambda (right-term)
               (cons (monomial-multiply (car left-term) (car right-term))
                     (* (cdr left-term) (cdr right-term))))
             right))
      left)))

(define (polynomial-power polynomial exponent)
  (let loop ((n exponent) (result (constant-polynomial 1)))
    (if (zero? n) result
        (loop (- n 1) (polynomial-multiply result polynomial)))))

(define (polynomial-constant-value polynomial)
  (and (or (null? polynomial)
           (and (= (length polynomial) 1) (null? (caar polynomial))))
       (if (null? polynomial) 0 (cdar polynomial))))

(define (parse-polynomial term)
  (match term
    (('var name)
     (list (cons (list (cons name 1)) 1)))
    (('num text)
     (let ((number (math-natural-value text)))
       (and number (constant-polynomial number))))
    (('- operand)
     (let ((operand (parse-polynomial operand)))
       (and operand (polynomial-scale operand -1))))
    (('+ left right)
     (let ((left (parse-polynomial left)) (right (parse-polynomial right)))
       (and left right (polynomial-add left right))))
    (('- left right)
     (let ((left (parse-polynomial left)) (right (parse-polynomial right)))
       (and left right
            (polynomial-add left (polynomial-scale right -1)))))
    (('* left right)
     (let ((left (parse-polynomial left)) (right (parse-polynomial right)))
       (and left right (polynomial-multiply left right))))
    (('/ numerator denominator)
     (let* ((numerator (parse-polynomial numerator))
            (denominator (parse-polynomial denominator))
            (value (and denominator (polynomial-constant-value denominator))))
       (and numerator value (not (zero? value))
            (polynomial-scale numerator (/ 1 value)))))
    (('^ base ('num exponent-text))
     (let ((base (parse-polynomial base))
           (exponent (math-natural-value exponent-text)))
       (and base exponent (<= exponent 128)
            (polynomial-power base exponent))))
    (_ #f)))

(define (math-polynomial-normal-form term)
  "Return an exact sparse-polynomial normal form, or #f outside its domain.

The accepted language is scalar polynomials over the rationals: variables,
integer and rational constants, addition, subtraction, multiplication, and
natural powers. Division is accepted only by a nonzero rational constant."
  (catch 'semantic-polynomial-limit
    (lambda () (and (math-term? term) (parse-polynomial term)))
    (lambda _ #f)))

(define (math-polynomial-equivalent? left right)
  (let ((left (math-polynomial-normal-form left))
        (right (math-polynomial-normal-form right)))
    (and left right (equal? left right))))

;; Rational expressions keep numerator, denominator and their explicit domain
;; obligations separate. Equality is checked by cross multiplication only
;; after every symbolic denominator is justified by a declared nonzero fact.
(define (fraction numerator denominator conditions)
  (list numerator denominator (delete-duplicates conditions equal?)))

(define (fraction-add left right sign)
  (fraction
    (polynomial-add
      (polynomial-multiply (car left) (cadr right))
      (polynomial-scale
        (polynomial-multiply (car right) (cadr left)) sign))
    (polynomial-multiply (cadr left) (cadr right))
    (append (caddr left) (caddr right))))

(define (parse-rational term)
  (let ((polynomial (parse-polynomial term)))
    (cond
      (polynomial (fraction polynomial (constant-polynomial 1) '()))
      (else
       (match term
         (('- operand)
          (let ((operand (parse-rational operand)))
            (and operand (fraction (polynomial-scale (car operand) -1)
                                   (cadr operand) (caddr operand)))))
         (('+ left right)
          (let ((left (parse-rational left)) (right (parse-rational right)))
            (and left right (fraction-add left right 1))))
         (('- left right)
          (let ((left (parse-rational left)) (right (parse-rational right)))
            (and left right (fraction-add left right -1))))
         (('* left right)
          (let ((left (parse-rational left)) (right (parse-rational right)))
            (and left right
                 (fraction (polynomial-multiply (car left) (car right))
                   (polynomial-multiply (cadr left) (cadr right))
                   (append (caddr left) (caddr right))))))
         (('/ numerator denominator-term)
          (let ((numerator (parse-rational numerator))
                (denominator (parse-rational denominator-term)))
            (and numerator denominator (not (null? (car denominator)))
                 (let* ((constant-polynomial
                          (math-polynomial-normal-form denominator-term))
                        (constant (and constant-polynomial
                                       (polynomial-constant-value constant-polynomial)))
                        (denominator-conditions
                          (if (and constant (not (zero? constant))) '()
                              (list denominator-term))))
                   (fraction (polynomial-multiply (car numerator) (cadr denominator))
                     (polynomial-multiply (cadr numerator) (car denominator))
                     (append (caddr numerator) (caddr denominator)
                             denominator-conditions))))))
         (('^ base ('num exponent-text))
          (let ((base (parse-rational base))
                (exponent (math-natural-value exponent-text)))
            (and base exponent (<= exponent 128)
                 (fraction (polynomial-power (car base) exponent)
                   (polynomial-power (cadr base) exponent) (caddr base)))))
         (_ #f))))))

(define (math-rational-normal-form term)
  (catch 'semantic-polynomial-limit
    (lambda () (and (math-term? term) (parse-rational term)))
    (lambda _ #f)))

