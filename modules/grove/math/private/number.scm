;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private number)
  #:use-module (grove string-fun)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-13)
  #:export (math-natural-value math-number-value math-number->term))

(define (math-natural-value text)
  (and (non-empty-string? text)
       (string-every (lambda (c) (char<=? #\0 c #\9)) text)
       (string->number text)))

;; Numbers retain the source IR: unsigned integers, unary signs and fractions.
;; Guile's exact rationals do the computation; no floating point is introduced.
(define (math-number-value term)
  (match term
    (('num value)
     (math-natural-value value))
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
