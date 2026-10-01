;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math declaration)
  #:use-module (grove math operator)
  #:use-module (srfi srfi-1)
  #:export (math-function-signature math-function-declaration?
            math-scalar-declared? math-variable-role math-field-declaration? math-space-declaration?
            math-field-coordinates))

(define (variable? term)
  (and (list? term) (= (length term) 2) (eq? (car term) 'var) (symbol? (cadr term))))

(define (coordinates? terms)
  (and (= (length terms) 3) (every variable? terms)
       (= (length (delete-duplicates terms equal?)) 3)))

(define (math-space-declaration? fact)
  (and (list? fact) (= (length fact) 4) (eq? (car fact) 'space) (coordinates? (cdr fact))))

(define (math-field-declaration? fact)
  (and (list? fact) (= (length fact) 6) (eq? (car fact) 'field)
       (variable? (cadr fact)) (not (math-operator-arities (cadadr fact)))
       (not (memq (cadadr fact) '(var num zero function)))
       (member (caddr fact) '((var scalar) (var vector)))
       (coordinates? (cdddr fact)) #t))

(define (math-field-coordinates facts name)
  (let ((fact (find (lambda (fact) (and (math-field-declaration? fact) (eq? (cadadr fact) name))) facts)))
    (and fact (cdddr fact))))

(define (math-function-declaration? fact)
  (and (list? fact) (>= (length fact) 3) (eq? (car fact) 'function)
       (list? (cadr fact)) (= (length (cadr fact)) 2) (eq? (caadr fact) 'var)
       (symbol? (cadadr fact))
       (not (math-operator-arities (cadadr fact)))
       (not (memq (cadadr fact) '(var num zero function)))
       (every (lambda (type) (member type '((var scalar) (var vector)))) (cddr fact)) #t))

;; Signature is (input-types result-type), including the zero-argument case.
(define (math-function-signature facts name)
  (let ((fact (find (lambda (fact)
                      (and (math-function-declaration? fact) (eq? (cadadr fact) name))) facts)))
    (if fact (list (map cadr (drop-right (cddr fact) 1)) (cadr (last fact)))
        (let ((field (find (lambda (fact) (and (math-field-declaration? fact) (eq? (cadadr fact) name))) facts)))
          (and field (list '(scalar scalar scalar) (cadr (caddr field))))))))

(define (math-scalar-declared? facts term)
  (any (lambda (role) (member (list role term) facts)) '(scalar coordinate parameter)))

(define (math-variable-role facts term)
  (cond ((member `(parameter ,term) facts) 'parameter)
        ((math-scalar-declared? facts term) 'coordinate)
        (else #f)))
