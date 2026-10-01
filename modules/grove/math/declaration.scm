;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math declaration)
  #:use-module (grove math operator)
  #:use-module (srfi srfi-1)
  #:export (math-function-signature math-function-declaration?
            math-scalar-declared? math-variable-role))

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
    (and fact (list (map cadr (drop-right (cddr fact) 1)) (cadr (last fact))))))

(define (math-scalar-declared? facts term)
  (any (lambda (role) (member (list role term) facts)) '(scalar coordinate parameter)))

(define (math-variable-role facts term)
  (cond ((member `(parameter ,term) facts) 'parameter)
        ((math-scalar-declared? facts term) 'coordinate)
        (else #f)))
