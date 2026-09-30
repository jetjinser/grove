;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math term)
  #:use-module (grove math syntax)
  #:use-module (srfi srfi-1)
  #:export (math-term?
            math-expression->term))

(define (math-term? term)
  (cond
    ((and (list? term) (= (length term) 2) (eq? (car term) 'var))
     (symbol? (cadr term)))
    ((and (list? term) (= (length term) 2) (eq? (car term) 'num))
     (string? (cadr term)))
    ((and (pair? term) (memq (car term) '(var num))) #f)
    ((not (list? term)) #f)
    ((not (pair? term)) #f)
    ((memq (car term) '(+ - * / ^ =))
     (and (= (length term) (if (and (eq? (car term) '-)
                                    (= (length term) 2))
                               2 3))
          (every math-term? (cdr term))))
    ((symbol? (car term)) (every math-term? (cdr term)))
    (else #f)))

(define (math-expression->term expression)
  (unless (math-expression? expression)
    (error "expected a parsed math expression" expression))
  (case (math-expression-kind expression)
    ((identifier) `(var ,(math-expression-value expression)))
    ((number) `(num ,(math-expression-value expression)))
    ((group) (math-expression->term (car (math-expression-children expression))))
    ((call)
     (cons (math-expression-value expression)
           (map math-expression->term (math-expression-children expression))))
    ((negate)
     `(- ,(math-expression->term (car (math-expression-children expression)))))
    ((binary)
     (let ((operator (case (math-expression-value expression)
                       ((plus) '+) ((minus) '-) ((multiply) '*)
                       ((divide) '/) ((power) '^) ((subscript) 'sub)
                       ((equals) '=))))
       (cons operator (map math-expression->term
                           (math-expression-children expression)))))
    (else (error "unsupported math syntax node"
                 (math-expression-kind expression)))))
