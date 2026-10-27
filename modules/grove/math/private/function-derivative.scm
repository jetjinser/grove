;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (grove math private function-derivative)
  #:use-module (grove math private declaration)
  #:use-module (grove math private number)
  #:use-module (grove math private operator)
  #:use-module (srfi srfi-1)
  #:export (math-function-derivative-shape math-function-derivative-info
            math-function-derivative-premise math-function-differentiation-premises))

(define (math-function-derivative-shape term)
  "Return (function arguments argument-indices), or #f for an invalid application.
Indices are one-based argument positions, independent of coordinate names."
  (if (eq? (car term) 'darg)
      (let ((info (math-function-derivative-shape (cadr term)))
            (index (and (eq? (caaddr term) 'num) (math-natural-value (cadr (caddr term))))))
        (and info index (> index 0) (<= index (length (cadr info)))
             (list (car info) (cadr info) (append (caddr info) (list index)))))
      (if (eq? (car term) 'prime)
          (and (eq? (caadr term) 'var) (list (cadadr term) (list (caddr term)) '(1)))
          (and (not (memq (car term) '(var num zero)))
               (not (math-operator-arities (car term)))
               (list (car term) (cdr term) '())))))

(define (math-function-derivative-info facts term)
  (let* ((info (math-function-derivative-shape term))
         (signature (and info (math-function-signature facts (car info)))))
    (and signature (eq? (cadr signature) 'scalar)
         (equal? (car signature) (make-list (length (cadr info)) 'scalar)) info)))

(define* (math-function-derivative-premise info #:optional (additional-orders 0))
  `(,(case (+ additional-orders (length (caddr info))) ((0 1) 'differentiable) ((2) 'c2) (else 'smooth))
    (var ,(car info))))

(define* (math-function-differentiation-premises term #:optional (order 1))
  "Regularity required to differentiate TERM, including pending derivatives."
  (cond ((memq (car term) '(var num zero)) '())
        ((memq (car term) '(differentiate partial))
         (math-function-differentiation-premises (cadr term) (+ order 1)))
        (else
         (let ((info (math-function-derivative-shape term)))
           (append (if info (list (math-function-derivative-premise info order)) '())
                   (append-map (lambda (child) (math-function-differentiation-premises child order))
                               (if info (cadr info) (cdr term))))))))
