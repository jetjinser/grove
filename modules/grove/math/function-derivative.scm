;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (grove math function-derivative)
  #:use-module (grove math declaration)
  #:use-module (grove math number)
  #:use-module (srfi srfi-1)
  #:export (math-function-derivative-info math-function-derivative-premise))

(define (math-function-derivative-info facts term)
  "Return (function arguments argument-indices), or #f for an invalid application.
Indices are one-based argument positions, independent of coordinate names."
  (if (eq? (car term) 'darg)
      (let ((info (math-function-derivative-info facts (cadr term)))
            (index (and (eq? (caaddr term) 'num) (math-natural-value (cadr (caddr term))))))
        (and info index (> index 0) (<= index (length (cadr info)))
             (list (car info) (cadr info) (append (caddr info) (list index)))))
      (let ((signature (math-function-signature facts (car term))))
        (and signature (eq? (cadr signature) 'scalar)
             (equal? (car signature) (make-list (length (cdr term)) 'scalar))
             (list (car term) (cdr term) '())))))

(define (math-function-derivative-premise info)
  `(,(case (length (caddr info)) ((0 1) 'differentiable) ((2) 'c2) (else 'smooth))
    (var ,(car info))))
