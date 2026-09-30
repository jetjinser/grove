;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:export (curl-curlo
            rewriteo
            rewrite-curl-curl))

;; Vector-calculus identity: curl(curl(E)) = grad(div(E)) - laplacian(E).
(define (curl-curlo input output)
  (fresh (field)
    (== input `(curl (curl ,field)))
    (== output `(- (grad (div ,field)) (laplacian ,field)))))

(define (assumptiono assumptions assumption)
  (conde
    ((fresh (rest)
       (== `(,assumption . ,rest) assumptions)))
    ((fresh (head rest)
       (== `(,head . ,rest) assumptions)
       (=/= head assumption)
       (assumptiono rest assumption)))))

(define (divergence-zeroo assumptions input output)
  (fresh (field)
    (== input `(grad (div ,field)))
    (assumptiono assumptions `(= (div ,field) (num "0")))
    (== output '(num "0"))))

(define (subtract-zeroo input output)
  (fresh (term)
    (== input `(- (num "0") ,term))
    (== output `(- ,term))))

(define (rewrite-arguments-o assumptions input output)
  (conde
    ((fresh (child rest rewritten)
       (== `(,child . ,rest) input)
       (rewriteo assumptions child rewritten)
       (== `(,rewritten . ,rest) output)))
    ((fresh (head rest rewritten)
       (== `(,head . ,rest) input)
       (rewrite-arguments-o assumptions rest rewritten)
       (== `(,head . ,rewritten) output)))))

;; One relational rewrite at the root or at one descendant of INPUT.
(define (rewriteo assumptions input output)
  (conde
    ((curl-curlo input output))
    ((divergence-zeroo assumptions input output))
    ((subtract-zeroo input output))
    ((fresh (operator arguments rewritten)
       (== `(,operator . ,arguments) input)
       (rewrite-arguments-o assumptions arguments rewritten)
       (== `(,operator . ,rewritten) output)))))

(define (rewrite-curl-curl term)
  (let ((answers (run 1 (result) (curl-curlo term result))))
    (and (pair? answers) (car answers))))
