;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:export (curl-curlo
            rewriteo
            rewrite-stepo
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

;; Apply one named rule at INPUT, returning the rule that fired.
(define (rewrite-rooto assumptions input output rule)
  (conde
    ((curl-curlo input output)
     (== rule 'curl-curl-identity))
    ((divergence-zeroo assumptions input output)
     (== rule 'divergence-free))
    ((subtract-zeroo input output)
     (== rule 'subtract-zero))))

;; Paths count operands only; the root is (), the first operand is (0), etc.
(define (rewrite-stepo assumptions input output rule path)
  (conde
    ((rewrite-rooto assumptions input output rule)
     (== path '()))
    ((fresh (operator arguments rewritten child-path)
       (== `(,operator . ,arguments) input)
       (rewrite-childreno assumptions arguments 0 rewritten rule child-path)
       (== `(,operator . ,rewritten) output)
       (== path child-path)))))

(define (rewrite-childreno assumptions inputs index outputs rule path)
  (fresh (child rest rewritten child-path)
    (== `(,child . ,rest) inputs)
    (conde
      ((rewrite-stepo assumptions child rewritten rule child-path)
       (== `(,rewritten . ,rest) outputs)
       (== path `(,index . ,child-path)))
      ((fresh (rewritten-rest rest-path)
         (rewrite-childreno assumptions rest (+ index 1)
                            rewritten-rest rule rest-path)
         (== `(,child . ,rewritten-rest) outputs)
         (== path `(,index . ,rest-path)))))))

;; Compatibility relation for callers that need only the resulting term.
(define (rewriteo assumptions input output)
  (fresh (rule path)
    (rewrite-stepo assumptions input output rule path)))

(define (rewrite-curl-curl term)
  (let ((answers (run 1 (result) (curl-curlo term result))))
    (and (pair? answers) (car answers))))
