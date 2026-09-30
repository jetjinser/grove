;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:use-module (grove math types)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (curl-curlo
            rewriteo
            rewrite-stepo
            math-rewrite-rule?
            math-rewrite-rule-name
            math-rewrite-rule-equation
            math-rewrite-rule-condition
            math-rewrite-rule-condition-checked?
            math-rewrite-rules
            math-rewrite-rule-ref
            rewrite-curl-curl))

(define-record-type <math-rewrite-rule>
  (make-math-rewrite-rule name equation condition condition-checked?)
  math-rewrite-rule?
  (name math-rewrite-rule-name)
  (equation math-rewrite-rule-equation)
  (condition math-rewrite-rule-condition)
  (condition-checked? math-rewrite-rule-condition-checked?))

;; This registry is the public contract for the relations below. A false
;; condition-checked? means callers must supply the condition themselves.
(define math-rewrite-rules
  (list
    (make-math-rewrite-rule
      'curl-curl-identity
      "curl(curl(E)) = grad(div(E)) - laplacian(E)"
      "assumptions declare vector(E) and smooth(E)"
      #t)
    (make-math-rewrite-rule
      'divergence-free
      "grad(div(E)) = 0"
      "assumptions declare vector(E) and contain the matching fact div(E) = 0"
      #t)
    (make-math-rewrite-rule
      'subtract-zero
      "0 - X = -X"
      "none"
      #t)))

(define (math-rewrite-rule-ref name)
  (find (lambda (rule) (eq? (math-rewrite-rule-name rule) name))
        math-rewrite-rules))

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
    (math-typeo assumptions field 'vector)
    (assumptiono assumptions `(= (div ,field) (num "0")))
    (== output '(num "0"))))

;; Vector-calculus identity, gated by explicit type and regularity facts.
(define (curl-curlo assumptions input output)
  (fresh (field)
    (== input `(curl (curl ,field)))
    (math-typeo assumptions field 'vector)
    (assumptiono assumptions `(smooth ,field))
    (== output `(- (grad (div ,field)) (laplacian ,field)))))

(define (subtract-zeroo input output)
  (fresh (term)
    (== input `(- (num "0") ,term))
    (== output `(- ,term))))

;; Apply one named rule at INPUT, returning the rule that fired.
(define (rewrite-rooto assumptions input output rule)
  (conde
    ((curl-curlo assumptions input output)
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

(define* (rewrite-curl-curl term #:key (assumptions '()))
  (let ((answers
          (run 1 (result)
            (curl-curlo assumptions term result))))
    (and (pair? answers) (car answers))))
