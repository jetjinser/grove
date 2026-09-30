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
            unrewrite-rooto
            unrewrite-stepo
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
      'div-curl-zero
      "div(curl(E)) = 0"
      "assumptions declare vector(E) and smooth(E)"
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
    (assumptiono assumptions `(= (div ,field) (num "0")))
    (math-typeo assumptions field 'vector)
    (== output '(num "0"))))

;; The divergence of a curl vanishes for a sufficiently regular vector field.
(define (div-curl-zeroo assumptions input output)
  (fresh (field)
    (== input `(div (curl ,field)))
    ;; Ground the inverse query from the user's declared vector fields;
    ;; otherwise zero would generate an unbounded family of arbitrary terms.
    (assumptiono assumptions `(vector ,field))
    (assumptiono assumptions `(smooth ,field))
    (math-typeo assumptions field 'vector)
    (== output '(num "0"))))

;; Vector-calculus identity, gated by explicit type and regularity facts.
(define (curl-curlo assumptions input output)
  (fresh (field)
    (== input `(curl (curl ,field)))
    (== output `(- (grad (div ,field)) (laplacian ,field)))
    (math-typeo assumptions field 'vector)
    (assumptiono assumptions `(smooth ,field))))

(define (subtract-zeroo input output)
  (fresh (term)
    (== input `(- (num "0") ,term))
    (== output `(- ,term))))

(define (unrewrite-rooto assumptions input output rule)
  (conde
    ((curl-curlo assumptions input output)
     (== rule 'curl-curl-identity))
    ((fresh (field)
       (== input `(grad (div ,field)))
       (== output '(num "0"))
       (assumptiono assumptions `(= (div ,field) (num "0")))
       (math-typeo assumptions field 'vector)
       (== rule 'divergence-free)))
    ((div-curl-zeroo assumptions input output)
     (== rule 'div-curl-zero))
    ((fresh (term)
       (== input `(- (num "0") ,term))
       (== output `(- ,term))
       (== rule 'subtract-zero)))))

;; Apply one named rule at INPUT, returning the rule that fired.
(define (rewrite-rooto assumptions input output rule)
  (conde
    ((curl-curlo assumptions input output)
     (== rule 'curl-curl-identity))
    ((divergence-zeroo assumptions input output)
     (== rule 'divergence-free))
    ((div-curl-zeroo assumptions input output)
     (== rule 'div-curl-zero))
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

(define (unrewrite-stepo assumptions input output rule path)
  (conde
    ((unrewrite-rooto assumptions input output rule)
     (== path '()))
    ((fresh (operator arguments restored child-path)
       (== `(,operator . ,arguments) output)
       (unrewrite-childreno assumptions arguments 0 restored rule child-path)
       (== `(,operator . ,restored) input)
       (== path child-path)))))

(define (unrewrite-childreno assumptions outputs index inputs rule path)
  (fresh (child rest restored child-path)
    (== `(,child . ,rest) outputs)
    (conde
      ((unrewrite-stepo assumptions restored child rule child-path)
       (== `(,restored . ,rest) inputs)
       (== path `(,index . ,child-path)))
      ((fresh (restored-rest rest-path)
         (unrewrite-childreno assumptions rest (+ index 1)
                              restored-rest rule rest-path)
         (== `(,child . ,restored-rest) inputs)
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
