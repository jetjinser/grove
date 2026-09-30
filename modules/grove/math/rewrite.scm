;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:use-module (grove math types)
  #:use-module (grove math differentiation)
  #:use-module (ice-9 match)
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
            math-simplification-rules
            math-backward-rules
            math-rewrite-rule-ref
            math-rule-required-facts
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
  (append (list
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
      "zero and X have the same scalar or vector type"
      #t)
    (make-math-rewrite-rule 'add-zero "X + 0 = X" "matching scalar or vector types" #t)
    (make-math-rewrite-rule 'zero-add "0 + X = X" "matching scalar or vector types" #t)
    (make-math-rewrite-rule 'subtract-right-zero "X - 0 = X" "matching scalar or vector types" #t)
    (make-math-rewrite-rule 'double-negation "-(-X) = X" "X is scalar or vector" #t)
    (make-math-rewrite-rule 'multiply-one "X * 1 = X" "X is scalar or vector" #t)
    (make-math-rewrite-rule 'one-multiply "1 * X = X" "X is scalar or vector" #t)
    (make-math-rewrite-rule 'curl-grad-zero "curl(grad(f)) = zero(vector)"
      "assumptions declare scalar(f) and smooth(f)" #t)
    (make-math-rewrite-rule 'div-grad-laplacian "div(grad(f)) = laplacian(f)"
      "assumptions declare scalar(f) and smooth(f)" #t)
    (make-math-rewrite-rule 'multiply-zero "X * 0 = 0" "X is scalar" #t)
    (make-math-rewrite-rule 'zero-multiply "0 * X = 0" "X is scalar" #t)
    (make-math-rewrite-rule 'power-one "X ^ 1 = X" "X is scalar" #t)
    (make-math-rewrite-rule 'power-zero "X ^ 0 = 1" "polynomial power convention" #t))
    (map (lambda (name)
           (make-math-rewrite-rule name (symbol->string name)
             "scalar polynomial in independent scalar coordinates; natural powers" #t))
         math-differentiation-rules)))

(define (math-rewrite-rule-ref name)
  (find (lambda (rule) (eq? (math-rewrite-rule-name rule) name))
        math-rewrite-rules))

(define math-simplification-rules
  '(subtract-zero add-zero zero-add subtract-right-zero double-negation
    multiply-one one-multiply multiply-zero zero-multiply power-one power-zero))

;; Inverse decoration rules are available on explicit selection, but are not
;; useful as default proof-search branches. Subtract-zero retains its inverse.
(define math-backward-rules
  '(curl-curl-identity divergence-free div-curl-zero subtract-zero
    curl-grad-zero div-grad-laplacian))

;; Diagnostic contracts describe explicit facts, not facts proved by Grove.
;; #f means the named rule does not match this left-hand structure. Types are
;; independently checked by the judgment; only direct declarations appear here.
(define (math-rule-required-facts name input)
  (define (zero? term) (member term '((num "0") (zero vector))))
  (match (list name input)
    (('curl-curl-identity ('curl ('curl field))) `((smooth ,field)))
    (('divergence-free ('grad ('div field))) `((= (div ,field) (num "0"))))
    (('div-curl-zero ('div ('curl field))) `((vector ,field) (smooth ,field)))
    (('curl-grad-zero ('curl ('grad field))) `((scalar ,field) (smooth ,field)))
    (('div-grad-laplacian ('div ('grad field))) `((scalar ,field) (smooth ,field)))
    (('subtract-zero ('- zero term)) (and (zero? zero) '()))
    (('add-zero ('+ term zero)) (and (zero? zero) '()))
    (('zero-add ('+ zero term)) (and (zero? zero) '()))
    (('subtract-right-zero ('- term zero)) (and (zero? zero) '()))
    (('double-negation ('- ('- term))) '())
    (('multiply-one ('* term ('num "1"))) '())
    (('one-multiply ('* ('num "1") term)) '())
    (('multiply-zero ('* term ('num "0"))) '())
    (('zero-multiply ('* ('num "0") term)) '())
    (('power-one ('^ term ('num "1"))) '())
    (('power-zero ('^ term ('num "0"))) '())
    (('differentiate-constant ('differentiate ('num value) ('var variable))) '())
    (('differentiate-variable ('differentiate ('var name) ('var variable)))
     (and (eq? name variable) '()))
    (('differentiate-independent ('differentiate ('var name) ('var variable)))
     (and (not (eq? name variable)) '()))
    (('differentiate-negation ('differentiate ('- operand) ('var variable))) '())
    (('differentiate-sum ('differentiate ('+ left right) ('var variable))) '())
    (('differentiate-difference ('differentiate ('- left right) ('var variable))) '())
    (('differentiate-product ('differentiate ('* left right) ('var variable))) '())
    (('differentiate-power ('differentiate ('^ base ('num exponent)) ('var variable)))
     (and (polynomial-term? `(^ ,base (num ,exponent))) '()))
    (_ #f)))

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
    (== output '(zero vector))))

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

(define (zeroo type term)
  (conde
    ((== type 'scalar) (== term '(num "0")))
    ((== type 'vector) (== term '(zero vector)))))

(define (subtract-zeroo assumptions input output)
  (fresh (term zero type)
    (== input `(- ,zero ,term))
    (== output `(- ,term))
    (math-typeo assumptions term type)
    (zeroo type zero)))

(define (scalar-field-identityo assumptions input output rule)
  (fresh (field)
    (conde
      ((== input `(curl (grad ,field)))
       (== output '(zero vector))
       (== rule 'curl-grad-zero))
      ((== input `(div (grad ,field)))
       (== output `(laplacian ,field))
       (== rule 'div-grad-laplacian)))
    ;; Explicit facts make inversion of vector zero a finite query.
    (assumptiono assumptions `(scalar ,field))
    (assumptiono assumptions `(smooth ,field))
    (math-typeo assumptions field 'scalar)))

;; Both sides are unified before type judgments: forward queries ground X from
;; the input; inverse queries ground it from the output. All forward rules shrink.
(define (simplifyo assumptions input output rule)
  (fresh (term type)
    (== output term)
    (conde
      ((fresh (zero)
         (== input `(+ ,term ,zero))
         (== rule 'add-zero)
         (math-typeo assumptions term type)
         (zeroo type zero)))
      ((fresh (zero)
         (== input `(+ ,zero ,term))
         (== rule 'zero-add)
         (math-typeo assumptions term type)
         (zeroo type zero)))
      ((fresh (zero)
         (== input `(- ,term ,zero))
         (== rule 'subtract-right-zero)
         (math-typeo assumptions term type)
         (zeroo type zero)))
      ((== input `(- (- ,term))) (== rule 'double-negation)
       (math-typeo assumptions term type)
       (conde ((== type 'scalar)) ((== type 'vector))))
      ((== input `(* ,term (num "1"))) (== rule 'multiply-one)
       (math-typeo assumptions term type)
       (conde ((== type 'scalar)) ((== type 'vector))))
      ((== input `(* (num "1") ,term)) (== rule 'one-multiply)
       (math-typeo assumptions term type)
       (conde ((== type 'scalar)) ((== type 'vector)))))))

(define (unrewrite-rooto assumptions input output rule)
  (conde
    ((== rule 'curl-curl-identity)
     (curl-curlo assumptions input output))
    ((fresh (field)
       (== input `(grad (div ,field)))
       (== output '(zero vector))
       (assumptiono assumptions `(= (div ,field) (num "0")))
       (math-typeo assumptions field 'vector)
       (== rule 'divergence-free)))
    ((== rule 'div-curl-zero)
     (div-curl-zeroo assumptions input output))
    ((== rule 'subtract-zero)
     (subtract-zeroo assumptions input output))
    ((scalar-field-identityo assumptions input output rule))
    ((simplifyo assumptions input output rule))))

;; Apply one named rule at INPUT, returning the rule that fired.
(define (rewrite-rooto assumptions input output rule)
  (conde
    ((differentiation-stepo assumptions input output rule))
    ((polynomial-simplifyo assumptions input output rule))
    ((== rule 'curl-curl-identity)
     (curl-curlo assumptions input output))
    ((== rule 'divergence-free)
     (divergence-zeroo assumptions input output))
    ((== rule 'div-curl-zero)
     (div-curl-zeroo assumptions input output))
    ((== rule 'subtract-zero)
     (subtract-zeroo assumptions input output))
    ((scalar-field-identityo assumptions input output rule))
    ((simplifyo assumptions input output rule))))

(define (polynomial-simplifyo assumptions input output rule)
  (fresh (term)
    (conde
      ((== input `(* ,term (num "0"))) (== output '(num "0"))
       (== rule 'multiply-zero))
      ((== input `(* (num "0") ,term)) (== output '(num "0"))
       (== rule 'zero-multiply))
      ((== input `(^ ,term (num "1"))) (== output term) (== rule 'power-one))
      ((== input `(^ ,term (num "0"))) (== output '(num "1"))
       (== rule 'power-zero)))
    (math-typeo assumptions term 'scalar)))

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
         (== path rest-path))))))

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
         (== path rest-path))))))

;; Compatibility relation for callers that need only the resulting term.
(define (rewriteo assumptions input output)
  (fresh (rule path)
    (rewrite-stepo assumptions input output rule path)))

(define* (rewrite-curl-curl term #:key (assumptions '()))
  (let ((answers
          (run 1 (result)
            (curl-curlo assumptions term result))))
    (and (pair? answers) (car answers))))
