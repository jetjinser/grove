;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math operator)
  #:use-module (srfi srfi-1)
  #:export (math-operator-arities math-operator-signatures math-operator-symbol))

;; Entries: name, type signatures (operand types followed by result), optional
;; infix display symbol. Arity is derived from signatures, not repeated.
;; Algorithms and special layout remain ordinary passes: a signature does not
;; promise that every algorithm supports the operator.
(define operators
  '((+ ((scalar scalar scalar) (vector vector vector)) "+")
    (- ((scalar scalar) (vector vector)
        (scalar scalar scalar) (vector vector vector)) "−")
    (* ((scalar scalar scalar) (scalar vector vector) (vector scalar vector)) "×")
    (/ ((scalar scalar scalar) (vector scalar vector)) #f)
    (^ ((scalar scalar scalar)) #f)
    (= ((scalar scalar proposition) (vector vector proposition)) "=")
    (grad ((scalar vector)) #f)
    (curl ((vector vector)) #f)
    (div ((vector scalar)) #f)
    (laplacian ((scalar scalar) (vector vector)) #f)
    (differentiate ((scalar scalar scalar)) #f)
    (sin ((scalar scalar)) #f)
    (cos ((scalar scalar)) #f)
    (exp ((scalar scalar)) #f)
    (log ((scalar scalar)) #f)
    (prime ((function scalar scalar)) #f)))

;; Assertions are reserved unary forms, not expression type signatures.
(define assertion-operators '(scalar vector smooth coordinate parameter nonzero positive))
(define arities
  (append (map (lambda (entry)
                 (cons (car entry)
                       (delete-duplicates (map (lambda (signature) (- (length signature) 1))
                                               (cadr entry)))))
               operators)
          (map (lambda (name) (list name 1)) assertion-operators)))

(define (math-operator-arities name) (assq-ref arities name))
(define (math-operator-symbol name)
  (let ((entry (assq name operators))) (and entry (caddr entry))))
(define math-operator-signatures
  (append-map
    (lambda (entry)
      (map (lambda (signature)
             (list (car entry) (drop-right signature 1) (last signature)))
           (cadr entry)))
    operators))
