;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math types)
  #:use-module (grove relation)
  #:use-module (grove math operator)
  #:use-module (grove math assumption)
  #:use-module (grove math term)
  #:use-module (grove math diagnostic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (math-typeo
            math-infer-type
            math-type-diagnostic
            math-typed-term?
            math-typed-term-term
            math-typed-term-type))

(define-record-type <math-typed-term>
  (make-math-typed-term term type)
  math-typed-term?
  (term math-typed-term-term)
  (type math-typed-term-type))

(define (facto assumptions fact)
  (conde
    ((fresh (rest)
       (== `(,fact . ,rest) assumptions)))
    ((fresh (head rest)
       (== `(,head . ,rest) assumptions)
       (=/= head fact)
       (facto rest fact)))))

(define (math-typeo assumptions term type)
  (conde
    ((== term '(zero vector)) (== type 'vector))
    ((fresh (name)
       (== `(var ,name) term)
       (facto assumptions `(scalar (var ,name)))
       (== type 'scalar)))
    ((fresh (name)
       (== `(var ,name) term)
       (facto assumptions `(vector (var ,name)))
       (== type 'vector)))
    ((fresh (number)
       (== `(num ,number) term)
       (== type 'scalar)))
    ((fresh (operator operands signature)
       (== term `(,operator . ,operands))
       (signatureo operator signature type)
       (operand-typeso assumptions operands signature)
       (conde
         ((=/= operator 'differentiate))
         ((fresh (expression name)
            (== term `(differentiate ,expression (var ,name))))))))))

(define signatures (make-relation math-operator-signatures #:arity 3))
(define (signatureo operator operands result)
  (relationo signatures operator operands result))
(define (operand-typeso assumptions operands types)
  (conde
    ((== operands '()) (== types '()))
    ((fresh (operand rest type remaining)
       (== operands `(,operand . ,rest))
       (== types `(,type . ,remaining))
       (math-typeo assumptions operand type)
       (operand-typeso assumptions rest remaining)))))

(define (math-infer-type assumptions term)
  (and (math-term? term)
       (null? (math-assumption-diagnostics assumptions))
       (let ((types
               (delete-duplicates
                 (run* (type)
                   (math-typeo assumptions term type)))))
         (and (= (length types) 1)
              (make-math-typed-term term (car types))))))

(define (math-type-diagnostic assumptions term)
  (define (diagnose term)
    (cond
      ((and (pair? term) (eq? (car term) 'var))
       (let* ((name (cadr term))
              (scalar? (member `(scalar ,term) assumptions))
              (vector? (member `(vector ,term) assumptions)))
         (cond
           ((and scalar? vector?)
            (format #f "variable ~a has conflicting scalar and vector declarations"
                    name))
           ((not (or scalar? vector?))
            (format #f "variable ~a has no type declaration; add scalar(~a) or vector(~a)"
                    name name name))
           (else #f))))
      ((and (pair? term) (memq (car term) '(curl grad div))
            (= (length term) 2))
       (let* ((operator (car term))
              (argument (cadr term))
              (expected (if (eq? operator 'grad) 'scalar 'vector))
              (typed (math-infer-type assumptions argument)))
         (cond
           ((and typed (not (eq? (math-typed-term-type typed) expected)))
            (format #f "~a requires a ~a operand, but this expression is ~a"
                    operator expected (math-typed-term-type typed)))
           ((not typed)
            (if (and (pair? argument) (eq? (car argument) 'var)
                     (not (member `(scalar ,argument) assumptions))
                     (not (member `(vector ,argument) assumptions)))
                (format #f "~a requires a ~a operand; add ~a(~a)"
                        operator expected expected (cadr argument))
                (or (diagnose argument)
                    (format #f "cannot determine the type required by ~a"
                            operator))))
           (else #f))))
      ((and (pair? term) (list? term))
       (let loop ((children (cdr term)))
         (and (pair? children)
              (or (diagnose (car children))
                  (loop (cdr children))))))
      (else #f)))
  (or (let ((diagnostics (math-assumption-diagnostics assumptions)))
        (and (pair? diagnostics) (math-diagnostic-message (car diagnostics))))
      (diagnose term)
      (let* ((signatures (filter (lambda (entry)
                                  (and (eq? (car entry) (car term))
                                       (= (length (cadr entry)) (length (cdr term)))))
                                math-operator-signatures))
             (operands (map (lambda (operand) (math-infer-type assumptions operand)) (cdr term))))
        (and (pair? signatures) (every identity operands)
             (format #f "~a cannot accept operand types ~s; expected one of ~s"
                     (car term) (map math-typed-term-type operands)
                     (map cadr signatures))))
      (format #f "no type rule applies to ~s" term)))
