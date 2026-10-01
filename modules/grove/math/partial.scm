;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math partial)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math rule)
  #:use-module (grove math differentiation)
  #:use-module (grove math function-derivative)
  #:use-module (grove math types)
  #:use-module (grove math declaration)
  #:use-module (grove math operator)
  #:use-module (grove math number)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (math-partial-rule-definitions math-partial-rules))

;; Reuse scalar calculus relations, changing only the derivative operator.
;; The scalar API keeps its closed input vocabulary; a partial normal form may
;; retain derivatives of declared multivariate functions as explicit terms.
(define (rename-derivatives term from to)
  (cond ((not (pair? term)) term)
        ((memq (car term) '(var num zero)) term)
        ((eq? (car term) from)
         (cons to (map (lambda (child) (rename-derivatives child from to)) (cdr term))))
        (else (map (lambda (child) (rename-derivatives child from to)) term))))

(define (independent-variable? facts input)
  (or (member `(coordinate ,(cadr input)) facts)
      (member `(parameter ,(cadr input)) facts)))

(define (partial-rule rule)
  (let* ((schema (rename-derivatives (math-rewrite-rule-schema rule) 'differentiate 'partial))
         (expression (cadar schema)) (coordinate (caddar schema))
         (premises `((coordinate ,coordinate) (differentiable ,expression))))
    (define (required input)
      (let ((facts (and (pair? input) (eq? (car input) 'partial)
                       ((math-rewrite-rule-required-facts rule)
                        (rename-derivatives input 'partial 'differentiate)))))
        (and facts
             (append `((coordinate ,(caddr input)) (differentiable ,(cadr input))) facts))))
    (make-math-rewrite-rule
      (string->symbol
        (string-append "partial-" (substring (symbol->string (math-rewrite-rule-name rule))
                                            (string-length "differentiate-"))))
      (math-rewrite-rule-equation rule)
      "C1 scalar expression in explicitly declared independent coordinates" 'partial
      (lambda (facts input output)
        (fresh (expression coordinate)
          (== input `(partial ,expression ,coordinate))
          (project (facts input)
            (let ((premises (required input)))
              (if (and premises
                       (or (not (eq? (math-rewrite-rule-name rule) 'differentiate-independent))
                           (independent-variable? facts input)))
                  (fresh (result)
                    (math-factso facts premises)
                    ((math-rewrite-rule-relation rule 'check) facts
                      (rename-derivatives input 'partial 'differentiate) result)
                    (project (result)
                      (== output (rename-derivatives result 'differentiate 'partial)))) fail)))))
      #:matcher required #:label (math-rewrite-rule-label rule)
      #:schema (list (car schema) (cadr schema)
                     (append premises (caddr schema))
                     (append (cadddr schema)
                       (if (eq? (math-rewrite-rule-name rule) 'differentiate-independent)
                           '((project (facts input)
                               (if (independent-variable? facts input) succeed fail))) '()))))))

(define (function-chain-premises input)
  (and (pair? input) (eq? (car input) 'partial)
       (or (eq? (caadr input) 'darg) (not (math-operator-arities (caadr input))))
       `((coordinate ,(caddr input)) (differentiable ,(cadr input)))))

(define function-chain-rule
  (make-math-rewrite-rule 'partial-function-chain
    "partial_x f(u_1,...,u_n) = sum_i partial_i f(u_1,...,u_n) partial_x u_i"
    "Declared scalar function, explicit coordinate, and sufficient function regularity" 'partial
    (lambda (facts input output)
      (fresh (application coordinate)
        (== input `(partial ,application ,coordinate))
        (project (facts input application coordinate)
          (let ((info (math-function-derivative-info facts application)))
            (if info
                (let ((terms (map (lambda (arg index)
                                    `(* (darg ,application (num ,(number->string index)))
                                        (partial ,arg ,coordinate)))
                                  (cadr info) (iota (length (cadr info)) 1))))
                  (fresh ()
                    (math-typeo facts application 'scalar)
                    (math-factso facts (function-chain-premises input))
                    (== output (if (null? terms) '(num "0")
                                   (fold (lambda (term sum) `(+ ,sum ,term)) (car terms) (cdr terms)))))) fail)))))
    #:matcher function-chain-premises
    #:documentation "Apply the multivariate chain rule using explicit one-based argument positions. Repeated and composed arguments remain distinct; C1 permits one derivative and C2 permits two."))

(define (mixed-partial-rule name operator)
  (define (premises input)
    (match input
      ((outer (inner expression a) b)
       (and (eq? outer operator) (eq? inner operator)
            (if (eq? operator 'partial)
                (and (eq? (car a) 'var) (eq? (car b) 'var)
                     (string>? (symbol->string (cadr a)) (symbol->string (cadr b))))
                (let ((i (math-number-value a)) (j (math-number-value b)))
                  (and i j (> i j))))
            (if (eq? operator 'partial)
                `((c2 ,expression) (coordinate ,a) (coordinate ,b))
                (let peel ((term expression) (order 2))
                  (if (eq? (car term) 'darg) (peel (cadr term) (+ order 1))
                      `((,(if (= order 2) 'c2 'smooth) (var ,(car term)))))))))
      (_ #f)))
  (make-math-rewrite-rule name "Exchange mixed derivatives into ascending order"
    "C2 operand; coordinate names and argument positions have separate orders" 'partial
    (lambda (facts input output)
      (project (facts input)
        (let ((required (premises input)))
          (if required
              (fresh ()
                (math-typeo facts input 'scalar)
                (math-factso facts required)
                (== output `(,operator (,operator ,(cadadr input) ,(caddr input)) ,(caddr (cadr input)))))
              fail))))
    #:matcher premises))

(define math-partial-rule-definitions
  (append (map partial-rule
            (filter (lambda (rule) (not (eq? (math-rewrite-rule-name rule) 'differentiate-function)))
                    math-differentiation-rule-definitions))
          (list function-chain-rule
                (mixed-partial-rule 'partial-coordinate-symmetry 'partial)
                (mixed-partial-rule 'partial-argument-symmetry 'darg))))
(define math-partial-rules (map math-rewrite-rule-name math-partial-rule-definitions))
