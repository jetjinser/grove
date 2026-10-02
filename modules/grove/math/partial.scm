;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math partial)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math rule)
  #:use-module (grove math differentiation)
  #:use-module (grove math types)
  #:use-module (grove math number)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (math-partial-rule-definitions math-partial-rules))

;; Reuse scalar calculus relations, changing only the derivative operator.
;; Both APIs share argument-indexed function calculus. The partial API also
;; requires explicit coordinate roles and operand regularity.
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
  (let* ((original (math-rewrite-rule-spec rule))
         (left (rename-derivatives (math-rule-spec-left original) 'differentiate 'partial))
         (right (rename-derivatives (math-rule-spec-right original) 'differentiate 'partial))
         (base-premises (math-rule-spec-premises original))
         (guards (rename-derivatives (math-rule-spec-guards original) 'differentiate 'partial))
         (expression (cadr left)) (coordinate (caddr left))
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
      #:spec (make-math-rule-spec left right
               (append premises base-premises)
               (append guards
                 (if (eq? (math-rewrite-rule-name rule) 'differentiate-independent)
                     '((project (facts input)
                         (if (independent-variable? facts input) succeed fail))) '()))))))

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
    "C2 operand; coordinate names and argument positions have separate orders" 'normalize
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
          (list (make-function-chain-rule (quote partial))
                (mixed-partial-rule 'partial-coordinate-symmetry 'partial)
                (mixed-partial-rule 'partial-argument-symmetry 'darg))))
(define math-partial-rules
  (map math-rewrite-rule-name
    (filter (lambda (rule) (eq? (math-rewrite-rule-group rule) 'partial)) math-partial-rule-definitions)))
