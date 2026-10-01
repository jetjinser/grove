;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math partial)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math rule)
  #:use-module (grove math differentiation)
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

(define math-partial-rule-definitions (map partial-rule math-differentiation-rule-definitions))
(define math-partial-rules (map math-rewrite-rule-name math-partial-rule-definitions))
