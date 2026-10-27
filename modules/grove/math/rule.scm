;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rule)
  #:use-module (grove private snapshot)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math premise)
  #:use-module (grove math rule-spec)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-math-rewrite-rule math-rule math-factso
            math-rules-stepo
            math-rewrite-rule? math-rewrite-rule-name math-rewrite-rule-equation
            math-rewrite-rule-label
            math-rewrite-rule-condition math-rewrite-rule-condition-checked?
            math-rewrite-rule-group math-rewrite-rule-modes
            math-rewrite-rule-relation math-rewrite-rule-required-facts
            math-rewrite-rule-backward-default? math-rewrite-rule-spec
            math-rewrite-rule-documentation)
  #:re-export (make-math-rule-spec math-rule-spec? math-rule-spec-left
    math-rule-spec-right math-rule-spec-premises math-rule-spec-guards
    math-rule-spec-match math-rule-spec-instantiate
    math-rule-spec-guard-kinds math-rule-spec-opaque-guards))

(define-record-type <math-rewrite-rule>
  (%make-math-rewrite-rule name equation label condition checked? group relation modes backward matcher default? spec)
  math-rewrite-rule?
  (name math-rewrite-rule-name)
  (equation rule-equation/raw)
  (label rule-label/raw)
  (condition rule-condition/raw)
  (checked? math-rewrite-rule-condition-checked?)
  (group math-rewrite-rule-group)
  (relation rule-relation)
  (modes rule-modes/raw)
  (backward rule-backward)
  (matcher math-rewrite-rule-required-facts)
  (default? math-rewrite-rule-backward-default?)
  (spec math-rewrite-rule-spec))

(define (math-rewrite-rule-equation rule)
  (snapshot-data (rule-equation/raw rule)))

(define (math-rewrite-rule-label rule)
  (snapshot-data (rule-label/raw rule)))

(define (math-rewrite-rule-condition rule)
  (snapshot-data (rule-condition/raw rule)))

(define (math-rewrite-rule-modes rule)
  (snapshot-data (rule-modes/raw rule)))

(define* (make-math-rewrite-rule name equation condition group relation
                               #:key (matcher #f)
                               (forward? #t) (backward #f) (backward-default? #f)
                               (condition-checked? #t) (label #f) (spec #f)
                               (documentation #f))
  (unless (and (symbol? name) (string? equation) (string? condition)
               (or (not label) (string? label))
               (symbol? group) (procedure? relation)
               (or (not documentation) (string? documentation))
               (boolean? forward?) (boolean? backward-default?) (boolean? condition-checked?)
               (or matcher spec) (or (not matcher) (procedure? matcher))
               (or (not backward) (eq? backward #t) (procedure? backward))
               (or (not spec) (math-rule-spec? spec))
               (or (not backward-default?) backward))
    (error "invalid math rule declaration" name))
  (when documentation
    (set-procedure-property! relation 'documentation
                             (snapshot-data documentation)))
  (%make-math-rewrite-rule name (snapshot-data equation)
    (snapshot-data
      (or label (string-capitalize
                  (string-map (lambda (c) (if (char=? c #\-) #\space c))
                              (symbol->string name)))))
    (snapshot-data condition) condition-checked? group relation
    (snapshot-data
      (append (if forward? '(forward) '())
              (if backward '(backward) '()) '(check)))
    (if (eq? backward #t) relation backward)
    (or matcher (lambda (term)
                  (let ((bindings (math-rule-spec-match spec term)))
                    (and bindings
                         (math-rule-spec-instantiate
                           spec (math-rule-spec-premises spec) bindings)))))
    backward-default? spec))

(define (math-rewrite-rule-documentation rule)
  "Return the execution relation's Guile docstring, or #f if absent."
  (snapshot-data (procedure-documentation (rule-relation rule))))

(define (math-rewrite-rule-relation rule mode)
  (and (memq mode (math-rewrite-rule-modes rule))
       (if (eq? mode 'backward) (rule-backward rule) (rule-relation rule))))

(define (candidate-rules rules term mode)
  ;; A known root operator can reject impossible schemas before constructing
  ;; their relational goals. Unknown roots and opaque rules retain all choices.
  (if (and (pair? term) (symbol? (car term)))
      (filter
        (lambda (rule)
          (let* ((spec (math-rewrite-rule-spec rule))
                 (pattern (and spec (if (eq? mode 'backward)
                                        (math-rule-spec-right spec)
                                        (math-rule-spec-left spec)))))
            (or (not (pair? pattern)) (not (symbol? (car pattern)))
                (eq? (car pattern) 'unquote) (eq? (car pattern) (car term)))))
        rules)
      rules))

(define (selected-rules-stepo rules candidates assumptions input output name mode)
  ;; Keep the original conde tree: pruning its branches would change fair
  ;; interleaving and therefore the first derivation chosen by bounded search.
  (if (null? rules) fail
      (let* ((rule (car rules))
             (relation (and (memq rule candidates)
                            (math-rewrite-rule-relation rule mode))))
        (conde
          ((== name (math-rewrite-rule-name rule))
           (if relation (relation assumptions input output) fail))
          ((selected-rules-stepo (cdr rules) candidates
                                 assumptions input output name mode))))))

(define (math-rules-stepo rules assumptions input output name mode)
  (project (input output)
    (selected-rules-stepo rules
      (candidate-rules rules (if (eq? mode 'backward) output input) mode)
      assumptions input output name mode)))

(define (facto assumptions fact)
  (conde
    ((fresh (rest) (== `(,fact . ,rest) assumptions)))
    ((fresh (head rest)
       (== `(,head . ,rest) assumptions) (=/= head fact) (facto rest fact)))))

(define (math-factso assumptions facts)
  (if (null? facts) succeed
      (fresh ()
        (project (assumptions)
          (let ((known
                  (delete-duplicates
                    (append-map
                      (lambda (fact)
                        (cons fact (case (car fact)
                                     ((smooth) `((c2 ,(cadr fact)) (differentiable ,(cadr fact))))
                                     ((c2) `((differentiable ,(cadr fact)))) (else '())))) assumptions) equal?)))
            (conde
              ((facto known (car facts)))
              ((project (facts)
                 (if (and (not (member (car facts) known))
                          (math-premise-known? assumptions (car facts))) succeed fail))))))
        (math-factso assumptions (cdr facts)))))

(define-syntax math-rule
  (syntax-rules ()
    ((_ name equation condition group (assumptions variable ...)
        left right premises (goal ...) option ...)
     (make-math-rewrite-rule name equation condition group
       (lambda (assumptions input output)
         (fresh (variable ...)
           (== input (quasiquote left))
           (== output (quasiquote right))
           (math-factso assumptions (quasiquote premises))
           goal ...))
       #:spec (make-math-rule-spec (quote left) (quote right)
               (quote premises) (quote (goal ...))) option ...))))
