;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rule)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math premise)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-math-rewrite-rule math-rule math-factso
            math-rules-stepo
            math-rewrite-rule? math-rewrite-rule-name math-rewrite-rule-equation
            math-rewrite-rule-label
            math-rewrite-rule-condition math-rewrite-rule-condition-checked?
            math-rewrite-rule-group math-rewrite-rule-modes
            math-rewrite-rule-relation math-rewrite-rule-required-facts
            math-rewrite-rule-backward-default? math-rewrite-rule-schema
            math-rewrite-rule-documentation))

(define-record-type <math-rewrite-rule>
  (%make-math-rewrite-rule name equation label condition checked? group relation modes backward matcher default? schema)
  math-rewrite-rule?
  (name math-rewrite-rule-name)
  (equation math-rewrite-rule-equation)
  (label math-rewrite-rule-label)
  (condition math-rewrite-rule-condition)
  (checked? math-rewrite-rule-condition-checked?)
  (group math-rewrite-rule-group)
  (relation rule-relation)
  (modes math-rewrite-rule-modes)
  (backward rule-backward)
  (matcher math-rewrite-rule-required-facts)
  (default? math-rewrite-rule-backward-default?)
  (schema math-rewrite-rule-schema))

;; The same quasiquoted left side used by the relation is its diagnostic
;; pattern. Repeated placeholders must bind structurally identical terms.
(define (match-pattern pattern term bindings)
  (cond
    ((and (pair? pattern) (eq? (car pattern) 'unquote))
     (let ((previous (assq (cadr pattern) bindings)))
       (if previous (and (equal? (cdr previous) term) bindings)
           (cons (cons (cadr pattern) term) bindings))))
    ((pair? pattern)
     (and (pair? term)
          (let ((next (match-pattern (car pattern) (car term) bindings)))
            (and next (match-pattern (cdr pattern) (cdr term) next)))))
    (else (and (equal? pattern term) bindings))))

(define (instantiate template bindings)
  (cond ((and (pair? template) (eq? (car template) 'unquote))
         (assq-ref bindings (cadr template)))
        ((pair? template) (cons (instantiate (car template) bindings)
                               (instantiate (cdr template) bindings)))
        (else template)))

(define* (make-math-rewrite-rule name equation condition group relation
                               #:key (pattern #f) (premises '()) (matcher #f)
                               (forward? #t) (backward #f) (backward-default? #f)
                               (condition-checked? #t) (label #f) (schema #f)
                               (documentation #f))
  (unless (and (symbol? name) (string? equation) (string? condition)
               (or (not label) (string? label))
               (symbol? group) (procedure? relation)
               (or (not documentation) (string? documentation))
               (boolean? forward?) (boolean? backward-default?) (boolean? condition-checked?)
               (or matcher pattern) (or (not matcher) (procedure? matcher))
               (or (not backward) (eq? backward #t) (procedure? backward))
               (or (not backward-default?) backward))
    (error "invalid math rule declaration" name))
  (when documentation
    (set-procedure-property! relation 'documentation documentation))
  (%make-math-rewrite-rule name equation
    (or label (string-capitalize (string-map (lambda (c) (if (char=? c #\-) #\space c))
                                             (symbol->string name))))
    condition condition-checked? group relation
    (append (if forward? '(forward) '()) (if backward '(backward) '()) '(check))
    (if (eq? backward #t) relation backward)
    (or matcher (lambda (term)
                  (let ((bindings (match-pattern pattern term '())))
                    (and bindings (instantiate premises bindings)))))
    backward-default? schema))

(define (math-rewrite-rule-documentation rule)
  "Return the execution relation's Guile docstring, or #f if absent."
  (procedure-documentation (rule-relation rule)))

(define (math-rewrite-rule-relation rule mode)
  (and (memq mode (math-rewrite-rule-modes rule))
       (if (eq? mode 'backward) (rule-backward rule) (rule-relation rule))))

(define (math-rules-stepo rules assumptions input output name mode)
  (if (null? rules) fail
      (let* ((rule (car rules)) (relation (math-rewrite-rule-relation rule mode)))
        (conde
          ((== name (math-rewrite-rule-name rule))
           (if relation (relation assumptions input output) fail))
          ((math-rules-stepo (cdr rules) assumptions input output name mode))))))

(define (facto assumptions fact)
  (conde
    ((fresh (rest) (== `(,fact . ,rest) assumptions)))
    ((fresh (head rest)
       (== `(,head . ,rest) assumptions) (=/= head fact) (facto rest fact)))))

(define (math-factso assumptions facts)
  (if (null? facts) succeed
      (fresh ()
        (conde
          ((facto assumptions (car facts)))
          ((project (assumptions facts)
             (if (and (not (member (car facts) assumptions))
                      (math-premise-known? assumptions (car facts))) succeed fail))))
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
       #:pattern (quote left) #:premises (quote premises)
       #:schema (quote (left right premises (goal ...))) option ...))))
