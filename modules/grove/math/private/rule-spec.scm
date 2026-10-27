;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private rule-spec)
  #:use-module (grove private snapshot)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-math-rule-spec math-rule-spec?
            math-rule-spec-left math-rule-spec-right
            math-rule-spec-premises math-rule-spec-guards
            math-rule-spec-match math-rule-spec-instantiate
            math-rule-spec-guard-kinds math-rule-spec-opaque-guards))

;; RuleSpec is the normalized declarative part of a math-rule declaration.
;; Guards retain their authored goal form; only recognized forms are assigned
;; a semantic kind, while opaque Scheme/miniKanren goals stay explicitly opaque.
(define-record-type <math-rule-spec>
  (%make-math-rule-spec left right premises guards)
  math-rule-spec?
  (left rule-spec-left/raw)
  (right rule-spec-right/raw)
  (premises rule-spec-premises/raw)
  (guards rule-spec-guards/raw))

(define (math-rule-spec-left spec)
  (snapshot-data (rule-spec-left/raw spec)))

(define (math-rule-spec-right spec)
  (snapshot-data (rule-spec-right/raw spec)))

(define (math-rule-spec-premises spec)
  (snapshot-data (rule-spec-premises/raw spec)))

(define (math-rule-spec-guards spec)
  (snapshot-data (rule-spec-guards/raw spec)))

(define (rule-template? value)
  (cond
    ((pair? value)
     (if (eq? (car value) 'unquote)
         (and (list? value) (= (length value) 2) (symbol? (cadr value)))
         (and (rule-template? (car value)) (rule-template? (cdr value)))))
    ((null? value) #t)
    ((or (symbol? value) (string? value) (number? value) (boolean? value) (char? value)) #t)
    (else #f)))

(define (make-math-rule-spec left right premises guards)
  (unless (and (list? left) (pair? left)
               (list? right) (pair? right)
               (rule-template? left) (rule-template? right)
               (list? premises) (every (lambda (form) (and (list? form) (rule-template? form))) premises)
               (list? guards) (every list? guards))
    (error "invalid math rule specification" left right premises guards))
  (%make-math-rule-spec (snapshot-data left)
                        (snapshot-data right)
                        (snapshot-data premises)
                        (snapshot-data guards)))

(define (math-rule-spec-match spec pattern)
  "Match PATTERN against a RuleSpec left side and return its bindings."
  (define (match-template template term bindings)
    (match template
      (('unquote name)
       (let ((previous (assq name bindings)))
         (if previous
             (and (equal? (cdr previous) term) bindings)
             (cons (cons name term) bindings))))
      ((head . tail)
       (and (pair? term)
            (let ((next (match-template head (car term) bindings)))
              (and next (match-template tail (cdr term) next)))))
      (_ (and (equal? template term) bindings))))
  (unless (math-rule-spec? spec) (error "expected a math rule spec" spec))
  (match-template (math-rule-spec-left spec) pattern '()))

(define (math-rule-spec-instantiate spec template bindings)
  "Instantiate TEMPLATE using bindings returned by math-rule-spec-match."
  (define (instantiate template)
    (match template
      (('unquote name)
       (let ((binding (assq name bindings)))
         (unless binding (error "unbound RuleSpec metavariable" name))
         (cdr binding)))
      ((head . tail) (cons (instantiate head) (instantiate tail)))
      (_ template)))
  (unless (math-rule-spec? spec) (error "expected a math rule spec" spec))
  (instantiate template))

(define (guard-kind guard)
  (match guard
    (('math-typeo _ term ('quote type))
     (and (symbol? type) `(type ,term ,type)))
    (('math-typeo _ term type)
     (and (symbol? type) `(type ,term ,type)))
    (('zeroo type zero)
     (and (symbol? type) (symbol? zero) `(zero-of-type ,type ,zero)))
    (('=/= left right) `(distinct ,left ,right))
    (_ #f)))

(define (math-rule-spec-guard-kinds spec)
  (unless (math-rule-spec? spec) (error "expected a math rule spec" spec))
  (map guard-kind (math-rule-spec-guards spec)))

(define (math-rule-spec-opaque-guards spec)
  (unless (math-rule-spec? spec) (error "expected a math rule spec" spec))
  (filter-map (lambda (guard) (and (not (guard-kind guard)) guard))
    (math-rule-spec-guards spec)))
