;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math catalog)
  #:use-module (grove relation)
  #:use-module (grove math rewrite)
  #:use-module (grove math rule)
  #:use-module (grove math layout)
  #:use-module (grove math term)
  #:use-module (grove math operator)
  #:use-module (grove ir)
  #:use-module ((grove math notation core) #:prefix notation:)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;; Query the finite registry without running a rewrite or inventing operands.
(define catalog
  (make-relation
    (map (lambda (rule)
           (list (math-rewrite-rule-name rule) (math-rewrite-rule-group rule)
                 (math-rewrite-rule-modes rule)
                 (math-rewrite-rule-documentation rule))) math-rewrite-rules)
    #:arity 4))

(define-public (ruleo name group modes documentation)
  "Relate rule names, groups, mode lists and Guile docstrings (#f if absent).
Every argument may be unknown."
  (relationo catalog name group modes documentation))

(define mode-catalog
  (make-relation
    (append-map (lambda (rule)
                  (map (lambda (mode) (list (math-rewrite-rule-name rule) mode))
                       (math-rewrite-rule-modes rule)))
                math-rewrite-rules)
    #:arity 2))

(define-public (rule-modeo name mode)
  "Relate a registered rule to each execution mode it supports."
  (relationo mode-catalog name mode))

(define (schema-operators term)
  (if (not (pair? term)) '()
      (append (if (math-operator-arities (car term)) (list (car term)) '())
              (append-map schema-operators (cdr term)))))

(define operation-catalog
  (make-relation
    (append-map
      (lambda (rule)
        (let ((schema (math-rewrite-rule-schema rule)))
          (if schema
              (map (lambda (operator) (list (math-rewrite-rule-name rule) operator))
                   (delete-duplicates (schema-operators (car schema))))
              '())))
      math-rewrite-rules)
    #:arity 2))

(define-public (rule-operationo name operator)
  "Relate a schema-backed rule to operators occurring in its input pattern."
  (relationo operation-catalog name operator))

(define premise-catalog
  (make-relation
    (append-map
      (lambda (rule)
        (let ((schema (math-rewrite-rule-schema rule)))
          (if schema
              (filter-map (lambda (premise)
                            (and (pair? premise) (symbol? (car premise))
                                 (list (math-rewrite-rule-name rule) (car premise) premise)))
                          (list-ref schema 2))
              '())))
      math-rewrite-rules)
    #:arity 3))

(define-public (rule-premiseo name predicate premise)
  "Relate a schema-backed rule to each declared premise predicate and term."
  (relationo premise-catalog name predicate premise))

;; Metavariables are display placeholders, never concrete solver inputs.
(define (template-term template)
  (match template
    (('unquote name) `(var ,name))
    (('var ('unquote name)) `(var ,name))
    (('num ('unquote name)) `(var ,name))
    ((head . tail) (cons (template-term head) (template-term tail)))
    (_ template)))

(define (text value) (notation:token 'mtext value))
(define (operator value) (notation:token 'mo value))
(define (template-layout template)
  (let ((term (template-term template)))
    (if (math-term? term) (math-term->layout term)
        (text (format #f "~s" template)))))

(define (separate layouts)
  (append-map (lambda (layout index)
                (append (if (zero? index) '() (list (text "   "))) (list layout)))
              layouts (iota (length layouts))))

(define (type-guard? goal)
  (and (pair? goal) (eq? (car goal) 'math-typeo) (= (length goal) 4)))

(define (guard-layout goal)
  (match goal
    (('math-typeo facts operand ('quote type))
     (notation:row (list (template-layout (list 'unquote operand))
                         (operator ":") (text (symbol->string type)))))
    (_ (text (format #f "~s" goal)))))

(define-public (math-rule->content name)
  "Render the named rule's schema, documentation and executable guards as content."
  (let ((rule (math-rewrite-rule-ref name)))
    (unless rule (error "unknown math rule" name))
    (let* ((schema (math-rewrite-rule-schema rule))
           (premises (if schema (list-ref schema 2) '()))
           (guards (if schema (list-ref schema 3) '()))
           (types (filter type-guard? guards))
           (checks (remove type-guard? guards))
           (numerator
             (append (map template-layout premises) (map guard-layout types)
               (if (or (not schema) (and (null? premises) (null? types)))
                   (list (text (math-rewrite-rule-condition rule))) '())
               (cond ((not schema) (list (text "; algorithm applicability checked")))
                     ((pair? checks) (list (text "; executable guards apply")))
                     (else '()))))
           (conclusion
             (if schema
                 (list (template-layout (car schema)) (operator "→")
                       (template-layout (cadr schema)))
                 (list (text (math-rewrite-rule-equation rule))))))
      (make-content-element 'math-derivation
        (append
          (list (make-content-element 'paragraph (list (symbol->string name)) #:flow-kind 'block))
          (let ((documentation (math-rewrite-rule-documentation rule)))
            (if documentation
                (list (make-content-element 'paragraph (list documentation) #:flow-kind 'block))
                '()))
          (list (notation:display-math
                  (list (notation:frac (if (null? numerator) (list (text "—")) (separate numerator)) conclusion)
                        (text (format #f "  [~a]" name))))
                (make-content-element 'paragraph
                  (list (format #f "Group: ~a. Modes: ~a. ~a Condition: ~a"
                          (math-rewrite-rule-group rule) (math-rewrite-rule-modes rule)
                          (if schema "Rewrite schema; metavariables denote terms."
                              "Algorithm rule; this description is not a fixed rewrite schema.")
                          (math-rewrite-rule-condition rule))) #:flow-kind 'block))
          (if (null? checks) '()
              (list (make-content-element 'details
                      (list (make-content-element 'summary (list "Executable guards"))
                            (make-content-element 'verbatim (list (format #f "~s" checks)) #:flow-kind 'block))
                      #:flow-kind 'block)))) #:flow-kind 'block))))
