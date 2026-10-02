;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:)
             (grove math pipeline)
             (grove math rewrite)
             (grove math rule)
             (grove math semantic-check)
             (grove math derivation)
             (ice-9 match)
             (grove ir)
             (grove render html)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "independent scalar-polynomial semantic checker")

(test-assert "exact sparse polynomials collect and multiply independently"
  (math-polynomial-equivalent?
    (φ:term "(x+y)^2")
    (φ:term "x^2+2*x*y+y^2")))
(test-assert "rational constants are normalized exactly"
  (math-polynomial-equivalent? (φ:term "x/2+x/2") (φ:term "x")))
(test-assert "expression denominators remain outside this checker"
  (not (math-polynomial-equivalent? (φ:term "x/x") (φ:term "1"))))
(test-assert "normalization limits report unsupported instead of expanding without bound"
  (not (math-polynomial-normal-form (φ:term "(x+y)^65"))))

(define (schema-metavariables term)
  (match term
    (('unquote name) (list name))
    ((head . tail) (append (schema-metavariables head)
                           (schema-metavariables tail)))
    (_ '())))

(define (instantiate-schema term bindings)
  (match term
    (('unquote name) (assq-ref bindings name))
    ((head . tail) (cons (instantiate-schema head bindings)
                         (instantiate-schema tail bindings)))
    (_ term)))

(define (scalar-variable-facts term)
  (match term
    (('var name) `((scalar (var ,name))))
    ((head . tail) (append (scalar-variable-facts head)
                           (scalar-variable-facts tail)))
    (_ '())))

(define (check-schema-implementation rule sample)
  (let* ((spec (math-rewrite-rule-spec (math-rewrite-rule-ref rule)))
         (names (delete-duplicates
                  (append (schema-metavariables (math-rule-spec-left spec))
                          (schema-metavariables (math-rule-spec-right spec)))))
         (zero-names (filter-map
                       (lambda (guard)
                         (match guard
                           (('zeroo _ (? symbol? name)) name)
                           (_ #f)))
                       (math-rule-spec-guards spec)))
         (bindings
           (map (lambda (name)
                  (cons name
                    (cond ((memq name zero-names) '(num "0"))
                          ((eq? sample 'variable) `(var ,name))
                          (else `(+ (var ,name) (num "1"))))))
             names))
         (input (instantiate-schema (math-rule-spec-left spec) bindings))
         (expected (instantiate-schema (math-rule-spec-right spec) bindings))
         (premises (instantiate-schema (math-rule-spec-premises spec) bindings))
         (assumptions
           (delete-duplicates
             (append
               (append-map (lambda (binding)
                             (scalar-variable-facts (cdr binding))) bindings)
               premises)
             equal?))
         (answers (math-rewrite-answers assumptions input (list rule) #f
                    #:mode 'check #:max-results 16)))
    (any (lambda (answer)
           (and (equal? (car answer) expected) (null? (caddr answer))))
      answers)))

(define checked-schema-rules
  (filter (lambda (rule)
            (math-semantic-check-valid? (φ:check-polynomial-rule rule)))
    (math-rule-names #:mode 'check)))

(for-each
  (lambda (rule)
    (test-assert (format #f "schema relation matches checker for ~a variables" rule)
      (check-schema-implementation rule 'variable))
    (test-assert (format #f "schema relation matches checker for ~a compound terms" rule)
      (check-schema-implementation rule 'compound)))
  checked-schema-rules)
(test-equal "algorithm rules are not presented as checked polynomial laws"
  'unsupported
  (math-semantic-check-status (φ:check-polynomial-rule 'differentiate-power)))
(test-equal "basic algebra laws have checked universal RuleSpecs"
  '(semantically-checked semantically-checked)
  (map (lambda (name)
         (math-semantic-check-status (φ:check-polynomial-rule name)))
    '(add-zero multiply-associative)))
(let ((conditional-cancel
        (make-math-rule-spec
          '(/ (* (unquote value) (unquote factor)) (unquote factor))
          '(unquote value)
          '((nonzero (unquote factor)))
          '((math-typeo facts value (quote scalar))
            (math-typeo facts factor (quote scalar)))))
      (unjustified-cancel
        (make-math-rule-spec
          '(/ (* (unquote value) (unquote factor)) (unquote factor))
          '(unquote value) '()
          '((math-typeo facts value (quote scalar))
            (math-typeo facts factor (quote scalar)))))
      (false-cancel
        (make-math-rule-spec
          '(/ (* (unquote value) (unquote factor)) (unquote factor))
          '(unquote other)
          '((nonzero (unquote factor))) '())))
  (test-equal "symbolic cancellation is proved under an explicit nonzero premise"
    'semantically-checked
    (math-semantic-check-status (math-polynomial-check-rule-spec conditional-cancel)))
  (test-equal "symbolic cancellation without its domain premise is rejected"
    'rejected
    (math-semantic-check-status (math-polynomial-check-rule-spec unjustified-cancel)))
  (test-equal "a nonzero premise cannot certify a forged cancellation result"
    'rejected
    (math-semantic-check-status (math-polynomial-check-rule-spec false-cancel))))

(let* ((checked (φ:check-chain '("x+0" "x")
                 #:assumptions '("scalar(x)") #:steps '(add-zero)))
       (derivation (math-verification-derivation checked)))
(test-assert "independent checker validates a concrete polynomial rewrite"
  (math-semantic-check-valid? (math-polynomial-check-derivation derivation)))
(test-assert "independent checker validates conditional cancellation with its premise"
  (math-semantic-check-valid?
    (φ:check-polynomial-step "(x*a)/a" "x" 'cancel-common-factor '()
      #:assumptions "scalar(x); scalar(a); nonzero(a)")))
(test-equal "independent checker rejects conditional cancellation without its premise"
  'rejected
  (math-semantic-check-status
    (φ:check-polynomial-step "(x*a)/a" "x" 'cancel-common-factor '()
      #:assumptions "scalar(x); scalar(a)")))
  (test-assert "semantic checker rejects a forged local conclusion"
    (not (math-semantic-check-valid?
           (φ:check-polynomial-step "x+0" "x+1" 'add-zero '()
             #:assumptions '("scalar(x)")))))
  (test-assert "semantic checker rejects a forged path"
    (not (math-semantic-check-valid?
           (φ:check-polynomial-step "x+0" "x" 'add-zero '(0)
             #:assumptions '("scalar(x)")))))
  (test-assert "scalar law cannot certify a vector rewrite"
    (not (math-semantic-check-valid?
           (φ:check-polynomial-step "E+0" "E" 'add-zero '()
             #:assumptions '("vector(E)"))))))

(test-assert "check-only algebra law supports handwritten verification"
  (let* ((checked (φ:check-chain '("x+y" "y+x")
                   #:assumptions '("scalar(x); scalar(y)")
                   #:steps '(add-commutative)))
         (derivation (math-verification-derivation checked)))
    (and (math-verification-valid? checked)
         (math-semantic-check-valid?
           (math-polynomial-check-derivation derivation)))))
  (test-error "check-only algebra laws cannot be selected for rewrite search" #t
  (math-selected-rules '(add-commutative) math-default-rules #:mode 'forward))
(test-assert "derivation checker rejects forged certificate endpoints"
  (not
    (math-semantic-check-valid?
      (math-polynomial-check-derivation
        (make-math-derivation
          (list (φ:term "x+0") (φ:term "x"))
          (list (make-math-derivation-step
                  (φ:term "x+0") (φ:term "y") 'add-zero '()))
          '((scalar (var x))))))))

(test-assert "derivative checker validates the expected polynomial result"
  (math-semantic-check-valid?
    (φ:check-polynomial-derivative "x^3+2*x" 'x "3*x^2+2")))
(test-assert "derivative checker accepts a Formula variable expression"
  (math-semantic-check-valid?
    (φ:check-polynomial-derivative "x^3+2*x" "x" "3*x^2+2")))
(test-assert "derivative checker rejects an incorrect result"
  (not (math-semantic-check-valid?
         (φ:check-polynomial-derivative "x^3+2*x" "x" "x^2+2"))))
(test-assert "derivative checker handles independent variables"
  (math-semantic-check-valid?
    (φ:check-polynomial-derivative "x^2+y" "x" "2*x")))
(test-assert "derivative certificate replays sourced steps and checks final denotation"
  (let* ((report (φ:differentiate "x^3+2*x" "x"
                   #:assumptions '("coordinate(x)")))
         (derivation (math-report-derivation report)))
    (and (math-report-complete? report)
         (every math-derivation-step? (math-derivation-steps derivation))
         (math-semantic-check-valid?
           (φ:check-polynomial-derivation derivation)))))
(test-assert "derivative certificate rejects a forged final value"
  (let* ((report (φ:differentiate "x^3+2*x" "x"
                   #:assumptions '("coordinate(x)")))
         (derivation (math-report-derivation report))
         (terms (math-derivation-terms derivation))
         (forged (make-math-derivation
                   (append (drop-right terms 1) (list (φ:term "x^2")))
                   (math-derivation-steps derivation)
                   (math-derivation-assumptions derivation))))
    (not (math-semantic-check-valid? (φ:check-polynomial-derivation forged)))))
(test-equal "non-polynomial differentiation is explicitly unsupported"
  'unsupported
  (math-semantic-check-status
    (φ:check-polynomial-derivative "sin(x)" "x" "cos(x)")))
(test-assert "check summaries lower to ordinary block content"
  (content-element?
    (φ:polynomial-check->content
      (φ:check-polynomial-rule 'add-zero))))
(test-assert "a universal-law summary identifies the rule it checked"
  (string-contains
    (sxml->html
      (content->sxml (φ:polynomial-check->content
                       (φ:check-polynomial-rule 'add-zero))))
    "Rule: add-zero"))

(test-end "independent scalar-polynomial semantic checker")
