;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math semantic-check)
  #:use-module (grove math derivation)
  #:use-module (grove math number)
  #:use-module (grove math rewrite)
  #:use-module (grove math term)
  #:use-module (grove math types)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (math-semantic-check? math-semantic-check-status
            math-semantic-check-valid? math-semantic-check-subject
            math-semantic-check-diagnostics
            math-polynomial-normal-form math-polynomial-equivalent?
            math-polynomial-check-rule math-polynomial-check-step
            math-polynomial-check-derivation math-polynomial-check-derivative))

;; A polynomial is a canonical list of (monomial . exact-rational) pairs.
;; Monomials are sorted lists of (variable . natural-power) pairs.
(define max-polynomial-monomials 256)
(define max-polynomial-input-terms (* max-polynomial-monomials max-polynomial-monomials))
(define max-polynomial-degree 64)

(define-record-type <math-semantic-check>
  (make-math-semantic-check status subject diagnostics)
  math-semantic-check?
  (status math-semantic-check-status)
  (subject math-semantic-check-subject)
  (diagnostics math-semantic-check-diagnostics))

(define (math-semantic-check-valid? check)
  (and (math-semantic-check? check)
       (eq? (math-semantic-check-status check) 'semantically-checked)))

(define (monomial<? left right)
  (string<? (format #f "~s" left) (format #f "~s" right)))

(define (canonical-polynomial terms)
  (when (> (length terms) max-polynomial-input-terms)
    (throw 'semantic-polynomial-limit))
  (let* ((sorted (sort terms
                  (lambda (left right)
                    (monomial<? (car left) (car right)))))
         (combined
           (let combine ((remaining sorted) (monomial #f) (coefficient 0) (result '()))
             (cond
               ((null? remaining)
                (if (and monomial (not (zero? coefficient)))
                    (cons (cons monomial coefficient) result)
                    result))
               ((and monomial (equal? monomial (caar remaining)))
                (combine (cdr remaining) monomial
                         (+ coefficient (cdar remaining)) result))
               (else
                (combine (cdr remaining) (caar remaining) (cdar remaining)
                  (if (and monomial (not (zero? coefficient)))
                      (cons (cons monomial coefficient) result)
                      result))))))
         (normal (reverse combined)))
    (when (or (> (length normal) max-polynomial-monomials)
              (any (lambda (term)
                     (> (apply + (map cdr (car term))) max-polynomial-degree))
                   normal))
      (throw 'semantic-polynomial-limit))
    normal))

(define (constant-polynomial value)
  (if (zero? value) '() (list (cons '() value))))

(define (polynomial-add left right)
  (canonical-polynomial (append left right)))

(define (polynomial-scale polynomial value)
  (canonical-polynomial
    (map (lambda (term) (cons (car term) (* value (cdr term)))) polynomial)))

(define (monomial-multiply left right)
  (sort
    (fold (lambda (factor result)
            (let ((old (assq (car factor) result)))
              (if old
                  (map (lambda (entry)
                         (if (eq? entry old)
                             (cons (car factor) (+ (cdr entry) (cdr factor)))
                             entry))
                       result)
                  (cons factor result))))
          '() (append left right))
    (lambda (left right) (string<? (symbol->string (car left))
                                   (symbol->string (car right))))))

(define (polynomial-multiply left right)
  (when (> (* (length left) (length right)) max-polynomial-input-terms)
    (throw 'semantic-polynomial-limit))
  (canonical-polynomial
    (append-map
      (lambda (left-term)
        (map (lambda (right-term)
               (cons (monomial-multiply (car left-term) (car right-term))
                     (* (cdr left-term) (cdr right-term))))
             right))
      left)))

(define (polynomial-power polynomial exponent)
  (let loop ((n exponent) (result (constant-polynomial 1)))
    (if (zero? n) result
        (loop (- n 1) (polynomial-multiply result polynomial)))))

(define (polynomial-constant-value polynomial)
  (and (or (null? polynomial)
           (and (= (length polynomial) 1) (null? (caar polynomial))))
       (if (null? polynomial) 0 (cdar polynomial))))

(define (parse-polynomial term)
  (match term
    (('var name)
     (list (cons (list (cons name 1)) 1)))
    (('num text)
     (let ((number (math-natural-value text)))
       (and number (constant-polynomial number))))
    (('- operand)
     (let ((operand (parse-polynomial operand)))
       (and operand (polynomial-scale operand -1))))
    (('+ left right)
     (let ((left (parse-polynomial left)) (right (parse-polynomial right)))
       (and left right (polynomial-add left right))))
    (('- left right)
     (let ((left (parse-polynomial left)) (right (parse-polynomial right)))
       (and left right
            (polynomial-add left (polynomial-scale right -1)))))
    (('* left right)
     (let ((left (parse-polynomial left)) (right (parse-polynomial right)))
       (and left right (polynomial-multiply left right))))
    (('/ numerator denominator)
     (let* ((numerator (parse-polynomial numerator))
            (denominator (parse-polynomial denominator))
            (value (and denominator (polynomial-constant-value denominator))))
       (and numerator value (not (zero? value))
            (polynomial-scale numerator (/ 1 value)))))
    (('^ base ('num exponent-text))
     (let ((base (parse-polynomial base))
           (exponent (math-natural-value exponent-text)))
       (and base exponent (<= exponent 128)
            (polynomial-power base exponent))))
    (_ #f)))

(define (math-polynomial-normal-form term)
  "Return an exact sparse-polynomial normal form, or #f outside its domain.

The accepted language is scalar polynomials over the rationals: variables,
integer and rational constants, addition, subtraction, multiplication, and
natural powers. Division is accepted only by a nonzero rational constant."
  (catch 'semantic-polynomial-limit
    (lambda () (and (math-term? term) (parse-polynomial term)))
    (lambda _ #f)))

(define (math-polynomial-equivalent? left right)
  (let ((left (math-polynomial-normal-form left))
        (right (math-polynomial-normal-form right)))
    (and left right (equal? left right))))

(define (semantic-check status subject message)
  (make-math-semantic-check status subject (if message (list message) '())))

(define (meta-template? term)
  (match term
    (('meta (? symbol?)) #t)
    ((head . tail) (and (meta-template? head) (meta-template? tail)))
    (_ #t)))

(define (schema-variable name)
  (string->symbol (string-append "__grove_schema_" (symbol->string name))))

(define (schema-term term)
  (match term
    (('meta name) `(var ,(schema-variable name)))
    ((head . tail) (cons (schema-term head) (schema-term tail)))
    (_ term)))

(define* (math-polynomial-check-rule name)
  "Check a registered scalar-polynomial law for all metavariable instances."
  (let* ((rule (math-rewrite-rule-ref name))
         (law (and rule (math-rewrite-rule-semantic-law rule))))
    (cond
      ((not rule)
       (semantic-check 'unsupported name "unknown rewrite rule"))
      ((not law)
       (semantic-check 'unsupported name
         "the rule has no declared scalar-polynomial semantic law"))
      ((not (every meta-template? law))
       (semantic-check 'rejected name "malformed semantic-law template"))
      (else
       (let ((left (math-polynomial-normal-form (schema-term (car law))))
             (right (math-polynomial-normal-form (schema-term (cadr law)))))
         (cond
           ((or (not left) (not right))
            (semantic-check 'unsupported name
              "the declared law is outside the scalar-polynomial checker"))
           ((equal? left right)
            (semantic-check 'semantically-checked name #f))
           (else
            (semantic-check 'rejected name
              "the declared law is not an identity in the rational polynomial model"))))))))

(define (match-law pattern term bindings)
  (match pattern
    (('meta name)
     (let ((previous (assq name bindings)))
       (if previous
           (and (equal? (cdr previous) term) bindings)
           (cons (cons name term) bindings))))
    ((head . tail)
     (and (pair? term)
          (let ((bindings (match-law head (car term) bindings)))
            (and bindings (match-law tail (cdr term) bindings)))))
    (_ (and (equal? pattern term) bindings))))

(define (instantiate-law template bindings)
  (match template
    (('meta name) (assq-ref bindings name))
    ((head . tail) (cons (instantiate-law head bindings)
                         (instantiate-law tail bindings)))
    (_ template)))

(define (scalar-term? assumptions term)
  (let ((inferred (math-infer-type assumptions term)))
    (and inferred (eq? (math-typed-term-type inferred) 'scalar))))

(define (replace-at-path term path replacement)
  (if (null? path) replacement
      (and (pair? term)
           (let ((index (car path)) (operands (cdr term)))
             (and (<= 0 index) (< index (length operands))
                  (let loop ((operands operands) (position 0) (result '()))
                    (if (null? operands)
                        (cons (car term) (reverse result))
                        (loop (cdr operands) (+ position 1)
                          (cons (if (= position index)
                                    (replace-at-path (car operands) (cdr path) replacement)
                                    (car operands))
                                result)))))))))

(define* (math-polynomial-check-step assumptions from to rule-name path)
  "Independently check a scalar-polynomial instance and its whole-term result."
  (let* ((law-check (math-polynomial-check-rule rule-name))
         (rule (math-rewrite-rule-ref rule-name))
         (law (and rule (math-rewrite-rule-semantic-law rule)))
         (local-from (math-term-at-path from path))
         (local-to (math-term-at-path to path)))
    (cond
      ((not (math-semantic-check-valid? law-check))
       (semantic-check (math-semantic-check-status law-check)
         (list from to rule-name path)
         (string-join (math-semantic-check-diagnostics law-check) "; ")))
      ((or (not local-from) (not local-to))
       (semantic-check 'rejected (list from to rule-name path)
         "the recorded operand path does not exist in both terms"))
      ((or (not (scalar-term? assumptions from))
           (not (scalar-term? assumptions to)))
       (semantic-check 'rejected (list from to rule-name path)
         "the whole rewrite is not a declared scalar expression"))
      (else
       (let* ((bindings (match-law (car law) local-from '()))
              (expected (and bindings (instantiate-law (cadr law) bindings)))
              (from-polynomial (math-polynomial-normal-form from))
              (to-polynomial (math-polynomial-normal-form to)))
         (cond
           ((not bindings)
            (semantic-check 'rejected (list from to rule-name path)
              "the local input does not match the declared semantic law"))
           ((not (and (equal? expected local-to)
                      (equal? (replace-at-path from path expected) to)))
            (semantic-check 'rejected (list from to rule-name path)
              "the recorded step is not exactly the declared local replacement"))
           ((or (not from-polynomial) (not to-polynomial))
            (semantic-check 'unsupported (list from to rule-name path)
              "the complete expressions are outside the scalar-polynomial checker"))
           ((equal? from-polynomial to-polynomial)
            (semantic-check 'semantically-checked (list from to rule-name path) #f))
           (else
            (semantic-check 'rejected (list from to rule-name path)
              "the rewrite changes the rational-polynomial denotation"))))))))

(define (math-polynomial-check-derivation certificate)
  "Check every recorded step without invoking its rewrite relation."
  (unless (math-derivation? certificate)
    (error "expected a math derivation" certificate))
  (let ((terms (math-derivation-terms certificate))
        (steps (math-derivation-steps certificate))
        (assumptions (math-derivation-assumptions certificate)))
    (if (not (and (list? terms) (pair? terms) (list? steps)
                  (list? assumptions)
                  (= (length terms) (+ 1 (length steps)))))
        (semantic-check 'rejected certificate "malformed derivation chain")
        (let ((checks
                (map (lambda (step from to)
                       (if (not (math-derivation-step? step))
                           (semantic-check 'rejected step "malformed derivation step")
                           (if (not (and (equal? from (math-derivation-step-from step))
                                         (equal? to (math-derivation-step-to step))))
                               (semantic-check 'rejected step
                                 "recorded step endpoints do not match the chain")
                               (math-polynomial-check-step assumptions from to
                                 (math-derivation-step-rule step)
                                 (math-derivation-step-path step)))))
                     steps (drop-right terms 1) (cdr terms))))
          (cond
            ((every math-semantic-check-valid? checks)
             (semantic-check 'semantically-checked certificate #f))
            ((any (lambda (check)
                    (eq? (math-semantic-check-status check) 'rejected)) checks)
             (make-math-semantic-check 'rejected certificate
               (append-map math-semantic-check-diagnostics checks)))
            (else
             (make-math-semantic-check 'unsupported certificate
               (append-map math-semantic-check-diagnostics checks))))))))

(define (differentiate-polynomial polynomial variable)
  (canonical-polynomial
    (filter-map
      (lambda (entry)
        (let ((power (or (assq variable (car entry)) #f)))
          (and power
               (cons (if (= (cdr power) 1)
                         (delq power (car entry))
                         (map (lambda (factor)
                                (if (eq? factor power)
                                    (cons variable (- (cdr factor) 1))
                                    factor))
                              (car entry)))
                     (* (cdr entry) (cdr power))))))
      polynomial)))

(define (math-polynomial-check-derivative expression variable result)
  "Check one derivative result against an independent exact polynomial derivative."
  (let ((input (math-polynomial-normal-form expression))
        (output (math-polynomial-normal-form result)))
    (cond
      ((not (symbol? variable))
       (semantic-check 'rejected (list expression variable result)
         "the differentiation variable must be a symbol"))
      ((or (not input) (not output))
       (semantic-check 'unsupported (list expression variable result)
         "the input or result is outside the scalar-polynomial checker"))
      ((equal? output (differentiate-polynomial input variable))
       (semantic-check 'semantically-checked (list expression variable result) #f))
      (else
       (semantic-check 'rejected (list expression variable result)
         "the supplied result differs from the exact polynomial derivative")))))
