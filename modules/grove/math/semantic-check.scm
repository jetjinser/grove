;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math semantic-check)
  #:use-module (grove math derivation)
  #:use-module (grove math number)
  #:use-module (grove math premise)
  #:use-module (grove math rule)
  #:use-module (grove math rewrite)
  #:use-module (grove math verification)
  #:use-module (grove math term)
  #:use-module (grove math types)
  #:use-module (grove math polynomial contract)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (math-semantic-check? math-semantic-check-status
            math-semantic-check-valid? math-semantic-check-subject
            math-semantic-check-diagnostics
            math-polynomial-normal-form math-polynomial-equivalent?
            math-polynomial-check-rule math-polynomial-check-rule-spec
            math-polynomial-check-step
            math-polynomial-check-derivation math-polynomial-check-derivative
            math-polynomial-check-antiderivative
            math-check-derivative-evidence math-check-antiderivative-evidence))

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

;; Rational expressions keep numerator, denominator and their explicit domain
;; obligations separate. Equality is checked by cross multiplication only
;; after every symbolic denominator is justified by a declared nonzero fact.
(define (fraction numerator denominator conditions)
  (list numerator denominator (delete-duplicates conditions equal?)))

(define (fraction-add left right sign)
  (fraction
    (polynomial-add
      (polynomial-multiply (car left) (cadr right))
      (polynomial-scale
        (polynomial-multiply (car right) (cadr left)) sign))
    (polynomial-multiply (cadr left) (cadr right))
    (append (caddr left) (caddr right))))

(define (parse-rational term)
  (let ((polynomial (parse-polynomial term)))
    (cond
      (polynomial (fraction polynomial (constant-polynomial 1) '()))
      (else
       (match term
         (('- operand)
          (let ((operand (parse-rational operand)))
            (and operand (fraction (polynomial-scale (car operand) -1)
                                   (cadr operand) (caddr operand)))))
         (('+ left right)
          (let ((left (parse-rational left)) (right (parse-rational right)))
            (and left right (fraction-add left right 1))))
         (('- left right)
          (let ((left (parse-rational left)) (right (parse-rational right)))
            (and left right (fraction-add left right -1))))
         (('* left right)
          (let ((left (parse-rational left)) (right (parse-rational right)))
            (and left right
                 (fraction (polynomial-multiply (car left) (car right))
                   (polynomial-multiply (cadr left) (cadr right))
                   (append (caddr left) (caddr right))))))
         (('/ numerator denominator-term)
          (let ((numerator (parse-rational numerator))
                (denominator (parse-rational denominator-term)))
            (and numerator denominator (not (null? (car denominator)))
                 (let* ((constant-polynomial
                          (math-polynomial-normal-form denominator-term))
                        (constant (and constant-polynomial
                                       (polynomial-constant-value constant-polynomial)))
                        (denominator-conditions
                          (if (and constant (not (zero? constant))) '()
                              (list denominator-term))))
                   (fraction (polynomial-multiply (car numerator) (cadr denominator))
                     (polynomial-multiply (cadr numerator) (car denominator))
                     (append (caddr numerator) (caddr denominator)
                             denominator-conditions))))))
         (('^ base ('num exponent-text))
          (let ((base (parse-rational base))
                (exponent (math-natural-value exponent-text)))
            (and base exponent (<= exponent 128)
                 (fraction (polynomial-power (car base) exponent)
                   (polynomial-power (cadr base) exponent) (caddr base)))))
         (_ #f))))))

(define (math-rational-normal-form term)
  (catch 'semantic-polynomial-limit
    (lambda () (and (math-term? term) (parse-rational term)))
    (lambda _ #f)))

(define (nonzero-obligation-covered? spec obligation)
  (let ((obligation (math-polynomial-normal-form obligation)))
    (and obligation
      (any
        (lambda (premise)
          (and (pair? premise) (memq (car premise) '(nonzero positive))
               (= (length premise) 2)
               (let ((premise-term (math-polynomial-normal-form
                                     (schema-term (cadr premise)))))
                 (and premise-term (equal? obligation premise-term)))))
        (math-rule-spec-premises spec)))))

(define (check-rational-law spec left right)
  (let ((left (math-rational-normal-form left))
        (right (math-rational-normal-form right)))
    (cond
      ((not (and left right))
       (semantic-check 'unsupported spec
         "the rule is outside the rational-polynomial checker"))
      ((not (every (lambda (condition)
                     (nonzero-obligation-covered? spec condition))
                   (append (caddr left) (caddr right))))
       (semantic-check 'rejected spec
         "the rule omits a nonzero premise required by its denominators"))
      ((equal? (polynomial-multiply (car left) (cadr right))
               (polynomial-multiply (car right) (cadr left)))
       (semantic-check 'semantically-checked spec #f))
      (else
       (semantic-check 'rejected spec
         "the rule changes its rational-polynomial denotation")))))

(define (semantic-check status subject message)
  (make-math-semantic-check status subject (if message (list message) '())))

(define (meta-template? term)
  (match term
    (('unquote name) (symbol? name))
    ((head . tail) (and (meta-template? head) (meta-template? tail)))
    (_ #t)))

(define (schema-variable name)
  (string->symbol (string-append "__grove_schema_" (symbol->string name))))

(define (spec-zero-metavariables spec)
  (define (zeroo-guard? goal)
    (match goal
      (('zeroo _ (? symbol?)) #t)
      ((head . tail) (or (zeroo-guard? head) (zeroo-guard? tail)))
      (_ #f)))
  (define (collect goal)
    (match goal
      (('zeroo _ (? symbol? name)) (list name))
      ((head . tail) (append (collect head) (collect tail)))
      (_ '())))
  (if (math-rule-spec? spec)
      (collect (math-rule-spec-guards spec))
      '()))

(define* (schema-term term #:optional (zero-variables '()))
  (match term
    (('unquote name)
     (if (memq name zero-variables) '(num "0")
         `(var ,(schema-variable name))))
    ((head . tail)
     (cons (schema-term head zero-variables) (schema-term tail zero-variables)))
    ((? symbol? name)
     (if (memq name zero-variables) '(num "0") term))
    (_ term)))

(define (check-rule-spec subject spec)
  (let ((law (list (math-rule-spec-left spec) (math-rule-spec-right spec))))
    (cond
      ((not (every meta-template? law))
       (semantic-check 'rejected subject "malformed rule specification"))
      (else
       (let* ((zero-variables (spec-zero-metavariables spec))
              (left (math-polynomial-normal-form
                      (schema-term (car law) zero-variables)))
              (right (math-polynomial-normal-form
                       (schema-term (cadr law) zero-variables))))
         (cond
           ((or (not left) (not right))
            (let ((rational-check
                    (check-rational-law spec
                      (schema-term (car law) zero-variables)
                      (schema-term (cadr law) zero-variables))))
              (make-math-semantic-check
                (math-semantic-check-status rational-check) subject
                (math-semantic-check-diagnostics rational-check))))
           ((equal? left right)
            (semantic-check 'semantically-checked subject #f))
           (else
            (semantic-check 'rejected subject
              "the rule specification is not an identity in the rational polynomial model"))))))))

(define (math-polynomial-check-rule-spec spec)
  "Check a RuleSpec as a universal law in Grove's exact scalar theory."
  (unless (math-rule-spec? spec) (error "expected a RuleSpec" spec))
  (check-rule-spec spec spec))

(define (math-polynomial-check-rule name)
  "Check a registered rule's RuleSpec as a universal law."
  (let ((rule (math-rewrite-rule-ref name)))
    (cond ((not rule) (semantic-check 'unsupported name "unknown rewrite rule"))
          ((not (math-rewrite-rule-spec rule))
           (semantic-check 'unsupported name "the rule has no declarative specification"))
          (else (check-rule-spec name (math-rewrite-rule-spec rule))))))

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
         (spec (and rule (math-rewrite-rule-spec rule)))
         (law (and spec (list (math-rule-spec-left spec)
                              (math-rule-spec-right spec))))
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
       (let* ((bindings (math-rule-spec-match spec local-from))
              (expected (and bindings
                             (math-rule-spec-instantiate spec (cadr law) bindings)))
              (premises (and bindings
                             (math-rule-spec-instantiate
                               spec (math-rule-spec-premises spec) bindings)))
              (from-rational (math-rational-normal-form from))
              (to-rational (math-rational-normal-form to)))
         (cond
           ((not bindings)
            (semantic-check 'rejected (list from to rule-name path)
              "the local input does not match the rule schema"))
           ((not (and (equal? expected local-to)
                      (equal? (replace-at-path from path expected) to)))
            (semantic-check 'rejected (list from to rule-name path)
              "the recorded step is not exactly the declared local replacement"))
           ((not (every (lambda (premise)
                          (math-premise-known? assumptions premise))
                        premises))
            (semantic-check 'rejected (list from to rule-name path)
              "the rewrite application is missing a declared rule premise"))
           ((or (not from-rational) (not to-rational))
            (semantic-check 'unsupported (list from to rule-name path)
              "the complete expressions are outside the rational-polynomial checker"))
           ((not (every (lambda (condition)
                          (math-premise-known? assumptions `(nonzero ,condition)))
                        (append (caddr from-rational) (caddr to-rational))))
            (semantic-check 'rejected (list from to rule-name path)
              "the complete expressions require an unproved nonzero denominator"))
           ((equal? (polynomial-multiply (car from-rational) (cadr to-rational))
                    (polynomial-multiply (car to-rational) (cadr from-rational)))
            (semantic-check 'semantically-checked (list from to rule-name path) #f))
           (else
            (semantic-check 'rejected (list from to rule-name path)
              "the rewrite changes the rational-polynomial denotation"))))))))

(define (check-polynomial-rewrite-derivation certificate terms steps assumptions)
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
         (append-map math-semantic-check-diagnostics checks))))))

(define (check-polynomial-derivative-derivation certificate expression variable terms)
  (let* ((replay (math-verify-derivation certificate))
         (evidence (polynomial-derivative-evidence expression variable))
         (check (and evidence (math-check-derivative-evidence evidence)))
         (actual (math-polynomial-normal-form (last terms))))
    (cond
      ((not (math-verification-valid? replay))
       (semantic-check 'rejected certificate
         (string-join (math-verification-errors replay) "; ")))
      ((or (not evidence) (not actual))
       (semantic-check 'unsupported certificate
         "the derivative chain is outside the scalar-polynomial checker"))
      ((and (math-semantic-check-valid? check)
            (equal? actual (math-derivative-evidence-result evidence)))
       (semantic-check 'semantically-checked evidence #f))
      (else
       (semantic-check 'rejected certificate
         "the replayed derivative chain has an incorrect polynomial result")))))

(define (math-polynomial-check-derivation certificate)
  "Check a rewrite chain, or replay and independently check a polynomial derivative."
  (unless (math-derivation? certificate)
    (error "expected a math derivation" certificate))
  (let ((terms (math-derivation-terms certificate))
        (steps (math-derivation-steps certificate))
        (assumptions (math-derivation-assumptions certificate)))
    (cond
      ((not (and (list? terms) (pair? terms) (list? steps)
                 (list? assumptions)
                 (= (length terms) (+ 1 (length steps)))))
       (semantic-check 'rejected certificate "malformed derivation chain"))
      ((match (car terms) (('differentiate _ ('var _)) #t) (_ #f))
       (match (car terms)
         (('differentiate expression ('var variable))
          (check-polynomial-derivative-derivation certificate expression variable terms))))
      ((match (car terms) (('antiderivative _ ('var _)) #t) (_ #f))
       (match (car terms)
         (('antiderivative expression ('var variable))
          (check-polynomial-antiderivative-derivation certificate expression variable terms))))
      (else
       (check-polynomial-rewrite-derivation certificate terms steps assumptions)))))

(define (polynomial-derivative-evidence term variable)
  "Construct case-shaped evidence for the recursive polynomial derivative."
  (match term
    (('num text)
     (let ((value (math-natural-value text)))
       (and value (make-math-derivative-evidence 'constant term variable '() '()))))
    (('var name)
     (let ((result (constant-polynomial (if (eq? name variable) 1 0))))
       (make-math-derivative-evidence
         (if (eq? name variable) 'coordinate 'independent-variable)
         term variable result '())))
    (('- operand)
     (let ((child (polynomial-derivative-evidence operand variable)))
       (and child
            (let ((result (polynomial-scale (math-derivative-evidence-result child) -1)))
              (make-math-derivative-evidence 'negation term variable result (list child))))))
    (((or '+ '-) left right)
     (let ((left-proof (polynomial-derivative-evidence left variable))
           (right-proof (polynomial-derivative-evidence right variable)))
       (and left-proof right-proof
            (let ((result (polynomial-add (math-derivative-evidence-result left-proof)
                            (polynomial-scale (math-derivative-evidence-result right-proof)
                              (if (eq? (car term) '+) 1 -1)))))
              (make-math-derivative-evidence
                (if (eq? (car term) '+) 'sum 'difference)
                term variable result (list left-proof right-proof))))))
    (('* left right)
     (let* ((left-value (parse-polynomial left))
            (right-value (parse-polynomial right))
            (left-proof (polynomial-derivative-evidence left variable))
            (right-proof (polynomial-derivative-evidence right variable)))
       (and left-value right-value left-proof right-proof
            (let ((result
                    (polynomial-add
                      (polynomial-multiply (math-derivative-evidence-result left-proof) right-value)
                      (polynomial-multiply left-value (math-derivative-evidence-result right-proof)))))
              (make-math-derivative-evidence 'product term variable result
                (list left-proof right-proof))))))
    (('/ numerator denominator)
     (let* ((value (parse-polynomial denominator))
            (constant (and value (polynomial-constant-value value)))
            (numerator-proof (polynomial-derivative-evidence numerator variable)))
       (and constant (not (zero? constant)) numerator-proof
            (let ((result (polynomial-scale
                            (math-derivative-evidence-result numerator-proof) (/ 1 constant))))
              (make-math-derivative-evidence 'constant-quotient term variable result
                (list numerator-proof))))))
    (('^ base ('num exponent-text))
     (let* ((exponent (math-natural-value exponent-text))
            (base-value (parse-polynomial base))
            (base-proof (polynomial-derivative-evidence base variable)))
       (and exponent (<= exponent 128) base-value base-proof
            (let ((result
                    (if (zero? exponent)
                        '()
                        (polynomial-scale
                          (polynomial-multiply
                            (polynomial-power base-value (- exponent 1))
                            (math-derivative-evidence-result base-proof))
                          exponent))))
              (make-math-derivative-evidence 'natural-power term variable result
                (list base-proof))))))
    (_ #f)))

(define (math-check-derivative-evidence evidence)
  "Independently check a derivative evidence tree, including every local case."
  (define (valid? evidence)
    (and (math-derivative-evidence? evidence)
         (symbol? (math-derivative-evidence-coordinate evidence))
         (list? (math-derivative-evidence-children evidence))
         (let* ((term (math-derivative-evidence-expression evidence))
                (coordinate (math-derivative-evidence-coordinate evidence))
                (result (math-derivative-evidence-result evidence))
                (children (math-derivative-evidence-children evidence)))
           (and (list? result)
                (equal? result (canonical-polynomial result))
                (match (list (math-derivative-evidence-case evidence) term children)
                  (('constant ('num text) '())
                   (and (math-natural-value text) (null? result)))
                  (('coordinate ('var name) '())
                   (and (eq? name coordinate)
                        (equal? result (constant-polynomial 1))))
                  (('independent-variable ('var name) '())
                   (and (not (eq? name coordinate)) (null? result)))
                  (('negation ('- operand) (child))
                   (and (valid? child)
                        (equal? (math-derivative-evidence-expression child) operand)
                        (eq? (math-derivative-evidence-coordinate child) coordinate)
                        (equal? result
                          (polynomial-scale (math-derivative-evidence-result child) -1))))
                  (('sum ('+ left right) (left-proof right-proof))
                   (and (valid? left-proof) (valid? right-proof)
                        (equal? (math-derivative-evidence-expression left-proof) left)
                        (equal? (math-derivative-evidence-expression right-proof) right)
                        (eq? (math-derivative-evidence-coordinate left-proof) coordinate)
                        (eq? (math-derivative-evidence-coordinate right-proof) coordinate)
                        (equal? result
                          (polynomial-add (math-derivative-evidence-result left-proof)
                            (math-derivative-evidence-result right-proof)))))
                  (('difference ('- left right) (left-proof right-proof))
                   (and (valid? left-proof) (valid? right-proof)
                        (equal? (math-derivative-evidence-expression left-proof) left)
                        (equal? (math-derivative-evidence-expression right-proof) right)
                        (eq? (math-derivative-evidence-coordinate left-proof) coordinate)
                        (eq? (math-derivative-evidence-coordinate right-proof) coordinate)
                        (equal? result
                          (polynomial-add (math-derivative-evidence-result left-proof)
                            (polynomial-scale (math-derivative-evidence-result right-proof) -1)))))
                  (('product ('* left right) (left-proof right-proof))
                   (and (valid? left-proof) (valid? right-proof)
                        (equal? (math-derivative-evidence-expression left-proof) left)
                        (equal? (math-derivative-evidence-expression right-proof) right)
                        (eq? (math-derivative-evidence-coordinate left-proof) coordinate)
                        (eq? (math-derivative-evidence-coordinate right-proof) coordinate)
                        (let ((left-value (parse-polynomial left))
                              (right-value (parse-polynomial right)))
                          (and left-value right-value
                            (equal? result
                              (polynomial-add
                                (polynomial-multiply (math-derivative-evidence-result left-proof) right-value)
                                (polynomial-multiply left-value (math-derivative-evidence-result right-proof))))))))
                  (('constant-quotient ('/ numerator denominator) (child))
                   (and (valid? child)
                        (equal? (math-derivative-evidence-expression child) numerator)
                        (eq? (math-derivative-evidence-coordinate child) coordinate)
                        (let* ((denominator-value (parse-polynomial denominator))
                               (constant (and denominator-value (polynomial-constant-value denominator-value))))
                          (and constant (not (zero? constant))
                               (equal? result
                                 (polynomial-scale (math-derivative-evidence-result child) (/ 1 constant)))))))
                  (('natural-power ('^ base ('num exponent-text)) (child))
                   (and (valid? child)
                        (equal? (math-derivative-evidence-expression child) base)
                        (eq? (math-derivative-evidence-coordinate child) coordinate)
                        (let ((exponent (math-natural-value exponent-text))
                              (base-value (parse-polynomial base)))
                          (and exponent (<= exponent 128) base-value
                            (equal? result
                              (if (zero? exponent) '()
                                  (polynomial-scale
                                    (polynomial-multiply (polynomial-power base-value (- exponent 1))
                                      (math-derivative-evidence-result child)) exponent)))))))
                  (_ #f))))))
  (if (catch #t (lambda () (valid? evidence)) (lambda _ #f))
      (semantic-check 'semantically-checked evidence #f)
      (semantic-check 'rejected evidence "malformed or invalid derivative contract evidence")))

(define (math-polynomial-check-derivative expression variable result)
  "Check a derivative by recursively interpreting its source constructors."
  (let* ((evidence (and (symbol? variable)
                        (polynomial-derivative-evidence expression variable)))
         (check (and evidence (math-check-derivative-evidence evidence)))
        (output (math-polynomial-normal-form result)))
    (cond
      ((not (symbol? variable))
       (semantic-check 'rejected (list expression variable result)
         "the differentiation variable must be a symbol"))
      ((or (not evidence) (not output))
       (semantic-check 'unsupported (list expression variable result)
         "the input or result is outside the scalar-polynomial checker"))
      ((and (math-semantic-check-valid? check)
            (equal? output (math-derivative-evidence-result evidence)))
       (semantic-check 'semantically-checked evidence #f))
      (else
       (semantic-check 'rejected (list expression variable result)
         "the supplied result differs from the exact polynomial derivative")))))

(define (integrate-monomial entry variable)
  (let* ((monomial (car entry))
         (factor (assq variable monomial))
         (exponent (if factor (cdr factor) 0))
         (raised (sort
                   (cons (cons variable (+ exponent 1))
                     (filter (lambda (power) (not (eq? (car power) variable))) monomial))
                   (lambda (left right)
                     (string<? (symbol->string (car left))
                               (symbol->string (car right))))))
         (result (cons raised (/ (cdr entry) (+ exponent 1)))))
    (make-math-integral-monomial-evidence entry result)))

(define (polynomial-antiderivative-evidence expression variable result)
  "Construct typed per-monomial evidence for a proposed primitive."
  (let ((input (math-polynomial-normal-form expression))
        (output (math-polynomial-normal-form result)))
    (and input output
      (make-math-antiderivative-evidence
        expression variable input output
        (map (lambda (entry) (integrate-monomial entry variable)) input)
        (polynomial-derivative-evidence result variable)))))

(define (valid-integral-monomial-evidence? variable entry step)
  (catch #t
    (lambda ()
      (and (math-integral-monomial-evidence? step)
           (equal? entry (math-integral-monomial-evidence-input step))
           (let* ((factor (assq variable (car entry)))
                  (exponent (if factor (cdr factor) 0))
                  (powers
                    (sort
                      (cons (cons variable (+ exponent 1))
                        (filter (lambda (power) (not (eq? (car power) variable)))
                          (car entry)))
                      (lambda (left right)
                        (string<? (symbol->string (car left))
                                  (symbol->string (car right))))))
                  (expected (cons powers (/ (cdr entry) (+ exponent 1)))))
             (equal? (math-integral-monomial-evidence-output step) expected))))
    (lambda _ #f)))

(define (check-antiderivative-evidence/internal evidence)
  "Check monomial recurrence, zero-origin normalization, and derivative evidence."
  (let* ((expression (and (math-antiderivative-evidence? evidence)
                          (math-antiderivative-evidence-expression evidence)))
         (variable (and (math-antiderivative-evidence? evidence)
                        (math-antiderivative-evidence-coordinate evidence)))
         (input (and expression (math-polynomial-normal-form expression)))
         (claimed-input (and (math-antiderivative-evidence? evidence)
                             (math-antiderivative-evidence-input evidence)))
         (result (and (math-antiderivative-evidence? evidence)
                      (math-antiderivative-evidence-result evidence)))
         (monomials (and (math-antiderivative-evidence? evidence)
                         (math-antiderivative-evidence-monomials evidence)))
         (derivative (and (math-antiderivative-evidence? evidence)
                          (math-antiderivative-evidence-derivative evidence))))
    (cond
      ((not (math-antiderivative-evidence? evidence))
       (semantic-check 'rejected evidence "expected polynomial antiderivative evidence"))
      ((not (and (symbol? variable) input (list? result) (list? monomials)
                 (equal? claimed-input input)
                 (equal? result (canonical-polynomial result))))
       (semantic-check 'rejected evidence "malformed or unsupported integration evidence"))
      ((not (= (length input) (length monomials)))
       (semantic-check 'rejected evidence "integration evidence omits or adds a monomial"))
      ((not (every (lambda (entry step)
                     (valid-integral-monomial-evidence? variable entry step))
                   input monomials))
       (semantic-check 'rejected evidence "a monomial evidence node has an invalid coefficient or exponent"))
      ((not (every (lambda (entry) (assq variable (car entry))) result))
       (semantic-check 'rejected evidence
         "the primitive is not normalized to zero at the coordinate origin"))
      ((not (equal? result
              (canonical-polynomial
                (map math-integral-monomial-evidence-output monomials))))
       (semantic-check 'rejected evidence "the claimed primitive differs from the monomial results"))
      ((not (and (math-derivative-evidence? derivative)
                 (equal? (parse-polynomial
                           (math-derivative-evidence-expression derivative))
                   result)
                 (eq? (math-derivative-evidence-coordinate derivative) variable)
                 (math-semantic-check-valid? (math-check-derivative-evidence derivative))
                 (equal? (math-derivative-evidence-result derivative) input)))
       (semantic-check 'rejected evidence "differentiating the primitive evidence does not recover the input"))
      (else
       (semantic-check 'semantically-checked evidence #f)))))

(define (math-check-antiderivative-evidence evidence)
  "Independently check typed integration evidence; malformed data is rejected."
  (catch #t
    (lambda () (check-antiderivative-evidence/internal evidence))
    (lambda _
      (semantic-check 'rejected evidence "malformed polynomial integration contract evidence"))))

(define (math-polynomial-check-antiderivative expression variable result)
  "Check the termwise polynomial integration contract and its normalization."
  (let* ((evidence (and (symbol? variable)
                        (catch 'semantic-polynomial-limit
                          (lambda () (polynomial-antiderivative-evidence expression variable result))
                          (lambda _ #f))))
         (input (math-polynomial-normal-form expression))
         (actual (math-polynomial-normal-form result)))
    (cond
      ((not (symbol? variable))
       (semantic-check 'rejected (list expression variable result)
         "the integration coordinate must be a symbol"))
      ((or (not evidence) (not input) (not actual))
       (semantic-check 'unsupported (list expression variable result)
         "the input or result is outside the scalar-polynomial integration checker"))
      (else (math-check-antiderivative-evidence evidence)))))

(define (check-polynomial-antiderivative-derivation certificate expression variable terms)
  (let ((replay (math-verify-derivation certificate))
        (check (math-polynomial-check-antiderivative expression variable (last terms))))
    (cond
      ((not (math-verification-valid? replay))
       (semantic-check 'rejected certificate
         (string-join (math-verification-errors replay) "; ")))
      ((math-semantic-check-valid? check)
       (semantic-check 'semantically-checked (math-semantic-check-subject check) #f))
      (else check))))
