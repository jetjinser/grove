;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math semantic-check)
  #:use-module (grove private snapshot)
  #:use-module (grove math derivation)
  #:use-module (grove math private checker-polynomial)
  #:use-module (grove math private declaration)
  #:use-module (grove math private number)
  #:use-module (grove math private premise)
  #:use-module (grove math rule)
  #:use-module (grove math rewrite)
  #:use-module (grove math verification)
  #:use-module (grove math term)
  #:use-module (grove math types)
  #:use-module (grove math private polynomial-contract)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:re-export (math-polynomial-normal-form math-polynomial-equivalent?
               math-derivative-evidence? make-math-derivative-evidence
               math-derivative-evidence-case math-derivative-evidence-expression
               math-derivative-evidence-coordinate math-derivative-evidence-result
               math-derivative-evidence-children
               math-integral-monomial-evidence? make-math-integral-monomial-evidence
               math-integral-monomial-evidence-input math-integral-monomial-evidence-output
               math-antiderivative-evidence? make-math-antiderivative-evidence
               math-antiderivative-evidence-expression math-antiderivative-evidence-coordinate
               math-antiderivative-evidence-input math-antiderivative-evidence-result
               math-antiderivative-evidence-monomials math-antiderivative-evidence-derivative
               math-poisson-evidence? make-math-poisson-evidence
               math-poisson-evidence-expression math-poisson-evidence-axes
               math-poisson-evidence-coordinate math-poisson-evidence-input
               math-poisson-evidence-result
               math-equation-evidence? make-math-equation-evidence
               math-equation-evidence-left math-equation-evidence-right
               math-equation-evidence-left-normal-form
               math-equation-evidence-right-normal-form)
  #:export (math-semantic-check? math-semantic-check-status
            math-semantic-check-valid? math-semantic-check-subject
            math-semantic-check-diagnostics
            math-polynomial-check-rule math-polynomial-check-rule-spec
            math-polynomial-check-step
            math-polynomial-check-derivation math-polynomial-check-derivative
            math-polynomial-check-antiderivative math-polynomial-check-poisson
            math-polynomial-check-equation math-polynomial-check-cartesian
            math-check-derivative-evidence math-check-antiderivative-evidence
            math-check-poisson-evidence math-check-equation-evidence))

(define-record-type <math-semantic-check>
  (%make-math-semantic-check status subject diagnostics)
  math-semantic-check?
  (status math-semantic-check-status)
  (subject semantic-check-subject/raw)
  (diagnostics semantic-check-diagnostics/raw))

(define (math-semantic-check-subject check)
  (snapshot-data (semantic-check-subject/raw check)))

(define (math-semantic-check-diagnostics check)
  (snapshot-data (semantic-check-diagnostics/raw check)))

(define (make-math-semantic-check status subject diagnostics)
  (%make-math-semantic-check status (snapshot-data subject)
                             (snapshot-data diagnostics)))

(define (math-semantic-check-valid? check)
  (and (math-semantic-check? check)
       (eq? (math-semantic-check-status check) 'semantically-checked)))

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
                      (equal? (math-term-replace-at-path from path expected) to)))
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
  "Check a rewrite chain, or replay and independently check scalar calculus contracts."
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
      ((match (car terms) (('partial _ ('var _)) #t) (_ #f))
       (match (car terms)
         (('partial expression ('var variable))
          (check-polynomial-derivative-derivation certificate expression variable terms))))
      ((match (car terms) (('antiderivative _ ('var _)) #t) (_ #f))
       (match (car terms)
         (('antiderivative expression ('var variable))
          (check-polynomial-antiderivative-derivation certificate expression variable terms))))
      ((match (car terms) (('poisson _ ('var _)) #t) (_ #f))
       (match (car terms)
         (('poisson expression ('var coordinate))
          (let* ((basis (find math-space-declaration? assumptions))
                 (axes (and basis (map cadr (cdr basis))))
                 (replay (math-verify-derivation certificate))
                 (check (and axes
                             (math-polynomial-check-poisson
                               expression axes coordinate (last terms)))))
            (cond
              ((not (math-verification-valid? replay))
               (semantic-check 'rejected certificate
                 (string-join (math-verification-errors replay) "; ")))
              ((not check)
               (semantic-check 'unsupported certificate
                 "a Cartesian space is required to audit a polynomial Poisson solution"))
              ((math-semantic-check-valid? check)
               (semantic-check 'semantically-checked
                 (math-semantic-check-subject check) #f))
              (else check))))))
      ((cartesian-expression? (car terms))
       (check-cartesian-derivation certificate (car terms) (last terms)
                                   assumptions))
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

(define (polynomial-normal-form->term polynomial)
  (define (product terms)
    (if (null? terms) '(num "1")
        (fold (lambda (term result) `(* ,result ,term))
              (car terms) (cdr terms))))
  (define (monomial->term entry)
    (let* ((powers (car entry))
           (coefficient (cdr entry))
           (factors
             (map (lambda (factor)
                    (let ((variable `(var ,(car factor))))
                      (if (= (cdr factor) 1) variable
                          `(^ ,variable (num ,(number->string (cdr factor)))))))
                  powers))
           (coefficient-term (math-number->term coefficient)))
      (cond ((null? factors) coefficient-term)
            ((= coefficient 1) (product factors))
            ((= coefficient -1) `(- ,(product factors)))
            (else (product (cons coefficient-term factors))))))
  (if (null? polynomial) '(num "0")
      (fold (lambda (entry result) `(+ ,result ,(monomial->term entry)))
            (monomial->term (car polynomial)) (cdr polynomial))))

(define (polynomial-at-zero polynomial coordinate)
  (canonical-polynomial
    (filter (lambda (entry) (not (assq coordinate (car entry)))) polynomial)))

(define (poisson-laplacian result axes)
  (let loop ((axes axes) (sum '()))
    (if (null? axes) sum
        (let* ((first-term (polynomial-derivative-evidence result (car axes)))
               (first-check (and first-term (math-check-derivative-evidence first-term)))
               (second-source
                 (and (math-semantic-check-valid? first-check)
                      (polynomial-normal-form->term
                        (math-derivative-evidence-result first-term))))
               (second-term
                 (and second-source
                      (polynomial-derivative-evidence second-source (car axes))))
               (second-check
                 (and second-term (math-check-derivative-evidence second-term))))
          (and (math-semantic-check-valid? second-check)
               (loop (cdr axes)
                 (polynomial-add sum
                   (math-derivative-evidence-result second-term))))))))

(define (check-poisson-evidence/internal evidence)
  (let* ((expression (and (math-poisson-evidence? evidence)
                          (math-poisson-evidence-expression evidence)))
         (axes (and (math-poisson-evidence? evidence)
                    (math-poisson-evidence-axes evidence)))
         (coordinate (and (math-poisson-evidence? evidence)
                          (math-poisson-evidence-coordinate evidence)))
         (claimed-input (and (math-poisson-evidence? evidence)
                             (math-poisson-evidence-input evidence)))
         (claimed-result (and (math-poisson-evidence? evidence)
                              (math-poisson-evidence-result evidence)))
         (input (and expression (math-polynomial-normal-form expression)))
         (result (and (list? claimed-result)
                      (math-polynomial-normal-form
                        (polynomial-normal-form->term claimed-result)))))
    (cond
      ((not (math-poisson-evidence? evidence))
       (semantic-check 'rejected evidence "expected polynomial Poisson evidence"))
      ((not (and (list? axes) (= (length axes) 3)
                 (every symbol? axes)
                 (= (length (delete-duplicates axes)) 3)
                 (memq coordinate axes)
                 input result
                 (equal? claimed-input input)
                 (equal? claimed-result result)))
       (semantic-check 'rejected evidence "malformed polynomial Poisson evidence"))
      (else
       (let ((laplacian (poisson-laplacian
                          (polynomial-normal-form->term result) axes))
             (first-derivative
               (polynomial-derivative-evidence
                 (polynomial-normal-form->term result) coordinate)))
         (cond
           ((or (not laplacian) (not first-derivative)
                (not (math-semantic-check-valid?
                       (math-check-derivative-evidence first-derivative))))
            (semantic-check 'unsupported evidence
              "the particular solution is outside the exact polynomial checker"))
           ((not (equal? input laplacian))
            (semantic-check 'rejected evidence
              "the particular solution does not satisfy the polynomial Poisson equation"))
           ((or (not (null? (polynomial-at-zero result coordinate)))
                (not (null? (polynomial-at-zero
                              (math-derivative-evidence-result first-derivative)
                              coordinate))))
            (semantic-check 'rejected evidence
              "the particular solution violates the selected-axis zero-value or zero-derivative anchor"))
           (else (semantic-check 'semantically-checked evidence #f))))))))

(define (math-check-poisson-evidence evidence)
  "Independently check a polynomial Poisson equation and its selected-axis anchor."
  (catch #t
    (lambda () (check-poisson-evidence/internal evidence))
    (lambda _
      (semantic-check 'rejected evidence "malformed polynomial Poisson contract evidence"))))

(define (math-polynomial-check-poisson expression axes coordinate result)
  "Check a polynomial particular solution of Delta u = EXPRESSION.
AXES gives the ordered Cartesian coordinates; COORDINATE selects the
zero-value and zero-first-derivative anchor used to choose one solution."
  (let* ((input (math-polynomial-normal-form expression))
         (output (math-polynomial-normal-form result)))
    (cond
      ((not (and (list? axes) (= (length axes) 3)
                 (every symbol? axes)
                 (= (length (delete-duplicates axes)) 3)
                 (symbol? coordinate) (memq coordinate axes)))
       (semantic-check 'rejected (list expression axes coordinate result)
         "Poisson evidence requires three distinct axes and a selected axis in that space"))
      ((or (not input) (not output))
       (semantic-check 'unsupported (list expression axes coordinate result)
         "the input or solution is outside the scalar-polynomial Poisson checker"))
      (else
       (math-check-poisson-evidence
         (make-math-poisson-evidence expression axes coordinate input output))))))

(define (check-equation-evidence/internal evidence)
  (let* ((left (and (math-equation-evidence? evidence)
                    (math-equation-evidence-left evidence)))
         (right (and (math-equation-evidence? evidence)
                     (math-equation-evidence-right evidence)))
         (left-normal-form (and left (math-polynomial-normal-form left)))
         (right-normal-form (and right (math-polynomial-normal-form right)))
         (claimed-left (and (math-equation-evidence? evidence)
                            (math-equation-evidence-left-normal-form evidence)))
         (claimed-right (and (math-equation-evidence? evidence)
                             (math-equation-evidence-right-normal-form evidence))))
    (cond
      ((not (math-equation-evidence? evidence))
       (semantic-check 'rejected evidence "expected polynomial equation evidence"))
      ((not (and left-normal-form right-normal-form
                 (equal? claimed-left left-normal-form)
                 (equal? claimed-right right-normal-form)))
       (semantic-check 'rejected evidence "malformed polynomial equation evidence"))
      ((equal? left-normal-form right-normal-form)
       (semantic-check 'semantically-checked evidence #f))
      (else
       (semantic-check 'rejected evidence
         "the two expressions have different exact polynomial meanings")))))

(define (math-check-equation-evidence evidence)
  "Independently verify the exact polynomial meanings recorded for an equation."
  (catch #t
    (lambda () (check-equation-evidence/internal evidence))
    (lambda _
      (semantic-check 'rejected evidence "malformed polynomial equation evidence"))))

(define (math-polynomial-check-equation left right)
  "Check whether two supported scalar polynomial expressions are identical."
  (let ((left-normal-form (math-polynomial-normal-form left))
        (right-normal-form (math-polynomial-normal-form right)))
    (cond
      ((or (not left-normal-form) (not right-normal-form))
       (semantic-check 'unsupported (list left right)
         "one or both sides are outside the exact scalar-polynomial checker"))
      (else
       (math-check-equation-evidence
         (make-math-equation-evidence
           left right left-normal-form right-normal-form))))))

;; The Cartesian checker interprets exact scalar polynomials and a bounded set
;; of explicit vector operations independently of the spatial rewrite rules.
(define cartesian-expression-node-limit 4096)
(define cartesian-expression-depth-limit 128)
(define cartesian-number-digit-limit 256)
(define cartesian-symbol-character-limit 256)
(define cartesian-coefficient-bit-limit 4096)

(define (cartesian-symbol-within-limits? symbol)
  (and (symbol? symbol)
       (<= (string-length (symbol->string symbol))
           cartesian-symbol-character-limit)))

(define (cartesian-term-within-limits? term)
  "Validate a term with bounded traversal before recursive interpretation."
  (let ((remaining cartesian-expression-node-limit))
    (define (walk-children children depth)
      (let loop ((children children))
        (cond ((null? children) #t)
              ((not (pair? children)) #f)
              ((and (walk (car children) depth)
                    (loop (cdr children))) #t)
              (else #f))))
    (define (walk term depth)
      (set! remaining (- remaining 1))
      (and (>= remaining 0)
           (<= depth cartesian-expression-depth-limit)
           (match term
             (('var (? cartesian-symbol-within-limits?)) #t)
             (('num text)
              (and (string? text)
                   (<= (string-length text) cartesian-number-digit-limit)))
             (('zero 'vector) #t)
             ((operator . operands)
              (and (cartesian-symbol-within-limits? operator)
                   (walk-children operands (+ depth 1))))
             (_ #f))))
    (walk term 0)))

(define (polynomial-partial polynomial variable)
  (canonical-polynomial
    (filter-map
      (lambda (entry)
        (let* ((powers (car entry))
               (factor (assq variable powers)))
          (and factor
               (cons (map (lambda (power)
                            (if (eq? (car power) variable)
                                (cons variable (- (cdr power) 1))
                                power))
                          (filter (lambda (power)
                                    (or (not (eq? (car power) variable))
                                        (> (cdr power) 1)))
                            powers))
                     (* (cdr entry) (cdr factor))))))
      polynomial)))

(define (cartesian-components term axes)
  (match term
    (('components x y z)
     (let ((values
             (map (lambda (component)
                    (cartesian-value-scalar
                      (cartesian-evaluate component axes)))
               (list x y z))))
       (and (every identity values) values)))
    (_ #f)))

(define (cartesian-polynomial-within-limits? polynomial)
  (every
    (lambda (term)
      (let ((coefficient (cdr term)))
        (and (rational? coefficient) (exact? coefficient)
             (<= (integer-length (abs (numerator coefficient)))
                 cartesian-coefficient-bit-limit)
             (<= (integer-length (denominator coefficient))
                 cartesian-coefficient-bit-limit))))
    polynomial))

(define (cartesian-scalar polynomial)
  (and (cartesian-polynomial-within-limits? polynomial)
       (list 'scalar polynomial)))

(define (cartesian-vector components)
  (and (= (length components) 3)
       (every cartesian-polynomial-within-limits? components)
       (list 'vector components)))

(define (cartesian-polynomial-power polynomial exponent)
  (let loop ((exponent exponent) (base polynomial)
             (result (constant-polynomial 1)))
    (cond
      ((zero? exponent) result)
      ((odd? exponent)
       (let ((next (polynomial-multiply result base)))
         (and (cartesian-polynomial-within-limits? next)
              (if (= exponent 1)
                  next
                  (let ((square (polynomial-multiply base base)))
                    (and (cartesian-polynomial-within-limits? square)
                         (loop (quotient exponent 2) square next)))))))
      (else
       (let ((square (polynomial-multiply base base)))
         (and (cartesian-polynomial-within-limits? square)
              (loop (quotient exponent 2) square result)))))))

(define (cartesian-value-scalar value)
  (match value (('scalar polynomial) polynomial) (_ #f)))

(define (cartesian-value-vector value)
  (match value (('vector components) components) (_ #f)))

(define (cartesian-add left right sign)
  (let ((left-scalar (cartesian-value-scalar left))
        (right-scalar (cartesian-value-scalar right))
        (left-vector (cartesian-value-vector left))
        (right-vector (cartesian-value-vector right)))
    (cond
      ((and left-scalar right-scalar)
       (cartesian-scalar
         (polynomial-add left-scalar (polynomial-scale right-scalar sign))))
      ((and left-vector right-vector)
       (cartesian-vector
         (map (lambda (a b) (polynomial-add a (polynomial-scale b sign)))
           left-vector right-vector)))
      (else #f))))

(define (cartesian-vector-scale vector scalar divide?)
  (let ((components (cartesian-value-vector vector))
        (polynomial (cartesian-value-scalar scalar)))
    (and components polynomial
      (let ((factor
              (if divide?
                  (let ((constant (polynomial-constant-value polynomial)))
                    (and constant (not (zero? constant)) (/ 1 constant)))
                  (polynomial-constant-value polynomial))))
        (if divide?
            (and factor
                 (cartesian-vector
                   (map (lambda (component) (polynomial-scale component factor))
                     components)))
            (cartesian-vector
              (map (lambda (component)
                     (polynomial-multiply polynomial component))
                components)))))))

(define (cartesian-dot left right)
  (let ((left (cartesian-value-vector left))
        (right (cartesian-value-vector right)))
    (and left right
      (cartesian-scalar
        (fold polynomial-add '()
          (map polynomial-multiply left right))))))

(define (cartesian-cross left right)
  (let ((left (cartesian-value-vector left))
        (right (cartesian-value-vector right)))
    (and left right
      (let ((a (car left)) (b (cadr left)) (c (caddr left))
            (d (car right)) (e (cadr right)) (f (caddr right)))
        (cartesian-vector
          (list (polynomial-add (polynomial-multiply b f)
                                (polynomial-scale (polynomial-multiply c e) -1))
                (polynomial-add (polynomial-multiply c d)
                                (polynomial-scale (polynomial-multiply a f) -1))
                (polynomial-add (polynomial-multiply a e)
                                (polynomial-scale (polynomial-multiply b d) -1))))))))

(define (cartesian-curl components axes)
  (let ((x (car components)) (y (cadr components)) (z (caddr components))
        (a (car axes)) (b (cadr axes)) (c (caddr axes)))
    (list
      (polynomial-add (polynomial-partial z b)
                      (polynomial-scale (polynomial-partial y c) -1))
      (polynomial-add (polynomial-partial x c)
                      (polynomial-scale (polynomial-partial z a) -1))
      (polynomial-add (polynomial-partial y a)
                      (polynomial-scale (polynomial-partial x b) -1)))))

(define (cartesian-evaluate term axes)
  "Interpret exact Cartesian polynomial scalars and explicit vector operations."
  (match term
    (('components x y z)
     (let ((components (cartesian-components term axes)))
       (and components (cartesian-vector components))))
    (('zero 'vector)
     (cartesian-vector (list '() '() '())))
    (('- operand)
     (let ((value (cartesian-evaluate operand axes)))
       (or (and (cartesian-value-scalar value)
                (cartesian-scalar
                  (polynomial-scale (cartesian-value-scalar value) -1)))
           (and (cartesian-value-vector value)
                (cartesian-vector
                  (map (lambda (component) (polynomial-scale component -1))
                    (cartesian-value-vector value)))))))
    (('+ left right)
     (let ((left (cartesian-evaluate left axes))
           (right (cartesian-evaluate right axes)))
       (and left right (cartesian-add left right 1))))
    (('- left right)
     (let ((left (cartesian-evaluate left axes))
           (right (cartesian-evaluate right axes)))
       (and left right (cartesian-add left right -1))))
    (('* left right)
     (let ((left (cartesian-evaluate left axes))
           (right (cartesian-evaluate right axes)))
       (and left right
         (or (and (cartesian-value-scalar left)
                  (cartesian-value-scalar right)
                  (cartesian-scalar
                    (polynomial-multiply
                      (cartesian-value-scalar left)
                      (cartesian-value-scalar right))))
             (cartesian-vector-scale left right #f)
             (cartesian-vector-scale right left #f)))))
    (('/ numerator denominator)
     (let ((numerator (cartesian-evaluate numerator axes))
           (denominator (cartesian-evaluate denominator axes)))
       (and numerator denominator
         (or (and (cartesian-value-scalar numerator)
                  (cartesian-value-scalar denominator)
                  (let ((constant
                          (polynomial-constant-value
                            (cartesian-value-scalar denominator))))
                    (and constant (not (zero? constant))
                      (cartesian-scalar
                        (polynomial-scale
                          (cartesian-value-scalar numerator)
                          (/ 1 constant))))))
             (cartesian-vector-scale numerator denominator #t)))))
    (('^ base ('num exponent-text))
     (let* ((value (cartesian-evaluate base axes))
            (polynomial (cartesian-value-scalar value))
            (exponent (math-natural-value exponent-text)))
       (and polynomial exponent (<= exponent 128)
            (let ((result
                    (cartesian-polynomial-power polynomial exponent)))
              (and result (cartesian-scalar result))))))
    (('dot left right)
     (let ((left (cartesian-evaluate left axes))
           (right (cartesian-evaluate right axes)))
       (and left right (cartesian-dot left right))))
    (('cross left right)
     (let ((left (cartesian-evaluate left axes))
           (right (cartesian-evaluate right axes)))
       (and left right (cartesian-cross left right))))
    (('grad expression)
     (let* ((value (cartesian-evaluate expression axes))
            (scalar (cartesian-value-scalar value)))
       (and scalar
         (cartesian-vector
           (map (lambda (axis) (polynomial-partial scalar axis)) axes)))))
    (('div expression)
     (let* ((value (cartesian-evaluate expression axes))
            (vector (cartesian-value-vector value)))
       (and vector
         (cartesian-scalar
           (fold polynomial-add '()
             (map polynomial-partial vector axes))))))
    (('curl expression)
     (let* ((value (cartesian-evaluate expression axes))
            (vector (cartesian-value-vector value)))
       (and vector (cartesian-vector (cartesian-curl vector axes)))))
    (('laplacian expression)
     (let* ((value (cartesian-evaluate expression axes))
            (scalar (cartesian-value-scalar value))
            (vector (cartesian-value-vector value)))
       (cond (scalar (cartesian-scalar (polynomial-laplacian scalar axes)))
             (vector
              (cartesian-vector
                (map (lambda (component)
                       (polynomial-laplacian component axes))
                  vector)))
             (else #f))))
    (((or 'partial 'differentiate) expression ('var coordinate))
     (let* ((value (cartesian-evaluate expression axes))
            (scalar (cartesian-value-scalar value)))
       (and scalar (memq coordinate axes)
         (cartesian-scalar (polynomial-partial scalar coordinate)))))
    (_ (let ((polynomial (math-polynomial-normal-form term)))
         (and polynomial (cartesian-scalar polynomial))))))

(define (cartesian-expression? term)
  "Recognize Cartesian terms with bounded traversal; overflow routes to unsupported."
  (let ((remaining cartesian-expression-node-limit))
    (define (walk-children children depth)
      (let loop ((children children))
        (cond ((null? children) #f)
              ((not (pair? children)) #f)
              (else (or (walk (car children) depth)
                        (loop (cdr children)))))))
    (define (walk term depth)
      (set! remaining (- remaining 1))
      (cond
        ((or (< remaining 0) (> depth cartesian-expression-depth-limit)) #t)
        ((not (pair? term)) #f)
        ((and (symbol? (car term))
              (memq (car term)
                '(components zero grad div curl laplacian dot cross))) #t)
        ((and (symbol? (car term)) (pair? (cdr term)))
         (walk-children (cdr term) (+ depth 1)))
        (else #f)))
    (walk term 0)))

(define (polynomial-laplacian polynomial axes)
  (fold polynomial-add '()
    (append-map
      (lambda (axis)
        (let ((first (polynomial-partial polynomial axis)))
          (list (polynomial-partial first axis))))
      axes)))

(define (math-polynomial-check-cartesian expression axes result)
  "Check a composed scalar/vector expression over exact Cartesian polynomials.
AXES is the ordered list of three distinct coordinate symbols."
  (cond
    ((not (and (pair? axes) (pair? (cdr axes)) (pair? (cddr axes))
               (null? (cdddr axes))
               (every cartesian-symbol-within-limits? axes)
               (= (length (delete-duplicates axes)) 3)))
     (semantic-check 'rejected 'cartesian-axes
       "Cartesian checking requires three distinct coordinate symbols"))
    ((not (and (cartesian-term-within-limits? expression)
               (cartesian-term-within-limits? result)))
     (semantic-check 'unsupported 'cartesian-expression
       "the expression or result exceeds the Cartesian checker's structural limits"))
    (else
     (catch 'semantic-polynomial-limit
       (lambda ()
         (let ((input (cartesian-evaluate expression axes))
               (output (cartesian-evaluate result axes)))
           (cond
             ((or (not input) (not output))
              (semantic-check 'unsupported (list expression axes result)
                "the expression or result is outside the Cartesian polynomial checker"))
             ((equal? input output)
              (semantic-check 'semantically-checked
                (list expression axes result) #f))
             (else
              (semantic-check 'rejected (list expression axes result)
                "the supplied result differs from the exact Cartesian denotation")))))
       (lambda _
         (semantic-check 'unsupported (list expression axes result)
           "the exact Cartesian polynomial expansion exceeds its limits"))))))

(define (check-cartesian-derivation certificate expression result assumptions)
  (let* ((space (find math-space-declaration? assumptions))
         (axes (and space (map cadr (cdr space))))
         (replay (math-verify-derivation certificate)))
    (cond
      ((not (math-verification-valid? replay))
       (semantic-check 'rejected certificate
         (string-join (math-verification-errors replay) "; ")))
      ((not axes)
       (semantic-check 'unsupported certificate
         "a declared Cartesian space is required for this expression contract"))
      (else
       (let ((check (math-polynomial-check-cartesian expression axes result)))
         (if (math-semantic-check-valid? check)
             (semantic-check 'semantically-checked
               (list expression axes result) #f)
             check))))))
