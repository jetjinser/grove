;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove ir)
  #:use-module ((grove math semantic) #:prefix semantic:)
  #:use-module (grove math expression)
  #:use-module (grove math term)
  #:use-module (grove math context)
  #:use-module (grove math rewrite)
  #:use-module (grove math derivation)
  #:use-module (grove math private strategy)
  #:use-module (grove math verification)
  #:use-module (grove math semantic-check)
  #:use-module (grove math audit)
  #:use-module (grove math private differentiation)
  #:use-module (grove math partial)
  #:use-module (grove math private integration)
  #:use-module (grove math private polynomial)
  #:use-module (grove math private declaration)
  #:use-module (grove math private pass)
  #:use-module (grove math report)
  #:use-module (grove math diagnostic)
  #:use-module (grove math private source)
  #:use-module (grove math private provenance)
  #:use-module (grove math private presentation)
  #:use-module (grove math layout)
  #:use-module (grove math private premise)
  #:use-module (grove math private assumption)
  #:use-module (srfi srfi-1)
  #:export (math-derive
            math-search math-search-backward math-simplify math-calculate math-collect
            math-check-chain
            math-check-equation math-antiderivative math-solve-poisson math-compute math-differentiate math-partial math-rewrite math-report-result math-normalize math-expand)
  #:re-export (math-derivation->content math-verification->content
               math-report->content math-completed-report->content
               math-semantic-check->content current-math-assumptions call-with-math-assumptions with-math-assumptions
               math-audit-calculation math-calculation-audit? math-calculation-audit-replay
               math-calculation-audit-replay-valid? math-calculation-audit-replay-errors
               math-calculation-audit-contract math-calculation-audit-contract-status
               math-calculation-audit-contract-diagnostics math-calculation-audit-proof
               math-calculation-audit-proof-status math-calculation-audit-proof-errors
               math-verify-step math-verify-derivation
               math-verification? math-verification-valid?
               math-verification-errors math-verification-diagnostics
               math-verification-derivation math-diagnostic?
               math-diagnostic-step math-diagnostic-rule math-diagnostic-path
               math-diagnostic-code math-diagnostic-premises math-diagnostic-message
               math-diagnostic-source math-diagnostic-origin math-origin? math-origin-location
               math-origin-locations math-origin-generated? math-origin-rule
               math-location? math-location-file math-location-line
               math-location-column math-location-excerpt
               math-derivation-step? math-derivation-step-from
               math-derivation-step-to math-derivation-step-rule
               math-derivation-step-path math-derivation?
               math-derivation-terms math-derivation-steps
               math-derivation-assumptions math-derivation-sources math-derivation-requirements
               math-semantic-check? math-semantic-check-status
               math-semantic-check-valid? math-semantic-check-subject
               math-semantic-check-diagnostics
               math-polynomial-normal-form math-polynomial-equivalent?
               math-polynomial-check-rule math-polynomial-check-step
               math-polynomial-check-derivation math-polynomial-check-derivative
               math-polynomial-check-antiderivative
               math-check-derivative-evidence math-check-antiderivative-evidence
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
               math-report? math-report-candidates
               math-report-reason math-report-limits
               math-report-states math-report-complete? math-report-status math-report-unresolved math-report-name
               math-report-diagnostics math-report-passes math-report-derivation math-report-final
               math-report-audit))

(define (value->term who value)
  (unless (math-value? value)
    (error who "expected a Semantic value" value))
  (math-value-term value))

(define (derivation-premises candidate)
  (append-map
    (lambda (step)
      (or (math-rule-required-facts (math-derivation-step-rule step)
            (math-term-at-path (math-derivation-step-from step) (math-derivation-step-path step))) '()))
    (math-derivation-steps candidate)))

(define* (report-with-input report input #:optional (other-inputs '()) (assumptions #f))
  (let* ((values (map (lambda (value) (begin (value->term 'math-report value) value))
                   (cons input other-inputs)))
         (requirements (delete-duplicates (append-map math-value-requirements values) equal?))
         (inherited (append-map (lambda (value)
                                 (lset-difference equal? (math-value-requirements value)
                                   (math-domain-premises (math-value-term value)))) values)))
    (define (attach report)
      (let* ((certificate (math-report-derivation report))
             (facts (if certificate (math-derivation-assumptions certificate) (or assumptions (current-math-assumptions))))
             (requirements (delete-duplicates
                             (append requirements
                               (append-map (lambda (value) (math-context-premises facts (math-value-term value))) values)) equal?))
             (missing (math-requirement-diagnostics facts inherited)))
        (make-math-report (math-report-name report)
          (if (pair? missing)
              (list (make-math-derivation (list (math-value-term (car values))) '() facts #:requirements requirements))
              (map (lambda (c)
                     (make-math-derivation (math-derivation-terms c) (math-derivation-steps c)
                       (math-derivation-assumptions c) #:sources (math-derivation-sources c)
                       #:requirements (delete-duplicates
                                        (append requirements (math-derivation-requirements c) (derivation-premises c)) equal?)))
                   (math-report-candidates report)))
          (if (pair? missing) 'missing-premise (math-report-reason report))
          (math-report-limits report) (if (pair? missing) 0 (math-report-states report))
          #:diagnostics (append missing (math-report-diagnostics report))
          #:passes (if (pair? missing) '() (map attach (math-report-passes report))))))
    (math-report-with-origin (attach report) (math-value-origin (car values)))))

(define (math-report-result report)
  (unless (math-report? report)
    (error "expected a math report"))
  (let ((term (math-report-final report)))
    (and (math-report-complete? report) term (make-math-value term
                #:requirements (math-derivation-requirements (math-report-derivation report))
                #:origin (let ((sources (math-derivation-sources (math-report-derivation report))))
                           (and (pair? sources) (last sources)))
                #:properties `((assumptions . ,(math-derivation-assumptions (math-report-derivation report))))))))

(define (audit-calculus-report report)
  (let ((certificate (math-report-derivation report)))
    (if certificate
        (make-math-report
          (math-report-name report)
          (math-report-candidates report)
          (math-report-reason report)
          (math-report-limits report)
          (math-report-states report)
          #:diagnostics (math-report-diagnostics report)
          #:passes (math-report-passes report)
          #:audit (math-audit-calculation certificate))
        report)))

(define (derivative-report name rules source variable assumptions validate max-steps max-term-size)
  (let* ((expression (value->term name source))
         (coordinate (value->term name variable))
         (facts (math-assumptions assumptions))
         (diagnostics (validate expression coordinate facts))
         (input `(,name ,expression ,coordinate)))
    (report-with-input
      (if (null? diagnostics)
          (run-math-passes input facts
            `((,name . ,rules)
              (simplify . ,math-simplification-rules))
            #:name name #:max-steps max-steps #:max-term-size max-term-size)
          (run-math-pass input facts #:name name #:rules '()
            #:max-steps max-steps #:max-term-size max-term-size
            #:validate (lambda (term) diagnostics)))
      (make-math-value input
        #:requirements (math-value-requirements source)
        #:origin (math-branch-origin (list (math-value-origin source)
                                          (math-value-origin variable)))))))

(define* (math-differentiate source variable #:key (assumptions '())
                              (max-steps 128) (max-term-size 2048))
  (audit-calculus-report
    (derivative-report 'differentiate math-differentiation-rules source variable assumptions
      differentiation-input-diagnostics max-steps max-term-size)))

(define* (math-partial source variable #:key (assumptions '())
                      (max-steps 128) (max-term-size 2048))
  "Compute a scalar partial derivative, then simplify in a separate pass.
Declared unknown function derivatives remain explicit partial terms."
  (derivative-report 'partial math-partial-rules source variable assumptions
    (lambda (expression coordinate facts) '()) max-steps max-term-size))

(define* (math-rewrite source #:key (assumptions '()) (rules #f)
                        (max-steps 32) (max-term-size 2048))
  (report-with-input
    (run-math-pass (value->term 'math-rewrite source) (math-assumptions assumptions)
      #:name 'rewrite #:rules rules #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-derive source #:key (assumptions '()) (rules #f)
                       (max-steps 32) (max-term-size 2048))
  (let* ((report (math-rewrite source #:assumptions assumptions #:rules rules
                   #:max-steps max-steps #:max-term-size max-term-size))
         (report (if (and (math-report-complete? report)
                          (null? (math-derivation-steps (math-report-derivation report))))
                     (let* ((certificate (math-report-derivation report))
                            (facts (math-derivation-assumptions certificate))
                            (input (car (math-derivation-terms certificate)))
                            (selected (math-selected-rules rules math-default-rules))
                            (diagnostics
                              (append-map
                                (lambda (path)
                                  (let ((local (math-term-at-path input path)))
                                    (filter-map
                                      (lambda (rule)
                                        (let* ((required (math-rule-required-facts rule local))
                                               (missing (and required (remove (lambda (fact) (math-premise-known? facts fact)) required))))
                                          (and missing (pair? missing)
                                               (null? (math-assumption-diagnostics (append facts missing)))
                                               (pair? (math-rewrite-answers (append facts missing) local (list rule) #f))
                                               (make-math-diagnostic #f rule path 'missing-premise missing
                                                 (format #f "missing premises for ~a: ~s" rule (map math-term->source missing))))))
                                      selected)))
                                (math-term-paths input))))
                       (report-with-input
                         (make-math-report 'rewrite (math-report-candidates report)
                           (if (pair? diagnostics) 'missing-premise 'no-rule) '() 0
                           #:diagnostics (if (pair? diagnostics) diagnostics
                                           (list (make-math-diagnostic #f #f '() 'no-rule '()
                                                   "no selected math rewrite rule applies under the supplied premises"))))
                         source))
                     report)))
    report))

(define* (math-search source target
                      #:key (assumptions '()) (rules #f)
                            (max-depth 8) (max-results 5)
                            (max-states 256) (max-term-size 128))
  (report-with-input
    (search-math-terms
      (value->term 'math-search source) (value->term 'math-search target)
      (math-assumptions assumptions) #f
      #:start-origin (math-value-origin source)
      #:goal-origin (math-value-origin target)
      #:rules rules #:max-depth max-depth #:max-results max-results
      #:max-states max-states #:max-term-size max-term-size)
    source (list target) (math-assumptions assumptions)))

(define* (math-search-backward target source
                               #:key (assumptions '()) (rules #f)
                                     (max-depth 8) (max-results 5)
                                     (max-states 256) (max-term-size 128))
  (report-with-input
    (search-math-terms
      (value->term 'math-search-backward target) (value->term 'math-search-backward source)
      (math-assumptions assumptions) #t
      #:start-origin (math-value-origin target)
      #:goal-origin (math-value-origin source)
      #:rules rules #:max-depth max-depth #:max-results max-results
      #:max-states max-states #:max-term-size max-term-size)
    source (list target) (math-assumptions assumptions)))

(define* (math-simplify source #:key (assumptions '()) (rules #f) (max-steps 32) (max-term-size 2048))
  (report-with-input
    (simplify-math-term (value->term 'math-simplify source)
      (math-assumptions assumptions) #:rules rules #:max-steps max-steps
      #:max-term-size max-term-size)
    source))

(define* (math-calculate source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (report-with-input
    (calculate-math-term (value->term 'math-calculate source)
      (math-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-normalize source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (report-with-input
    (run-math-pass (value->term 'math-normalize source) (math-assumptions assumptions)
      #:name 'normalize #:rules '(normalize-scalar)
      #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-expand source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (report-with-input
    (run-math-passes (value->term 'math-expand source) (math-assumptions assumptions)
      `((expand . (expand-scalar-product)) (simplify . ,math-simplification-rules))
      #:name 'expand #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-collect source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (report-with-input
    (collect-math-term (value->term 'math-collect source)
      (math-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-check-chain sources #:key (assumptions '()) (steps #f) (rules #f))
  (unless (and (list? sources) (every math-value? sources))
    (error "handwritten math chains require a list of Semantic values" sources))
  (check-math-chain (map math-value-term sources)
    (math-assumptions assumptions) #:steps steps #:rules rules
    #:requirements (delete-duplicates (append-map math-value-requirements sources) equal?)
    #:sources (map math-value-origin sources)))

(define (cartesian-passes term)
  (append
    (if (any (lambda (path) (memq (car (math-term-at-path term path)) '(antiderivative poisson)))
             (math-term-paths term))
        '((polynomial-calculus . (integrate-polynomial solve-polynomial-poisson))) '())
    `((components . ,(math-rule-names #:groups '(components spatial)))
      (partial . ,(append math-simplification-rules math-partial-rules)))))

(define* (math-compute source #:key (assumptions '()) (max-steps 256) (max-term-size 8192))
  "Lower Cartesian operators and component algebra, compute partials, then simplify.
Unknown field derivatives remain explicit; no abstract field components are invented."
  (report-with-input
    (run-math-passes (value->term 'math-compute source) (math-assumptions assumptions)
      (append (cartesian-passes (value->term 'math-compute source))
              `((simplify . ,math-simplification-rules)))
      #:name 'compute #:order 'innermost #:max-steps max-steps #:max-term-size max-term-size
      #:validate-final
      (lambda (term)
        (filter-map
          (lambda (path)
            (let ((local (math-term-at-path term path)))
              (and (memq (car local) '(grad div curl laplacian antiderivative poisson))
                   (make-math-diagnostic #f #f path 'unsupported-expression '()
                     "Cartesian computation requires explicit vector components for this operator"))))
          (math-term-paths term))))
    source))

(define (polynomial-calculus-report name rule source coordinate assumptions max-steps max-term-size)
  (derivative-report name (list rule) source coordinate assumptions
    (lambda (expression coordinate facts)
      (let ((preconditions (math-execution-diagnostics facts `(,name ,expression ,coordinate) name)))
        (if (pair? preconditions) preconditions
            (polynomial-calculus-diagnostics facts `(,name ,expression ,coordinate)))))
    max-steps max-term-size))

(define* (math-antiderivative source coordinate #:key (assumptions '())
                             (max-steps 128) (max-term-size 8192))
  "Compute the exact polynomial primitive anchored at coordinate zero."
  (audit-calculus-report
    (polynomial-calculus-report 'antiderivative 'integrate-polynomial source coordinate assumptions max-steps max-term-size)))

(define* (math-solve-poisson source #:key (coordinate #f) (assumptions '())
                           (max-steps 128) (max-term-size 8192))
  "Construct a polynomial particular solution of Delta u = source.
No boundary conditions or uniqueness claim. The selected axis fixes one solution."
  (let* ((facts (math-assumptions assumptions))
         (basis (find math-space-declaration? facts))
         (axis (or coordinate
                   (semantic:variable (if basis (cadadr basis) 'x)))))
    (polynomial-calculus-report 'poisson 'solve-polynomial-poisson source axis assumptions max-steps max-term-size)))

(define (equation-residual-diagnostics term facts)
  (if (member term '((num "0") (zero vector) (components (num "0") (num "0") (num "0")))) '()
      (catch 'polynomial-limit
        (lambda ()
          (math-term->polynomial term facts)
          (list (make-math-diagnostic #f #f
                  (or (find (lambda (path)
                              (memq (car (math-term-at-path term path)) '(differentiate partial prime darg)))
                            (math-term-paths term)) '()) 'equation-not-proven '()
                  "The equation has a nonzero or unresolved residual; equality was not established")))
        (lambda (key message)
          (list (make-math-diagnostic #f #f '() 'polynomial-limit '() message))))))

(define* (math-check-equation left right #:key (assumptions '())
                             (max-steps 256) (max-term-size 8192))
  "Check an identity by computing and expanding its residual.
A nonzero or symbolic residual is not a completed verification."
  (value->term 'math-check-equation left)
  (value->term 'math-check-equation right)
  (let* ((input (semantic:subtract left right))
         (facts (math-assumptions assumptions)))
    (report-with-input
      (run-math-passes (math-value-term input) facts
        (append (cartesian-passes (math-value-term input))
                `((polynomial . (expand-polynomial))
                  (simplify . ,math-simplification-rules)))
        #:name 'check-equation #:order 'innermost #:max-steps max-steps #:max-term-size max-term-size
        #:validate-final
        (lambda (term) (equation-residual-diagnostics term facts)))
      input)))
