;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math verification)
  #:use-module (grove math derivation)
  #:use-module (grove math diagnostic)
  #:use-module (grove math pass)
  #:use-module (grove math assumption)
  #:use-module (grove math premise)
  #:use-module (grove math term)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove math source)
  #:use-module (grove relation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (math-verify-step math-verify-derivation check-math-chain
            math-verification? math-verification-valid?
            math-verification-errors math-verification-diagnostics
            math-verification-derivation)
  #:re-export (math-diagnostic? math-diagnostic-step math-diagnostic-rule
            math-diagnostic-path math-diagnostic-code
            math-diagnostic-premises math-diagnostic-message math-diagnostic-source))

(define-record-type <math-verification>
  (make-math-verification derivation diagnostics)
  math-verification?
  (derivation math-verification-derivation)
  (diagnostics math-verification-diagnostics))

(define (math-verification-valid? verification)
  (and (math-verification? verification)
       (null? (math-verification-diagnostics verification))))

(define (math-verification-errors verification)
  (map math-diagnostic-description (math-verification-diagnostics verification)))

(define* (diagnostic index rule path code message #:optional (premises '()))
  (make-math-diagnostic index rule path code premises
    (format #f "~a~a: ~a"
            (if index (format #f "step ~a" index) "chain")
            (if rule (format #f ", ~a at ~a" rule (math-path-label path)) "")
            message)))

(define (step-diagnostics index assumptions from to rule path allowed)
  (let* ((metadata (math-rewrite-rule-ref rule))
         (local (math-term-at-path from path))
         (required (and local (math-rule-required-facts rule local))))
    (cond
      ((not rule)
       (list (diagnostic index #f path 'no-rule
               "no selected one-step rule relates these adjacent lines")))
      ((not metadata)
       (list (diagnostic index rule path 'unknown-rule "unknown rewrite rule")))
      ((not (memq rule allowed))
       (list (diagnostic index rule path 'disabled-rule "rule is excluded by #:rules")))
      ((not (math-term? to))
       (list (diagnostic index rule path 'invalid-term "target is not a valid math term")))
      ((not local)
       (list (diagnostic index rule path 'invalid-path "operand path does not exist")))
      ((not required)
       (list (diagnostic index rule path 'rule-shape "rule does not match this operand")))
      (else
       (let* ((missing (remove (lambda (fact) (math-premise-known? assumptions fact)) required))
              (from-type (math-infer-type assumptions from))
              (to-type (and (math-term? to) (math-infer-type assumptions to)))
              (conditions
                (append
                  (if (null? missing) '()
                      (list (diagnostic index rule path 'missing-premise
                              (string-append "missing premises: "
                                (string-join (map math-term->source missing) "; "))
                              missing)))
                  (if (and from-type to-type) '()
                      (list (diagnostic index rule path 'type-error
                              (string-join
                                (append
                                  (if from-type '()
                                      (list (string-append "source: "
                                              (math-type-diagnostic assumptions from))))
                                  (if to-type '()
                                      (list (string-append "target: "
                                              (math-type-diagnostic assumptions to)))))
                                "; "))))
                  (if (and from-type to-type
                           (not (eq? (math-typed-term-type from-type)
                                     (math-typed-term-type to-type))))
                      (list (diagnostic index rule path 'type-change
                              "source and target have different types")) '())
                  (if (math-rewrite-rule-condition-checked? metadata) '()
                      (list (diagnostic index rule path 'unchecked-condition
                              "rule conditions are not checked by the implementation"))))))
         (if (pair? conditions) conditions
             (let ((answers
                     (run 1 (expected)
                       (check-stepo assumptions from expected rule path))))
               (cond
                 ((and (pair? answers) (equal? (car answers) to)) '())
                 ((pair? answers)
                  (list (diagnostic index rule path 'target-mismatch
                          (string-append "rule produces " (math-term->source (car answers))
                                         ", not the supplied target"))))
                 (else (list (diagnostic index rule path 'rule-failure
                               "rule does not apply under the supplied premises")))))))))))

(define (math-verify-step assumptions from to rule path)
  (and (math-term? from) (math-term? to)
       (null? (math-execution-diagnostics assumptions from 'check))
       (null? (math-execution-diagnostics assumptions to 'check))
       (null? (step-diagnostics 1 assumptions from to rule path
                               (math-rule-names #:mode 'check)))))

(define* (verify-math-certificate result #:key (rules #f))
  (unless (math-derivation? result) (error "expected a math derivation" result))
  (let* ((assumptions (math-derivation-assumptions result))
         (terms (math-derivation-terms result))
         (steps (math-derivation-steps result))
         (premise-diagnostics (math-assumption-diagnostics assumptions))
         (allowed (math-selected-rules rules (math-rule-names #:mode 'check)
                    #:mode 'check)))
    (make-math-verification result
      (cond
        ((not (and (list? terms) (pair? terms) (every math-term? terms)
                    (list? steps) (list? assumptions)))
          (list (diagnostic #f #f '() 'invalid-chain "malformed term chain or assumptions")))
        ((pair? premise-diagnostics) premise-diagnostics)
        (else
          (append
            (math-requirement-diagnostics assumptions (math-derivation-requirements result))
            (append-map
              (lambda (term)
                (remove (lambda (item) (eq? (math-diagnostic-code item) 'type-error))
                        (math-execution-diagnostics assumptions term 'check)))
              terms)
            (if (= (length terms) (+ (length steps) 1)) '()
                (list (diagnostic #f #f '() 'chain-length
                        "term and step counts do not form a complete chain")))
            (if (and (null? steps) (not (math-infer-type assumptions (car terms))))
                (list (diagnostic #f #f '() 'type-error
                        (math-type-diagnostic assumptions (car terms)))) '())
            (let check ((steps steps) (terms terms) (index 1))
              (if (or (null? steps) (null? (cdr terms))) '()
                  (let* ((step (car steps))
                         (errors
                           (cond
                             ((not (math-derivation-step? step))
                              (list (diagnostic index #f '() 'invalid-step "invalid step record")))
                             ((or (not (equal? (math-derivation-step-from step) (car terms)))
                                  (not (equal? (math-derivation-step-to step) (cadr terms))))
                              (list (diagnostic index (math-derivation-step-rule step)
                                      (math-derivation-step-path step) 'endpoints
                                      "step endpoints do not match the term chain")))
                             (else
                              (step-diagnostics index assumptions (car terms) (cadr terms)
                                (math-derivation-step-rule step)
                                (math-derivation-step-path step) allowed)))))
                    (append errors (check (cdr steps) (cdr terms) (+ index 1))))))))))))

;; Missing-premise hints are found with augmented facts only to identify a rule
;; and path. The certificate retains the ORIGINAL assumptions and is replayed
;; independently below; hints never discharge a premise.
(define (infer-step assumptions from to names)
  (let ((answer (find (lambda (answer) (equal? (car answer) to))
                     (math-rewrite-answers assumptions from names #f #:mode 'check))))
    (if answer
        (make-math-derivation-step from to (cadr answer) (caddr answer))
        (let* ((hints
                 (append-map
                   (lambda (name)
                     (filter-map
                       (lambda (path)
                         (let ((required
                                 (math-rule-required-facts name (math-term-at-path from path))))
                           (and required (list name path required))))
                       (math-term-paths from)))
                   names))
               (hint
                 (or (find
                       (lambda (hint)
                         (any (lambda (answer)
                                (and (equal? (car answer) to)
                                     (equal? (caddr answer) (cadr hint))))
                              (math-rewrite-answers
                                (append (caddr hint) assumptions) from (list (car hint)) #f
                                #:mode 'check)))
                       hints)
                     (and (pair? hints) (car hints)))))
          (make-math-derivation-step from to (and hint (car hint))
                                     (if hint (cadr hint) '()))))))

(define* (check-math-chain terms assumptions #:key (steps #f) (rules #f) (sources '()) (requirements '()))
  (unless (and (list? terms) (>= (length terms) 2) (every math-term? terms))
    (error "a handwritten math chain needs at least two valid terms" terms))
  (when (and steps (not (and (list? steps) (= (length steps) (- (length terms) 1)))))
    (error "provide one step specification per adjacent pair" steps))
  (let ((allowed (math-selected-rules rules (math-rule-names #:mode 'check)
                   #:mode 'check)))
    (math-verify-derivation
      (make-math-derivation terms
        (map
          (lambda (from to spec)
            (cond
              ((not spec) (infer-step assumptions from to allowed))
              ((symbol? spec)
               (if (math-rewrite-rule-ref spec)
                   (let ((inferred (infer-step assumptions from to (list spec))))
                     (if (math-derivation-step-rule inferred) inferred
                         (make-math-derivation-step from to spec '())))
                   (make-math-derivation-step from to spec '())))
              ((and (list? spec) (= (length spec) 2) (symbol? (car spec)))
               (make-math-derivation-step from to (car spec) (cadr spec)))
              (else (error "expected a rule name or (rule operand-path)" spec))))
          (drop-right terms 1) (cdr terms)
          (or steps (make-list (- (length terms) 1) #f)))
        assumptions #:sources sources #:requirements requirements)
      #:rules allowed)))

(define* (math-verify-derivation certificate #:key (rules #f))
  (let* ((verification (verify-math-certificate certificate #:rules rules))
         (sources (math-derivation-sources certificate))
         (sources (if (and (list? sources) (every (lambda (s) (or (not s) (math-origin? s))) sources))
                      sources '()))
         (fallback (find math-origin? sources)))
    (make-math-verification certificate
      (map (lambda (diagnostic)
             (let* ((step (math-diagnostic-step diagnostic))
                    (index (if step
                               (if (eq? (math-diagnostic-code diagnostic) 'target-mismatch)
                                   step (- step 1)) 0))
                    (origin (and (< index (length sources)) (list-ref sources index))))
               (math-locate-diagnostic diagnostic (or origin fallback)
                 (if origin (math-diagnostic-path diagnostic) '()))))
           (math-verification-diagnostics verification)))))
