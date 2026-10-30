;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math report)
  #:use-module (grove private snapshot)
  #:use-module (grove math derivation)
  #:use-module (grove math diagnostic)
  #:use-module (grove math expression)
  #:use-module (grove math term)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-math-report math-report? math-report-name
            math-report-candidates math-report-reason math-report-limits
            math-report-states math-report-diagnostics math-report-passes
            math-report-finished? math-report-status math-report-unresolved
            math-report-derivation math-report-final
            math-report-audit math-report-result))

(define-record-type <math-report>
(%make-math-report name candidates reason limits states diagnostics passes audit)
  math-report?
  (name math-report-name)
  (candidates report-candidates/raw)
  (reason math-report-reason)
  (limits report-limits/raw)
  (states math-report-states)
  (diagnostics report-diagnostics/raw)
  (passes report-passes/raw)
  (audit math-report-audit))

(define (math-report-candidates report)
  (snapshot-data (report-candidates/raw report)))

(define (math-report-limits report)
  (snapshot-data (report-limits/raw report)))

(define (math-report-diagnostics report)
  (snapshot-data (report-diagnostics/raw report)))

(define (math-report-passes report)
  (snapshot-data (report-passes/raw report)))

(define* (make-math-report name candidates reason limits states
                          #:key (diagnostics '()) (passes '()) (audit #f))
  "Construct a report from a symbolic NAME and REASON, derivation CANDIDATES,
LIMITS, and a non-negative STATES count. DIAGNOSTICS and PASSES contain math
diagnostic and report values; AUDIT is an optional opaque evidence value."
  (unless (symbol? name)
    (error "math report name must be a symbol" name))
  (unless (and (list? candidates) (every math-derivation? candidates))
    (error "math report candidates must be derivations" candidates))
  (unless (symbol? reason)
    (error "math report reason must be a symbol" reason))
  (unless (list? limits)
    (error "math report limits must be a list" limits))
  (unless (and (exact-integer? states) (>= states 0))
    (error "math report state count must be a non-negative integer" states))
  (unless (and (list? diagnostics) (every math-diagnostic? diagnostics))
    (error "math report diagnostics must be diagnostic records" diagnostics))
  (unless (and (list? passes) (every math-report? passes))
    (error "math report passes must be reports" passes))
  (%make-math-report name (snapshot-data candidates) reason
                     (snapshot-data limits) states
                     (snapshot-data diagnostics)
                     (snapshot-data passes) audit))

(define (math-report-finished? report)
  "Return whether calculation or search finished without diagnostics.

This does not mean that a search found a derivation or that an equation was
proved; inspect MATH-REPORT-STATUS for the outcome."
  (and (null? (math-report-diagnostics report))
       (memq (math-report-reason report) '(exhausted normal-form)) #t))

(define (math-report-derivation report)
  (and (pair? (math-report-candidates report)) (car (math-report-candidates report))))

(define (math-report-final report)
  (let ((derivation (math-report-derivation report)))
    (and derivation (last (math-derivation-terms derivation)))))

(define (math-report-result report)
  "Return a finished report's derived result as a reusable Semantic value, or #f.

Symbolic residuals are allowed in the result. A report without a derivation,
or one stopped before completion, has no reusable result; inspect its final
term, status, and diagnostics instead."
  (unless (math-report? report)
    (error "expected a math report" report))
  (let ((derivation (math-report-derivation report)))
    (and (math-report-finished? report)
         derivation
         (math-report-final report)
         (make-math-value
           (math-report-final report)
           #:requirements (math-derivation-requirements derivation)
           #:origin (let ((sources (math-derivation-sources derivation)))
                      (and (pair? sources) (last sources)))
           #:properties `((assumptions .
                           ,(math-derivation-assumptions derivation)))))))

(define (math-report-unresolved report)
  "Return operand paths of symbolic derivatives remaining in the final term."
  (let ((term (math-report-final report)))
    (if term
        (filter (lambda (path)
                  (memq (car (math-term-at-path term path)) '(differentiate partial prime darg)))
                (math-term-paths term)) '())))

(define (math-report-status report)
  "Classify the outcome as complete, symbolic, no-result, cutoff, unproven, or
error. FINISHED? describes whether the computation was exhausted; NO-RESULT
distinguishes an exhausted report that contains no derivation. A complete
calculation is not by itself a proof of an arbitrary mathematical claim."
  (cond ((math-report-finished? report)
         (cond ((not (math-report-derivation report)) 'no-result)
               ((null? (math-report-unresolved report)) 'complete)
               (else 'symbolic)))
        ((memq (math-report-reason report)
               '(depth-limit state-limit result-limit term-size-limit step-limit polynomial-limit)) 'cutoff)
        ((eq? (math-report-reason report) 'equation-not-proven) 'unproven)
        (else 'error)))
