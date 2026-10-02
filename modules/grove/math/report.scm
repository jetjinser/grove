;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math report)
  #:use-module (grove math derivation)
  #:use-module (grove math audit)
  #:use-module (grove math term)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-math-report math-report? math-report-name
            math-report-candidates math-report-reason math-report-limits
            math-report-states math-report-diagnostics math-report-passes
            math-report-complete? math-report-status math-report-unresolved
            math-report-derivation math-report-final
            math-report-audit math-report-with-audit))

(define-record-type <math-report>
(%make-math-report name candidates reason limits states diagnostics passes audit)
  math-report?
  (name math-report-name)
  (candidates math-report-candidates)
  (reason math-report-reason)
  (limits math-report-limits)
  (states math-report-states)
  (diagnostics math-report-diagnostics)
  (passes math-report-passes)
  (audit math-report-audit))

(define* (make-math-report name candidates reason limits states
                          #:key (diagnostics '()) (passes '()) (audit #f))
  (%make-math-report name candidates reason limits states diagnostics passes audit))

(define (math-report-with-audit report audit)
  (unless (math-report? report) (error "expected a math report" report))
  (unless (or (not audit) (math-calculation-audit? audit))
    (error "expected a calculation audit" audit))
  (%make-math-report (math-report-name report) (math-report-candidates report)
    (math-report-reason report) (math-report-limits report) (math-report-states report)
    (math-report-diagnostics report) (math-report-passes report) audit))

(define (math-report-complete? report)
  (and (null? (math-report-diagnostics report))
       (memq (math-report-reason report) '(exhausted normal-form)) #t))

(define (math-report-derivation report)
  (and (pair? (math-report-candidates report)) (car (math-report-candidates report))))

(define (math-report-final report)
  (let ((derivation (math-report-derivation report)))
    (and derivation (last (math-derivation-terms derivation)))))

(define (math-report-unresolved report)
  "Return operand paths of symbolic derivatives remaining in the final term."
  (let ((term (math-report-final report)))
    (if term
        (filter (lambda (path)
                  (memq (car (math-term-at-path term path)) '(differentiate partial prime darg)))
                (math-term-paths term)) '())))

(define (math-report-status report)
  "Classify completion separately from the strategy's stopping reason."
  (cond ((math-report-complete? report)
         (if (null? (math-report-unresolved report)) 'complete 'symbolic))
        ((memq (math-report-reason report)
               '(depth-limit state-limit result-limit term-size-limit step-limit polynomial-limit)) 'cutoff)
        ((eq? (math-report-reason report) 'equation-not-proven) 'unproven)
        (else 'error)))
