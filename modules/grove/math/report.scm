;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math report)
  #:use-module (grove private snapshot)
  #:use-module (grove math derivation)
  #:use-module (grove math term)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-math-report math-report? math-report-name
            math-report-candidates math-report-reason math-report-limits
            math-report-states math-report-diagnostics math-report-passes
            math-report-complete? math-report-status math-report-unresolved
            math-report-derivation math-report-final
            math-report-audit))

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
  (%make-math-report name (snapshot-data candidates) reason
                     (snapshot-data limits) states
                     (snapshot-data diagnostics)
                     (snapshot-data passes) audit))

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
