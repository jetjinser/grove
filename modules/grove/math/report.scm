;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math report)
  #:use-module (grove math derivation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-math-report math-report? math-report-name
            math-report-candidates math-report-reason math-report-limits
            math-report-states math-report-diagnostics math-report-passes
            math-report-complete? math-report-derivation math-report-final))

(define-record-type <math-report>
  (%make-math-report name candidates reason limits states diagnostics passes)
  math-report?
  (name math-report-name)
  (candidates math-report-candidates)
  (reason math-report-reason)
  (limits math-report-limits)
  (states math-report-states)
  (diagnostics math-report-diagnostics)
  (passes math-report-passes))

(define* (make-math-report name candidates reason limits states
                          #:key (diagnostics '()) (passes '()))
  (%make-math-report name candidates reason limits states diagnostics passes))

(define (math-report-complete? report)
  (and (null? (math-report-diagnostics report))
       (memq (math-report-reason report) '(exhausted normal-form)) #t))

(define (math-report-derivation report)
  (and (pair? (math-report-candidates report)) (car (math-report-candidates report))))

(define (math-report-final report)
  (let ((derivation (math-report-derivation report)))
    (and derivation (last (math-derivation-terms derivation)))))
