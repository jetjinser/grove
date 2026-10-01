;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math pass) (grove math rewrite)
             (grove math context) (tests unit grove cartesian-fixtures)
             (srfi srfi-64))
(test-begin "bounded innermost execution")
(let* ((source (φ:term "partial(partial(x^2,x),x)"))
       (report (run-math-pass source facts #:name 'partial #:rules '(partial-power partial-variable)
                 #:order 'innermost #:max-steps 1))
       (certificate (math-report-derivation report)))
  (test-equal "inner derivative is rewritten first" '(0)
    (math-derivation-step-path (car (math-derivation-steps certificate))))
  (test-equal "budget retains the inner-step prefix" 'step-limit (math-report-reason report))
  (test-assert "innermost edges use the same checked relation"
    (math-verification-valid? (math-verify-derivation certificate))))
(let* ((term (φ:term "(x+0)+(y+0)"))
       (answer (math-rewrite-innermost facts term '(add-zero))))
  (test-equal "innermost traverses operands left to right" '(0) (caddr answer))
  (test-equal "the chosen edge is an ordinary relational answer" answer
    (car (math-rewrite-answers facts term '(add-zero) #f #:max-results 1))))
(test-error "unknown execution orders are rejected" #t
  (run-math-pass '(num "0") facts #:name 'order #:rules '() #:order 'unknown))
(test-end "bounded innermost execution")
