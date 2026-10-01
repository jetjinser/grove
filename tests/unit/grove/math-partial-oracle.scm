;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (tests unit grove polynomial-oracle)
             (srfi srfi-1) (srfi srfi-64))

;; Keep composed partial checks in their own suite so each independent oracle
;; stays within the ordinary test budget. Every seed checks every recorded edge.
(test-begin "independent partial derivative oracle")
(define fixtures
  (map (lambda (entry)
         (list (car entry) (cadr entry)
               (math-partial (car entry) (cadr entry) #:assumptions (caddr entry))))
       '(("(x+y)^3+x*y-z^2" "y" ("coordinate(x); coordinate(y); coordinate(z)"))
         ("x*f(x,y)" "x" ("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)"))
         ("f(y,x)+x" "x" ("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)"))
         ("(f(x,y)+x)^2" "x" ("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)")))))
(for-each
  (lambda (fixture)
    (let ((report (caddr fixture)))
      (test-assert "polynomial and symbolic partials reach normal form" (math-report-complete? report))
      (test-assert "composed partial certificates replay independently"
        (math-verification-valid? (math-verify-derivation (math-report-derivation report)))))) fixtures)
(for-each
  (lambda (seed)
    (let* ((f (normalize `((,seed 3 1 0) (2 0 2 1) (-1 1 0 2) (1 0 0 0))))
           (environment (append coordinates (list (cons 'f (scalar f))))))
      (for-each
        (lambda (fixture)
          (let ((source (car fixture)) (coordinate (cadr fixture)) (report (caddr fixture)))
            (test-equal "symbolic partials agree with an independently supplied polynomial function"
              (evaluate `(partial ,(math-source->term source) ,(math-source->term coordinate)) environment)
              (evaluate (math-report-final report) environment))
            (for-each
              (lambda (step)
                (test-equal "independent polynomial oracle checks every partial pass edge"
                  (evaluate (math-derivation-step-from step) environment)
                  (evaluate (math-derivation-step-to step) environment)))
              (math-derivation-steps (math-report-derivation report))))) fixtures)))
  '(-3 0 2))
(test-end "independent partial derivative oracle")
