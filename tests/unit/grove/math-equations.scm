;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (tests unit grove polynomial-oracle)
             (tests unit grove cartesian-fixtures) ((grove math semantic) #:prefix ◆:)
             (srfi srfi-1) (srfi srfi-64))
(test-begin "equations verified by residual certificates")
(check-equation '("(x+y)^2" "x^2+2*x*y+y^2"))
(for-each (lambda (entry)
            (let ((report (φ:check-equation (car entry) (cadr entry) #:assumptions (list basis))))
              (test-equal "unverified equations are explicit" 'equation-not-proven (math-report-reason report))
              (test-assert "nonzero residual is not success" (not (math-report-finished? report)))))
  '(("x^2" "x") ("laplacian(x^2)" "3")))
(test-equal "a symbolic derivative does not establish an identity" 'equation-not-proven
  (math-report-reason (φ:check-equation "partial(u,x)" "0"
    #:assumptions (list basis "scalar(u); differentiable(u)"))))
(test-equal "residual respects step limits" 'step-limit
  (math-report-reason (φ:check-equation "x+0" "x" #:max-steps 0 #:assumptions (list basis))))
(test-equal "polynomial verification reports expansion cutoffs" 'polynomial-limit
  (math-report-reason (φ:check-equation "(x+y+z)^65" "0" #:assumptions (list basis))))
(test-end "equations verified by residual certificates")
