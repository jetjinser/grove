;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math calculation)
             (grove math presentation) (grove math term) (grove render html)
             (grove math pass) (srfi srfi-1) (srfi srfi-64))

(define facts '("scalar(x)"))
(test-begin "shared math execution")
(let ((interface (module-public-interface (resolve-module '(grove math pass)))))
  (test-assert "execution diagnostics stay inside the math implementation"
    (not (module-variable interface 'math-execution-diagnostics)))
  (test-assert "bound validation helper stays inside the math implementation"
    (not (module-variable interface 'check-math-bound))))
(for-each
  (lambda (report)
    (test-assert "every executor returns the shared report" (math-report? report))
    (test-equal "step budget has a common stopping reason" 'step-limit (math-report-reason report))
    (test-equal "step budget retains its structured diagnostic" 'step-limit
      (math-diagnostic-code (car (math-report-diagnostics report))))
    (test-assert "limited reports are not complete" (not (math-report-complete? report)))
    (test-assert "valid partial certificates replay"
      (math-verification-valid? (math-verify-derivation (math-report-derivation report)))))
  (list (φ:rewrite "x+x" #:assumptions facts #:max-steps 0)
        (φ:simplify "1*x" #:assumptions facts #:max-steps 0)
        (φ:calculate "2*3" #:max-steps 0)
        (φ:collect "x+x" #:assumptions facts #:max-steps 0)
        (φ:differentiate "x^3" "x" #:assumptions facts #:max-steps 1)))
(let ((report (φ:differentiate "x^1" "x" #:assumptions facts #:max-steps 2)))
  (test-equal "composition retains both named passes" '(differentiate simplify)
    (map math-report-name (math-report-passes report)))
  (test-assert "earlier successful passes remain complete"
    (math-report-complete? (car (math-report-passes report))))
  (test-assert "the aggregate partial certificate independently replays"
    (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
  (test-equal "aggregate final term is the last pass's retained term"
    (math-report-final (last (math-report-passes report))) (math-report-final report)))
(let ((report (φ:differentiate "x^3" "x" #:assumptions facts #:max-term-size 5)))
  (test-equal "oversized successor stops expansion" 'term-size-limit (math-report-reason report))
  (test-equal "oversized successor is excluded from the certificate"
    (φ:term "differentiate(x^3,x)") (math-report-final report)))
(for-each
  (lambda (report)
    (test-equal "division by zero has an explicit diagnostic" 'division-by-zero
      (math-diagnostic-code (car (math-report-diagnostics report))))
    (test-assert "undefined arithmetic is never reported as complete"
      (not (math-report-complete? report))))
  (list (φ:calculate "1/0") (φ:simplify "x+1/0" #:assumptions facts)
        (φ:search "1/0" "1")))
(test-equal "computed zero denominators stop before an invalid edge is recorded"
  (φ:term "1/(1-1)") (math-report-final (φ:calculate "1/(1-1)")))
(test-equal "search endpoints receive structured type diagnostics" 'type-error
  (math-report-reason (φ:search "x" "1")))
(test-equal "search endpoint type mismatch is reported" 'type-change
  (math-report-reason (φ:search "E" "x" #:assumptions '("vector(E); scalar(x)"))))
(test-assert "a normal form requires no step budget"
  (math-report-complete? (φ:calculate "3" #:max-steps 0)))
(test-error "argument errors still reject invalid bounds" #t
  (φ:differentiate "x" "x" #:assumptions facts #:max-steps -1))
(test-error "rendering rejects an incomplete derivative" #t
  (math-completed-report->content
    (φ:differentiate "x^3" "x" #:assumptions facts #:max-steps 1)))
(test-assert "generic report rendering identifies the operation and its diagnostics"
  (let ((html (sxml->html
                `(article ,@(map content->sxml (math-report->content (φ:calculate "1/0")))))))
    (and (string-contains html "Calculate failed:")
         (string-contains html "division by zero"))))
(test-end "shared math execution")
