;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math expression) (grove math derivation)
             ((grove math semantic) #:prefix ◆:)
             (ice-9 match) (srfi srfi-1) (srfi srfi-64))
(define facts '("coordinate(x); coordinate(y); nonzero(x); nonzero(y)"))

;; A numeric interpreter independent of the algebraic transforms. Fixtures are
;; sampled away from excluded denominators, and every recorded edge is checked.
(define (evaluate term x y)
  (define (go t) (evaluate t x y))
  (match term
    (('var 'x) x) (('var 'y) y)
    (('num n) (string->number n))
    (('- a) (- (go a)))
    (('+ a b) (+ (go a) (go b)))
    (('- a b) (- (go a) (go b)))
    (('* a b) (* (go a) (go b)))
    (('/ a b) (/ (go a) (go b)))
    (('^ a ('num n)) (expt (go a) (string->number n)))))

(test-begin "stable scalar normalization")
(for-each
  (lambda (fixture)
    (let* ((report (math-simplify (car fixture) #:assumptions facts))
           (final (math-report-final report)))
      (test-assert "scalar simplification completes" (math-report-complete? report))
      (test-equal "canonical scalar result" (math-source->term (cadr fixture)) final)
      (test-equal "simplification is idempotent" final
        (math-report-final (math-simplify (math-report-result report) #:assumptions facts)))
      (test-assert "normalization and cancellation certificates replay"
        (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
      (for-each
        (lambda (point)
          (for-each
            (lambda (step)
              (test-equal "independent numeric oracle checks each algebraic edge"
                (evaluate (math-derivation-step-from step) (car point) (cadr point))
                (evaluate (math-derivation-step-to step) (car point) (cadr point))))
            (math-derivation-steps (math-report-derivation report))))
        '((-2 3) (1 2) (3 -1)))))
  '(("x+2*x+x" "4*x") ("2*x*3*x^2" "6*x^3")
    ("y+x+y" "x+2*y") ("x*y+y*x" "2*x*y")
    ("x*(y*x)" "x^2*y") ("(x^2)^3" "x^6")
    ("(1/2)*x+(1/3)*x" "(5/6)*x")
    ("x/x" "1") ("0/x" "0") ("x^3/x" "x^2") ("x/(x*x)" "1/x")
    ("(2*x*y)/(4*x)" "(1/2)*y") ("(x*y)/(y*x)" "1")
    ("-x/x^2" "-1/x") ("x+(-2*x)" "-x")))
(test-equal "quotient differentiation simplifies to the acceptance result"
  (math-source->term "-1/x^2")
  (math-report-final (math-differentiate "x/x^2" "x" #:assumptions facts)))
(let ((a (math-report-final (math-normalize "y+x+x*y" #:assumptions facts)))
      (b (math-report-final (math-normalize "y*x+x+y" #:assumptions facts))))
  (test-equal "equivalent scalar orders share a normal form" a b))
(test-equal "factored structure remains the default"
  (math-source->term "x*(x+1)")
  (math-report-final (math-simplify "(x+1)*x" #:assumptions facts)))
(let ((expanded (math-expand "(x+1)*(x-1)" #:assumptions facts)))
  (test-equal "expansion is explicitly selected" (math-source->term "x^2-1")
    (math-report-final expanded))
  (test-assert "expansion certificate replays"
    (math-verification-valid? (math-verify-derivation (math-report-derivation expanded)))))
(test-equal "expansion budget retains an incomplete report" 'step-limit
  (math-report-reason (math-expand "(x+1)*(x-1)" #:assumptions facts #:max-steps 0)))
(test-equal "normalization budget is enforced" 'step-limit
  (math-report-reason (math-normalize "y*x" #:assumptions facts #:max-steps 0)))
(test-equal "no cancellation without a denominator premise" 'missing-premise
  (math-report-reason (math-simplify "x/x" #:assumptions '("scalar(x)"))))
(test-equal "domain premises survive scalar reordering"
  (math-source->term "1/(x*y)")
  (math-report-final (math-simplify "1/(y*x)"
                       #:assumptions '("scalar(x); scalar(y); nonzero(y*x)"))))
(test-assert "a default rewrite does not expand a scalar product"
  (not (memq 'expand-scalar-product
         (map math-derivation-step-rule
           (math-derivation-steps (math-report-derivation
             (math-rewrite "(x+1)*(x-1)" #:assumptions facts)))))))

(let* ((report (math-simplify "x/x" #:assumptions facts))
       (one (math-report-result report))
       (embedded (◆:add one "2")))
  (test-assert "cancelled denominator remains a domain requirement"
    (member '(nonzero (var x)) (math-value-requirements one)))
  (test-assert "embedding retains the original domain"
    (member '(nonzero (var x)) (math-value-requirements embedded)))
  (for-each
    (lambda (operation)
      (test-equal "reuse cannot silently widen a computed value's domain" 'missing-premise
        (math-report-reason (operation one #:assumptions '("scalar(x)")))))
    (list math-simplify math-normalize math-expand math-calculate math-collect math-rewrite))
  (test-equal "the original premise authorizes reuse" '(num "3")
    (math-report-final (math-simplify embedded #:assumptions facts)))
  (test-equal "differentiating a cancelled value retains its domain" 'missing-premise
    (math-report-reason (math-differentiate one "x" #:assumptions '("scalar(x)"))))
  (test-equal "search target retains inherited conditions" 'missing-premise
    (math-report-reason (math-search "1" one #:assumptions '("scalar(x)"))))
  (test-assert "search with the retained premise finds a certificate"
    (pair? (math-report-candidates (math-search "1" one #:assumptions facts))))
  (test-assert "authored chains retain inherited domain requirements"
    (not (math-verification-valid? (math-check-chain (list embedded "3")
                                    #:assumptions '("scalar(x)")))))
  (test-assert "replay cannot drop a retained domain after the original term disappears"
    (not (math-verification-valid?
           (make-math-derivation '((num "1")) '() '((scalar (var x)))
             #:requirements (math-value-requirements one)))))
  (test-assert "presentation changes preserve requirements"
    (equal? (math-value-requirements one) (math-value-requirements (◆:styled one #:variant 'bold)))))

(test-equal "scalar ordering does not reorder vector products"
  (math-source->term "E*x")
  (math-report-final (math-normalize "E*x" #:assumptions '("vector(E); scalar(x)"))))
(test-assert "normalization provenance is retained"
  (any math-origin-generated?
    (math-derivation-sources (math-report-derivation (math-normalize "y*x" #:assumptions facts)))))
(test-end "stable scalar normalization")
