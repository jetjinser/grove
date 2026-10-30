;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math calculation)
             (grove math provenance) (grove math expression) (grove math derivation)
             ((grove math semantic) #:prefix ◆:)
             (tests unit grove source-fixtures)
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
    (let* ((report (φ:simplify (car fixture) #:assumptions facts))
           (final (math-report-final report)))
      (test-assert "scalar simplification completes" (math-report-finished? report))
      (test-equal "canonical scalar result" (φ:term (cadr fixture)) final)
      (test-equal "simplification is idempotent" final
        (math-report-final (φ:simplify (math-report-result report) #:assumptions facts)))
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
  (φ:term "-1/x^2")
  (math-report-final (φ:differentiate "x/x^2" "x" #:assumptions facts)))
(let ((a (math-report-final (φ:normalize "y+x+x*y" #:assumptions facts)))
      (b (math-report-final (φ:normalize "y*x+x+y" #:assumptions facts))))
  (test-equal "equivalent scalar orders share a normal form" a b))
(test-equal "factored structure remains the default"
  (φ:term "x*(x+1)")
  (math-report-final (φ:simplify "(x+1)*x" #:assumptions facts)))
(let ((expanded (φ:expand "(x+1)*(x-1)" #:assumptions facts)))
  (test-equal "expansion is explicitly selected" (φ:term "x^2-1")
    (math-report-final expanded))
  (test-assert "expansion certificate replays"
    (math-verification-valid? (math-verify-derivation (math-report-derivation expanded)))))
(test-equal "expansion budget retains an incomplete report" 'step-limit
  (math-report-reason (φ:expand "(x+1)*(x-1)" #:assumptions facts #:max-steps 0)))
(test-equal "normalization budget is enforced" 'step-limit
  (math-report-reason (φ:normalize "y*x" #:assumptions facts #:max-steps 0)))
(test-equal "no cancellation without a denominator premise" 'missing-premise
  (math-report-reason (φ:simplify "x/x" #:assumptions '("scalar(x)"))))
(test-equal "domain premises survive scalar reordering"
  (φ:term "1/(x*y)")
  (math-report-final (φ:simplify "1/(y*x)"
                       #:assumptions '("scalar(x); scalar(y); nonzero(y*x)"))))
(test-assert "a default rewrite does not expand a scalar product"
  (not (memq 'expand-scalar-product
         (map math-derivation-step-rule
           (math-derivation-steps (math-report-derivation
             (φ:rewrite "(x+1)*(x-1)" #:assumptions facts)))))))

(let* ((report (φ:simplify "x/x" #:assumptions facts))
       (one (math-report-result report))
       (embedded (φ:add one "2")))
  (test-assert "cancelled denominator remains a domain requirement"
    (member '(nonzero (var x)) (math-value-requirements one)))
  (test-assert "embedding retains the original domain"
    (member '(nonzero (var x)) (math-value-requirements embedded)))
  (for-each
    (lambda (operation)
      (test-equal "reuse cannot silently widen a computed value's domain" 'missing-premise
        (math-report-reason (operation one #:assumptions '("scalar(x)")))))
    (list φ:simplify φ:normalize φ:expand φ:calculate φ:collect φ:rewrite))
  (test-equal "the original premise authorizes reuse" '(num "3")
    (math-report-final (φ:simplify embedded #:assumptions facts)))
  (test-equal "differentiating a cancelled value retains its domain" 'missing-premise
    (math-report-reason (φ:differentiate one "x" #:assumptions '("scalar(x)"))))
  (test-equal "search target retains inherited conditions" 'missing-premise
    (math-report-reason (φ:search "1" one #:assumptions '("scalar(x)"))))
  (test-assert "search with the retained premise finds a certificate"
    (pair? (math-report-candidates (φ:search "1" one #:assumptions facts))))
  (test-assert "authored chains retain inherited domain requirements"
    (not (math-verification-valid? (φ:check-chain (list embedded "3")
                                    #:assumptions '("scalar(x)")))))
  (test-assert "replay cannot drop a retained domain after the original term disappears"
    (not (math-verification-valid?
           (make-math-derivation '((num "1")) '() '((scalar (var x)))
             #:requirements (math-value-requirements one)))))
  (test-assert "presentation changes preserve requirements"
    (equal? (math-value-requirements one) (math-value-requirements (φ:styled one #:variant 'bold)))))

(test-equal "scalar ordering does not reorder vector products"
  (φ:term "E*x")
  (math-report-final (φ:normalize "E*x" #:assumptions '("vector(E); scalar(x)"))))
(test-assert "normalization provenance is retained"
  (any math-origin-generated?
    (math-derivation-sources (math-report-derivation
      (φ:normalize (located-text "y*x") #:assumptions facts)))))
(for-each
  (lambda (source)
    (let ((report (φ:simplify source #:assumptions '("coordinate(x)"))))
      (test-assert "function-plus-constant normalization does not cycle"
        (math-report-finished? report))
      (test-equal "constants follow zero-degree function terms"
        (φ:term "sin(x)+1") (math-report-final report))
      (test-assert "function-plus-constant normal forms are idempotent"
        (null? (math-derivation-steps (math-report-derivation
                 (φ:simplify (math-report-result report) #:assumptions '("coordinate(x)"))))))))
  '("sin(x)+1" "1+sin(x)" "sin(x)+2-1"))
(test-end "stable scalar normalization")
