;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math context) (grove math types)
             (grove math term) (grove math derivation) (grove math operator)
             (grove math assumption) (grove math layout) (grove math expression)
             ((grove math semantic) #:prefix semantic:)
             (grove render html) (grove ir) (grove relation)
             (srfi srfi-1) (srfi srfi-13) (srfi srfi-64))

(define (first-diagnostic report) (car (math-report-diagnostics report)))
(define scalar-x '((scalar (var x))))
(define conflict '((scalar (var y)) (vector (var y)) (scalar (var x))))
(test-begin "mathematical expression contracts")
(let ((value (semantic:read "x+opaque(x)")))
  (test-assert "unknown function applications remain structurally valid"
    (math-term? (math-value-term value)))
  (test-assert "unknown function applications remain displayable"
    (string-contains (sxml->html (content->sxml
      (make-content-element 'math (list (math-value->layout value))))) "<mi>opaque</mi>"))
  (for-each
    (lambda (execute)
      (let ((report (execute value #:assumptions '("scalar(x)"))))
        (test-equal "executors distinguish unsupported operations from type failures"
          'unsupported-expression (math-report-reason report))
        (test-equal "unsupported operations identify the nested operand"
          '(1) (math-diagnostic-path (first-diagnostic report)))
        (test-assert "unsupported operations never count as a completed normal form"
          (not (math-report-complete? report)))))
    (list math-rewrite math-simplify math-calculate math-collect)))
(test-equal "search uses the same executable vocabulary"
  'unsupported-expression
  (math-report-reason (math-search "opaque(x)" "opaque(x)" #:assumptions '("scalar(x)"))))
(test-assert "handwritten verification rejects unsupported functions"
  (not (math-verification-valid?
    (math-check-chain '("opaque(x)+0" "opaque(x)") #:assumptions '("scalar(x)") #:steps '(add-zero)))))
(test-equal "supported operators still diagnose missing variable declarations as type errors"
  'type-error (math-report-reason (math-simplify "x+0")))
(test-assert "a supported expression with no applicable rule is a normal form"
  (math-report-complete? (math-simplify "x" #:assumptions '("scalar(x)"))))
(test-equal "displayable non-decimal numerals do not silently become exact numbers"
  'unsupported-expression (math-report-reason (math-calculate "Ⅲ")))
(test-assert "type mismatch describes the actual operands"
  (string-contains
    (math-diagnostic-message (first-diagnostic
      (math-simplify "x+E" #:assumptions '("scalar(x); vector(E)"))))
    "operand types (scalar vector)"))
(let* ((source (string-copy "x+opaque(x)")))
  (set-source-properties! source '((filename . "contract.plant") (line . 4) (column . 2)))
  (let* ((report (math-differentiate source "x" #:assumptions '("scalar(x)")))
         (diagnostic (first-diagnostic report)))
    (test-equal "polynomial rejection locates the unsupported subexpression" '(0 1)
      (math-diagnostic-path diagnostic))
    (test-equal "domain failure retains the authored line" 5
      (math-location-line (math-diagnostic-source diagnostic)))
    (test-equal "domain failure retains the nested expression column" 5
      (math-location-column (math-diagnostic-source diagnostic)))
    (test-equal "a specialized pass returns one domain failure"
      1 (length (math-report-diagnostics report)))))
(test-equal "a typed quotient requires a nonzero denominator"
  'missing-premise
  (math-report-reason (math-differentiate "x/y" "x" #:assumptions '("scalar(x); scalar(y)"))))
(test-equal "differentiation identifies an invalid coordinate separately"
  '(1) (math-diagnostic-path (first-diagnostic (math-differentiate "x" "x+1"))))
(test-equal "natural powers retain their expected derivative"
  (math-source->term "3*x^2+2")
  (math-report-final (math-differentiate "x^3+2*x" "x" #:assumptions '("scalar(x)"))))
(test-assert "replay rejects contradictory premises even in a zero-step certificate"
  (not (math-verification-valid?
    (math-verify-derivation (make-math-derivation '((num "1")) '() conflict)))))
(test-assert "one-step verification checks the full assumption set"
  (not (math-verify-step conflict '(+ (num "1") (num "0")) '(num "1") 'add-zero '())))
(test-equal "arbitrary expressions cannot masquerade as premises"
  'invalid-assumption
  (math-report-reason (math-simplify "1" #:assumptions '("x+1"))))
(test-equal "type declarations name variables rather than computed expressions"
  'invalid-assumption
  (math-report-reason (math-simplify "1" #:assumptions '("scalar(x+1)"))))
(test-assert "facts retain the supported declaration, regularity and equation forms"
  (every math-assumption?
    (map math-source->term '("vector(E)" "scalar(x)" "smooth(E)" "div(E)=0"))))
(let ((result (math-report-result
                (math-simplify "E+zero(vector)" #:assumptions '("vector(E)")))))
  (test-assert "retained display premises still render a vector accent"
    (string-contains (sxml->html (content->sxml
      (make-content-element 'math (list (math-value->layout result))))) "<mover"))
  (test-equal "display premises do not authorize another computation"
    'type-error (math-report-reason (math-simplify result))))
(with-math-assumptions '("scalar(x)")
  (with-math-assumptions '("vector(x)")
    (test-equal "conflicting child contexts cannot silently shadow the parent"
      'type-error (math-report-reason (math-calculate "1+1"))))
  (test-assert "the parent context remains usable after a conflicting child"
    (math-report-complete? (math-simplify "x+0"))))
(for-each
  (lambda (term)
    (test-assert "reserved operator arities reject malformed source-independent terms"
      (not (math-term? term))))
  '((curl (var E) (var B)) (differentiate (var x)) (+ (var x)) (smooth)))
(test-equal "type signatures remain usable relationally with an unknown operator"
  '(+ - * / ^)
  (run* (operator) (math-typeo scalar-x `(,operator (var x) (num "2")) 'scalar)))
(test-end "mathematical expression contracts")
