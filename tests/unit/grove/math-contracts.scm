;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math context) (grove math types)
             ((grove math formula) #:prefix φ:)
             (grove math term) (grove math derivation)
             (grove math private assumption) (grove math layout) (grove math expression)
             ((grove math semantic) #:prefix semantic:)
             (grove render html) (grove ir) (grove relation)
             (srfi srfi-1) (srfi srfi-13) (srfi srfi-64))

(define (first-diagnostic report) (car (math-report-diagnostics report)))
(define scalar-x '((scalar (var x))))
(define conflict '((scalar (var y)) (vector (var y)) (scalar (var x))))
(test-begin "mathematical expression contracts")
(test-error "pipeline requires a Semantic value" #t (math-simplify "x+0"))
(test-assert "Formula facade parses then calls the Semantic pipeline"
  (math-report-complete? (φ:simplify "x+0" #:assumptions '("scalar(x)"))))
(test-equal "prime notation accepts a declared Semantic function value"
  '(prime (var f) (var x))
  (math-value-term
    (semantic:prime (semantic:function 'f) (semantic:variable 'x))))
(let* ((term (list '+ (list 'var 'x) (list 'num (string-copy "2"))))
       (properties (list (cons 'children (list (list (cons 'variant 'original))))))
       (declarations (list '(scalar (var x))))
       (requirements (list '(positive (var x))))
       (value (make-math-value term #:properties properties
                               #:declarations declarations
                               #:requirements requirements)))
  (set-car! (cadr term) 'changed)
  (string-set! (cadr (caddr term)) 0 #\9)
  (set-cdr! (cdar properties) '((variant . changed)))
  (set-car! declarations '(vector (var x)))
  (set-car! requirements '(nonzero (var x)))
  (test-equal "Semantic values snapshot mutable constructor data"
    '(+ (var x) (num "2")) (math-value-term value))
  (test-equal "Semantic values snapshot presentation properties"
    '((children ((variant . original)))) (math-value-properties value))
  (test-equal "Semantic values snapshot declarations"
    '((scalar (var x))) (math-value-declarations value))
  (test-equal "Semantic values snapshot requirements"
    '((positive (var x))) (math-value-requirements value))
  (let ((exposed-term (math-value-term value))
        (exposed-properties (math-value-properties value))
        (exposed-declarations (math-value-declarations value))
        (exposed-requirements (math-value-requirements value)))
    (set-car! (cadr exposed-term) 'changed)
    (set-car! (car (cdar exposed-properties)) 'changed)
    (set-car! (car exposed-declarations) 'changed)
    (set-car! (car exposed-requirements) 'changed))
  (test-equal "Semantic value accessors return independent term trees"
    '(+ (var x) (num "2")) (math-value-term value))
  (test-equal "Semantic value accessors return independent properties"
    '((children ((variant . original)))) (math-value-properties value))
  (test-equal "Semantic value accessors return independent declarations"
    '((scalar (var x))) (math-value-declarations value))
  (test-equal "Semantic value accessors return independent requirements"
    '((positive (var x))) (math-value-requirements value)))
(let ((value (φ:read "x+opaque(x)")))
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
    (list φ:rewrite φ:simplify φ:calculate φ:collect)))
(test-equal "search uses the same executable vocabulary"
  'unsupported-expression
  (math-report-reason (φ:search "opaque(x)" "opaque(x)" #:assumptions '("scalar(x)"))))
(test-assert "handwritten verification rejects unsupported functions"
  (not (math-verification-valid?
    (φ:check-chain '("opaque(x)+0" "opaque(x)") #:assumptions '("scalar(x)") #:steps '(add-zero)))))
(test-equal "supported operators still diagnose missing variable declarations as type errors"
  'type-error (math-report-reason (φ:simplify "x+0")))
(test-assert "a supported expression with no applicable rule is a normal form"
  (math-report-complete? (φ:simplify "x" #:assumptions '("scalar(x)"))))
(test-equal "displayable non-decimal numerals do not silently become exact numbers"
  'unsupported-expression (math-report-reason (φ:calculate "Ⅲ")))
(test-assert "type mismatch describes the actual operands"
  (string-contains
    (math-diagnostic-message (first-diagnostic
      (φ:simplify "x+E" #:assumptions '("scalar(x); vector(E)"))))
    "operand types (scalar vector)"))
(let* ((source (string-copy "x+opaque(x)")))
  (set-source-properties! source '((filename . "contract.plant") (line . 4) (column . 2)))
  (let* ((report (φ:differentiate source "x" #:assumptions '("scalar(x)")))
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
  (math-report-reason (φ:differentiate "x/y" "x" #:assumptions '("scalar(x); scalar(y)"))))
(test-equal "differentiation identifies an invalid coordinate separately"
  '(1) (math-diagnostic-path (first-diagnostic (φ:differentiate "x" "x+1"))))
(test-equal "natural powers retain their expected derivative"
  (φ:term "3*x^2+2")
  (math-report-final (φ:differentiate "x^3+2*x" "x" #:assumptions '("scalar(x)"))))
(test-assert "replay rejects contradictory premises even in a zero-step certificate"
  (not (math-verification-valid?
    (math-verify-derivation (make-math-derivation '((num "1")) '() conflict)))))
(test-assert "one-step verification checks the full assumption set"
  (not (math-verify-step conflict '(+ (num "1") (num "0")) '(num "1") 'add-zero '())))
(test-equal "arbitrary expressions cannot masquerade as premises"
  'invalid-assumption
  (math-report-reason (φ:simplify "1" #:assumptions '("x+1"))))
(test-equal "type declarations name variables rather than computed expressions"
  'invalid-assumption
  (math-report-reason (φ:simplify "1" #:assumptions '("scalar(x+1)"))))
(test-assert "facts retain the supported declaration, regularity and equation forms"
  (every math-assumption?
    (map φ:term '("vector(E)" "scalar(x)" "smooth(E)" "div(E)=0"))))
(let ((result (math-report-result
                (φ:simplify "E+zero(vector)" #:assumptions '("vector(E)")))))
  (test-assert "retained display premises still render a vector accent"
    (string-contains (sxml->html (content->sxml
      (make-content-element 'math (list (math-value->layout result))))) "<mover"))
  (test-equal "display premises do not authorize another computation"
    'type-error (math-report-reason (φ:simplify result))))
(with-math-assumptions (φ:assumptions "scalar(x)")
  (with-math-assumptions (φ:assumptions "vector(x)")
    (test-equal "conflicting child contexts cannot silently shadow the parent"
      'type-error (math-report-reason (φ:calculate "1+1"))))
  (test-assert "the parent context remains usable after a conflicting child"
    (math-report-complete? (φ:simplify "x+0"))))
(for-each
  (lambda (term)
    (test-assert "reserved operator arities reject malformed source-independent terms"
      (not (math-term? term))))
  '((curl (var E) (var B)) (differentiate (var x)) (+ (var x)) (smooth)))
(test-equal "type signatures remain usable relationally with an unknown operator"
  '(+ - * / ^)
  (run* (operator) (math-typeo scalar-x `(,operator (var x) (num "2")) 'scalar)))
(test-end "mathematical expression contracts")
