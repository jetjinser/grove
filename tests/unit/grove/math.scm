;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove math)
  #:use-module (srfi srfi-64)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove math pipeline)
  #:use-module (grove relation)
  #:use-module (grove render html)
  #:use-module (grove ir)
  #:export (run-math-tests))

(define (run-math-tests)
  (test-begin "grove math pipeline")
  (let* ((tokens (tokenize-math "curl(curl(E))"))
         (expression (parse-math "curl(curl(E))")))
    (test-equal "lexer records tokens with source spans"
      '(identifier left-paren identifier left-paren identifier
        right-paren right-paren eof)
      (map math-token-kind tokens))
    (test-equal "parser retains the complete source span"
      '(0 . 13)
      (cons (math-expression-start expression)
            (math-expression-end expression)))
    (test-equal "syntax trees lower to symbolic Scheme terms"
      '(curl (curl (var E)))
      (math-expression->term expression)))
  (test-equal "precedence is resolved before term lowering"
    '(+ (var x) (* (var y) (num "2")))
    (math-source->term "x + y * 2"))
  (test-equal "underscore tokenizes as a script marker between identifiers"
    '(identifier subscript identifier eof)
    (map math-token-kind (tokenize-math "E_x")))
  (test-equal "subscript source lowers to a distinct symbolic term"
    '(sub (var E) (var x))
    (math-source->term "E_x"))
  (test-equal "superscript and subscript attach to the same base in order"
    '(sub (^ (var x) (num "2")) (var i))
    (math-source->term "x^2_i"))
  (test-equal "grouping keeps a compound subscript inside an exponent"
    '(^ (var x) (sub (var a) (var i)))
    (math-source->term "x^(a_i)"))
  (test-equal "unary minus, powers, and fractions have distinct term forms"
    '(+ (- (^ (var x) (num "2"))) (/ (var y) (num "3")))
    (math-source->term "-x^2 + y/3"))
  (test-equal "equations parse as first-class mathematical terms"
    '(= (div (var E)) (num "0"))
    (math-source->term "div(E)=0"))
  (test-assert "math terms accept the declared symbolic grammar"
    (math-term? '(+ (var x) (* (var y) (num "2")))))
  (test-equal "numbers and declared variables receive scalar and vector types"
    '(scalar vector)
    (list (math-typed-term-type
            (math-infer-type '((scalar (var x))) '(+ (var x) (num "2"))))
          (math-typed-term-type
            (math-infer-type '((vector (var E))) '(curl (var E))))))
  (test-equal "type inference rejects curl applied to a scalar field"
    #f
    (math-infer-type '((scalar (var E))) '(curl (var E))))
  (test-equal "type inference rejects mixed scalar-vector addition"
    #f
    (math-infer-type '((scalar (var x)) (vector (var E)))
                     '(+ (var x) (var E))))
  (test-equal "type inference rejects conflicting declarations"
    #f
    (math-infer-type '((scalar (var x)) (vector (var x))) '(var x)))
  (test-equal "type diagnostics identify the required vector declaration"
    "curl requires a vector operand; add vector(E)"
    (math-type-diagnostic '() '(curl (var E))))
  (test-error "@derive reports missing type declarations" #t
    (math-derive "curl(curl(E))"))
  (test-error "@derive reports a missing regularity assumption" #t
    (math-derive "curl(curl(E))"
                 #:assumptions '("vector(E)")))
  (test-equal "symbolic terms lower to the separate layout IR"
    'math-row
    (content-element-kind (math-term->layout '(curl (var E)))))
  (test-assert "subscript layout lowers to the MathML msub element"
    (string-contains
      (sxml->html
        (content->sxml
          (make-content-element 'math
            (list (math-term->layout '(sub (var E) (var x)))))))
      "<msub>"))
  (test-equal "miniKanren relation derives curl-curl identity"
    '((- (grad (div (var E))) (laplacian (var E))) curl-curl-identity ())
    (car (run 1 (result rule path)
           (rewrite-stepo '((vector (var E)) (smooth (var E)))
                          '(curl (curl (var E))) result rule path))))
  (test-equal "curl-curl helper enforces vector and regularity premises"
    '(- (grad (div (var E))) (laplacian (var E)))
    (rewrite-curl-curl '(curl (curl (var E)))
                       #:assumptions '((vector (var E)) (smooth (var E)))))
  (test-equal "curl-curl helper rejects a missing regularity premise"
    #f
    (rewrite-curl-curl '(curl (curl (var E)))
                       #:assumptions '((vector (var E)))))
  (test-equal "conditional rewrite uses a declared divergence-free fact"
    '(- (num "0") (laplacian (var E)))
    (car (run 1 (result)
           (rewriteo '((vector (var E))
                       (= (div (var E)) (num "0")))
                     '(- (grad (div (var E))) (laplacian (var E)))
                     result))))
  (test-equal "named rewrite records the nested operand path"
    '((- (num "0") (laplacian (var E))) divergence-free (0))
    (car (run 1 (result rule path)
           (rewrite-stepo '((vector (var E))
                            (= (div (var E)) (num "0")))
                          '(- (grad (div (var E))) (laplacian (var E)))
                          result rule path))))
  (test-equal "curl-curl rewrite records its rule at the root"
    '((- (grad (div (var E))) (laplacian (var E))) curl-curl-identity ())
    (car (run 1 (result rule path)
           (rewrite-stepo '((vector (var E)) (smooth (var E)))
                          '(curl (curl (var E))) result rule path))))
  (test-equal "subtraction simplification is named at the root"
    '((- (var x)) subtract-zero ())
    (car (run 1 (result rule path)
           (rewrite-stepo '() '(- (num "0") (var x)) result rule path))))
  (test-equal "rewrite relations expose a contract for every current rule"
    '(curl-curl-identity divergence-free subtract-zero)
    (map math-rewrite-rule-name math-rewrite-rules))
  (test-assert "curl-curl side conditions are matched against typed assumptions"
    (and (math-rewrite-rule? (math-rewrite-rule-ref 'curl-curl-identity))
         (string-contains
           (math-rewrite-rule-condition
             (math-rewrite-rule-ref 'curl-curl-identity))
           "vector(E)")
         (math-rewrite-rule-condition-checked?
           (math-rewrite-rule-ref 'curl-curl-identity))))
  (test-assert "the divergence-free relation checks its declared assumption"
    (math-rewrite-rule-condition-checked?
      (math-rewrite-rule-ref 'divergence-free)))
  (test-equal "conditional rewrite does not use a fact about another field"
    '()
    (run* (result)
      (rewriteo '((vector (var F)) (= (div (var F)) (num "0")))
                '(grad (div (var E)))
                result)))
  (test-equal "curl-curl relation leaves a non-matching term without answer"
    '()
    (run* (result rule path)
      (rewrite-stepo '((vector (var E)))
                     '(curl (curl (var E))) result rule path)))
  (let* ((derivation
           (math-derive-curl-curl "curl(curl(E))"
                                  #:assumptions
                                  '("vector(E); smooth(E)")))
         (html (sxml->html (content->sxml derivation))))
    (test-assert "lowering creates a display MathML alignment"
      (and (string-contains html "<mtable>")
           (string-contains html "<mo>∇</mo>")
           (string-contains html "<mo>×</mo>")
           (string-contains html "<mo>·</mo>")
           (string-contains html "<mover accent=\"true\">")
           (string-contains html "Δ")
           (string-contains html "curl-curl-identity (at root)"))))
  (test-assert "generic function calls use MathML function application"
    (string-contains
      (sxml->html
        (content->sxml
          (make-content-element 'math (list (math-term->layout '(sin (var x)))))))
      "<mo>⁡</mo>"))
  (test-end "grove math pipeline"))

(run-math-tests)
