;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove math)
  #:use-module (srfi srfi-64)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
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
  (test-equal "unary minus, powers, and fractions have distinct term forms"
    '(+ (- (^ (var x) (num "2"))) (/ (var y) (num "3")))
    (math-source->term "-x^2 + y/3"))
  (test-equal "equations parse as first-class mathematical terms"
    '(= (div (var E)) (num "0"))
    (math-source->term "div(E)=0"))
  (test-assert "math terms accept the declared symbolic grammar"
    (math-term? '(+ (var x) (* (var y) (num "2")))))
  (test-equal "symbolic terms lower to the separate layout IR"
    'math-row
    (content-element-kind (math-term->layout '(curl (var E)))))
  (test-equal "miniKanren relation derives curl-curl identity"
    '(- (grad (div (var E))) (laplacian (var E)))
    (rewrite-curl-curl '(curl (curl (var E)))))
  (test-equal "conditional rewrite uses a declared divergence-free fact"
    '(- (num "0") (laplacian (var E)))
    (car (run 1 (result)
           (rewriteo '((= (div (var E)) (num "0")))
                     '(- (grad (div (var E))) (laplacian (var E)))
                     result))))
  (test-equal "conditional rewrite does not use a fact about another field"
    '()
    (run* (result)
      (rewriteo '((= (div (var F)) (num "0")))
                '(grad (div (var E)))
                result)))
  (test-equal "curl-curl relation leaves a non-matching term without answer"
    #f
    (rewrite-curl-curl '(curl (var E))))
  (let* ((derivation (math-derive-curl-curl "curl(curl(E))"))
         (html (sxml->html (content->sxml derivation))))
    (test-assert "lowering creates a display MathML alignment"
      (and (string-contains html "<mtable>")
           (string-contains html "<mo>∇</mo>")
           (string-contains html "<mo>×</mo>")
           (string-contains html "<mo>·</mo>")
           (string-contains html "Δ"))))
  (test-assert "generic function calls use MathML function application"
    (string-contains
      (sxml->html
        (content->sxml
          (make-content-element 'math (list (math-term->layout '(sin (var x)))))))
      "<mo>⁡</mo>"))
  (test-end "grove math pipeline"))

(run-math-tests)
