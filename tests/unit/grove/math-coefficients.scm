;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (grove math syntax) (grove math term) (grove math source)
             (grove math layout) (grove math pipeline) (grove math expression)
             ((grove math semantic) #:prefix semantic:)
             (grove ir) (grove render html)
             (srfi srfi-13) (srfi srfi-64))
(test-begin "implicit numeric coefficients")
(for-each
  (lambda (pair)
    (test-equal (string-append "same multiplication term: " (car pair))
      (math-expression->term (parse-math (cadr pair)))
      (math-expression->term (parse-math (car pair)))))
  '(("6x" "6*x") ("6x^2" "6*x^2") ("6(x+1)" "6*(x+1)")
    ("6sin(x)" "6*sin(x)") ("-6x" "-6*x") ("6 x" "6*x")
    ("6x/2" "6*x/2") ("(1/2)*6x" "(1/2)*6*x")
    ("6x+2y" "6*x+2*y") ("6x^2_i" "6*x^2_i")))
(test-equal "multi-letter variables remain whole" '(var xy) (math-source->term "xy"))
(test-equal "function calls remain calls" '(f (var x)) (math-source->term "f(x)"))
(test-equal "implicit multiplication preserves source spans" '(0 . 4)
  (let ((expression (parse-math "6x^2")))
    (cons (math-expression-start expression) (math-expression-end expression))))
(for-each (lambda (source) (test-error source #t (math-source->term source)))
  '("1/2x" "x y" "2 3" "x^2y"))
(test-equal "embedded semantic operands accept coefficients"
  '(* (num "6") (var x))
  (math-value-term (semantic:expression "6" (semantic:variable 'x))))
(define (html source)
  (sxml->html (content->sxml
    (make-content-element 'math (list (math-term->layout (math-source->term source)))))))
(test-assert "coefficients render without a multiplication sign"
  (string-contains (html "6*x") "<mn>6</mn><mi>x</mi>"))
(test-assert "negative coefficients retain their sign without extra grouping"
  (let ((rendered (html "-6x")))
    (and (string-contains rendered "<mo>−</mo><mn>6</mn>")
         (not (string-contains rendered "<mo>×</mo>"))
         (not (string-contains rendered "<mo>(</mo>")))))
(test-assert "powers retain their exponent and omit the coefficient sign"
  (string-contains (html "3*x^2") "<mn>3</mn><msup>"))
(test-assert "sums retain grouping after coefficients"
  (string-contains (html "2*(x+1)") "<mn>2</mn><mrow><mo>(</mo>"))
(test-assert "number products retain a multiplication sign"
  (string-contains (html "6*2") "<mn>6</mn><mo>×</mo><mn>2</mn>"))
(test-assert "numeric powers cannot look like a larger numeric base"
  (string-contains (html "6*2^3") "<mn>6</mn><mo>×</mo><msup>"))
(test-assert "cross products retain their symbol"
  (string-contains (html "cross(E,B)") "<mo>×</mo>"))
(test-equal "implicit coefficients participate in differentiation"
  '(+ (* (num "3") (^ (var x) (num "2"))) (num "2"))
  (math-value-term (math-report-result
    (math-differentiate "x^3+2x" "x" #:assumptions '("coordinate(x)")))))
(test-end "implicit numeric coefficients")
