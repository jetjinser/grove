;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math semantic) #:prefix semantic:)
             ((grove math formula) #:prefix φ:)
             (grove math provenance)
             (grove math expression) (grove math private source)
             (grove math private syntax) (grove math term) (srfi srfi-64))

(define (term . fragments) (math-value-term (apply φ:read fragments)))
(define (located text line column)
  (let ((text (string-copy text)))
    (set-source-properties! text `((filename . "formula.plant") (line . ,line) (column . ,column))) text))

(test-begin "Semantic DSL")
(test-error "Semantic constructors reject Formula text" #t (semantic:add "x" "x"))
(test-error "Semantic calls require a function identifier" #t
  (semantic:call (semantic:add (semantic:variable 'f) (semantic:variable 'g))
    (semantic:variable 'x)))
(test-equal "constructing an expression does not simplify it"
  '(+ (var x) (var x)) (math-value-term (φ:add "x" "x")))
(test-equal "mixed commands and text lower to the same term as plain text"
  (term "x^3 + 2*x") (term (φ:power "x" "3") " + 2*" (semantic:variable 'x)))
(test-equal "an embedded sum remains one operand under multiplication"
  (term "(x+y)*2") (term (φ:add "x" "y") "*2"))
(test-equal "an embedded quotient remains one operand under power"
  (term "(x/y)^3") (term (φ:divide "x" "y") "^3"))
(test-equal "adjacent text fragments do not split identifiers"
  '(var alpha) (term "al" "pha"))
(test-equal "a Scheme list is a sequence of mathematical fragments"
  (term "x+2*y") (term (list (semantic:variable 'x) "+2*" (semantic:variable 'y))))
(test-equal "Unicode minus shares the semantic subtraction operator"
  (term "x-y") (term "x−y"))
(test-equal "unary minus binds outside a natural power"
  '(- (^ (var x) (num "2"))) (term "-x^2"))
(test-equal "power remains right associative"
  '(^ (var x) (^ (var y) (num "2"))) (term "x^y^2"))
(test-equal "constants retain exact rational structure"
  '(- (/ (num "5") (num "6"))) (math-value-term (semantic:constant -5/6)))
(test-error "inexact constants are not silently rationalized" #t (semantic:constant 0.5))
(test-error "variable declarations require identifiers" #t (semantic:variable "x+y"))
(test-error "juxtaposed operands require an explicit multiplication operator" #t
  (term "x" (semantic:variable 'y)))
(test-error "ambiguous textual subscripts do not acquire a guessed meaning" #t
  (term "E_x"))
(test-error "reserved vector operators validate their arity" #t
  (term "curl(E,F)"))
(test-error "reserved differentiation validates its arity" #t
  (term "differentiate(x)"))
(test-equal "vector operators compose without presentation nodes"
  '(curl (curl (var E))) (math-value-term (φ:curl (φ:curl "E"))))
(test-equal "a formal derivative has explicit operands"
  '(differentiate (^ (var x) (num "3")) (var x))
  (math-value-term (φ:derivative (φ:power "x" "3") "x")))
(let* ((value (φ:group "x+x"))
       (copy (math-value-with-properties value '((variant . normal)))))
  (test-equal "presentation preferences do not change mathematical identity"
    (math-value-term value) (math-value-term copy))
  (test-equal "explicit grouping is recorded beside the term"
    #t (assq-ref (math-value-properties value) 'parenthesized?)))
(let* ((child (φ:divide (located "2" 4 10) (located "0" 5 8)))
       (parent (φ:read (located "x + " 3 2) child))
       (origin (math-value-origin parent)))
  (test-equal "embedded constructors retain a nested operand's source line"
    6 (math-location-line (math-origin-location origin '(1 1))))
  (test-equal "embedded constructors retain a nested operand's source column"
    9 (math-location-column (math-origin-location origin '(1 1))))
  (test-equal "text operands retain their independent source positions"
    4 (math-location-line (math-origin-location origin '(0)))))
(test-end "Semantic DSL")
