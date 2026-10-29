;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) ((grove math semantic) #:prefix ◆:)
             (grove math expression) (grove math calculation) (grove math types)
             (grove math context)
             (grove render html) (language scribble spec) (system base language)
             (srfi srfi-64) (srfi srfi-13))
(define x (◆:coordinate 'x))
(define a (◆:parameter 'a))
(define f (◆:function 'f))
(define formula (φ:call f (φ:power x "2")))
(test-begin "mathematical declarations and function applications")
(test-equal "a coordinate is an ordinary reusable mathematical variable" '(var x) (math-value-term x))
(test-equal "declarations are separate from display preferences" '() (math-value-properties x))
(test-equal "declarations do not implicitly enter a calculation"
  'type-error (math-report-reason (φ:simplify x)))
(with-math-assumptions (list x a f)
  (test-equal "explicit declaration scopes infer coordinate type" 'scalar
    (math-typed-term-type (math-infer-type (current-math-assumptions) (math-value-term x))))
  (test-equal "explicit declaration scopes infer function-reference type" 'function
    (math-typed-term-type (math-infer-type (current-math-assumptions) (math-value-term f))))
  (test-equal "declared calls have their specified return type" 'scalar
    (math-typed-term-type (math-infer-type (current-math-assumptions) (math-value-term formula))))
  (test-equal "parameters are constant relative to independent coordinates" '(num "0")
    (math-report-final (φ:differentiate a x)))
  (test-equal "a parameter cannot masquerade as a differentiation coordinate" 'unsupported-expression
    (math-report-reason (φ:differentiate a a)))
  (test-assert "independent replay also rejects differentiation with respect to a parameter"
    (not (math-verification-valid? (φ:check-chain '("differentiate(a,a)" "1")))))
  (test-equal "declared applications participate in typed algebraic identities" (math-value-term formula)
    (math-report-final (φ:simplify (φ:add formula "0"))))
  (test-equal "declared signatures enforce arity" 'type-error
    (math-report-reason (φ:simplify "f(x,a)")))
  (test-equal "declared signatures enforce operand types" 'type-error
    (math-report-reason (φ:simplify "f(f)")))
  (test-equal "a function reference cannot act as an independent scalar variable" 'type-error
    (math-report-reason (φ:differentiate f x))))
(test-equal "the declaration scope restores its parent" '() (current-math-assumptions))
(test-equal "composed expressions retain explicit declarations for reuse"
  '((function (var f) (var scalar) (var scalar)) (coordinate (var x)))
  (math-value-declarations formula))
(with-math-assumptions (list formula)
  (test-assert "an expression may explicitly provide the declarations it carries"
    (math-report-complete? (φ:simplify formula))))
(with-math-assumptions (φ:assumptions "function(v,scalar,scalar,vector);coordinate(x);parameter(a)")
  (test-equal "multiargument function signatures specify the return type" 'vector
    (math-typed-term-type (math-infer-type (current-math-assumptions) (φ:term "v(x,a)"))))
  (test-equal "a declared vector-valued call participates in vector identities"
    (φ:term "v(x,a)") (math-report-final (φ:simplify "v(x,a)+zero(vector)"))))
(for-each
  (lambda (facts)
    (test-equal "contradictory or malformed declarations invalidate the whole scope" 'invalid-assumption
      (math-report-reason (φ:simplify "1" #:assumptions (list facts)))))
  '("coordinate(x);parameter(x)" "scalar(f);function(f,scalar,scalar)"
    "function(f,scalar,scalar);function(f,vector,vector)" "function(curl,scalar,scalar)"
    "function(f,unknown,scalar)"))
(test-error "function constructors validate unsupported signature types" #t (◆:function 'f #:inputs '(unknown)))
(test-equal "an undeclared function remains displayable but lacks computational semantics" 'unsupported-expression
  (math-report-reason (φ:simplify "g(x)" #:assumptions '("coordinate(x)"))))
(let* ((source "@use-modules[((grove math formula) #:prefix φ:) ((grove math semantic) #:prefix ◆:)]\n@(define x (◆:coordinate 'x))\n@(define a (◆:parameter 'a))\n@(define f (◆:function 'f))\n@with-assumptions[(list x a f)]{@math{@φ:call[f]{x^2}} @differentiate[x]{@φ:multiply[a x]}}")
       (document (scribble-evaluate-document (open-input-string source) (default-environment scribble) "declarations.plant")))
  (test-assert "article declaration scopes reuse values through at-exp constructors"
    (string-contains (sxml->html (content->sxml document)) "<mi>f</mi>")))
(test-end "mathematical declarations and function applications")
