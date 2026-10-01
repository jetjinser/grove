;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (grove math pipeline) (grove math types) (grove math expression)
             (grove math context) (grove math rewrite) (grove math partial)
             (grove math layout) (grove math term) (grove math premise)
             (grove ir) (grove render html) (grove render text)
             ((grove math semantic) #:prefix ◆:)
             ((grove math notation) #:prefix ◇:)
             (language scribble spec) (language scribble parser) (system base language)
             (srfi srfi-1) (srfi srfi-13) (srfi srfi-64))
(define r3 (◆:space 'x 'y 'z))
(define axes (list r3 "parameter(a)"))
(define v (◆:components "x^2" "y+a" "sin(z)" #:space r3))
(define (html value) (sxml->html (content->sxml value)))
(define (document source)
  (scribble-evaluate-document (open-input-string source)
    (default-environment scribble) "notes/components.plant"))
(define (derive source variable)
  (math-partial source variable #:assumptions axes))
(test-begin "Cartesian components and partial derivatives")
(test-equal "components are three ordinary scalar terms"
  '(components (^ (var x) (num "2")) (+ (var y) (var a)) (sin (var z)))
  (math-value-term v))
(test-equal "components retain their ordered basis as a requirement"
  (list (math-value-term r3)) (math-value-requirements v))
(test-equal "components do not silently declare their space" '() (math-value-declarations v))
(test-equal "components infer a vector result" 'vector
  (math-typed-term-type (math-infer-type (math-assumptions axes) (math-value-term v))))
(test-error "components require an explicitly chosen space" #t (◆:components "x" "y" "z"))
(test-error "a scalar is not a component basis" #t
  (◆:components "x" "y" "z" #:space (◆:constant 3)))
(test-error "component count is structural" #t (math-source->term "components(x,y)"))
(test-equal "a vector cannot be a scalar component" 'type-error
  (math-report-reason (math-simplify "components(E,y,z)" #:assumptions (list r3 "vector(E)"))))
(test-equal "a raw component term requires an active space" 'missing-premise
  (math-report-reason (math-simplify "components(1,2,3)")))
(test-assert "component constructors preserve intrinsic scalar domains"
  (member '(nonzero (var x))
          (math-value-requirements (◆:components "1/x" "y" "z" #:space r3))))
(let ((report (math-simplify "components(x,y,1/z)" #:assumptions (list r3))))
  (test-equal "undefined component expressions prevent vector computation" 'missing-premise
    (math-report-reason report))
  (test-equal "component domain diagnostics identify the third entry's denominator" '(2 1)
    (math-diagnostic-path (car (math-report-diagnostics report)))))
(test-assert "explicit domain premises authorize component computations"
  (math-report-complete? (math-simplify "components(x,y,1/z)"
                           #:assumptions (list r3 "nonzero(z)"))))
(test-equal "a bound vector cannot be reused in another basis" 'missing-premise
  (math-report-reason (math-simplify v #:assumptions (list (◆:space 'u 'v 'w) "coordinate(x); coordinate(y); coordinate(z); parameter(a)"))))
(test-equal "scalar simplification traverses component entries"
  '(components (var x) (var y) (num "0"))
  (math-report-final (math-simplify "components(x+0,y*1,2-2)" #:assumptions (list r3))))
(let ((result (math-report-result (math-simplify "components(x+0,y*1,z)" #:assumptions (list r3)))))
  (test-assert "raw component results acquire and retain the active basis"
    (member (math-value-term r3) (math-value-requirements result)))
  (test-equal "raw component results cannot silently change their basis" 'missing-premise
    (math-report-reason (math-simplify result
      #:assumptions (list (◆:space 'u 'v 'w) "coordinate(x); coordinate(y); coordinate(z)")))))
(test-assert "regularity follows component expressions"
  (math-premise-known? (math-assumptions axes) `(smooth ,(math-value-term v))))
(test-assert "column rendering contains three native MathML rows"
  (let ((rendered (html (◇:math v))))
    (and (string-contains rendered "<mtable>")
         (= 3 (let count ((offset 0) (n 0))
                (let ((next (string-contains rendered "<mtr>" offset)))
                  (if next (count (+ next 5) (+ n 1)) n))))
         (string-contains rendered "<msup>"))))
(test-equal "text rendering preserves component boundaries" "(x, y, z)"
  (content->text (◇:math (◆:components "x" "y" "z" #:space r3))))
(test-assert "notation columns remain nonsemantic"
  (and (string-contains (html (◇:math (◇:column "a+b" "c"))) "<mtable>")
       (not (math-value? (◇:column "a" "b")))))
(for-each
  (lambda (fixture)
    (let* ((report (derive (car fixture) (cadr fixture)))
           (certificate (math-report-derivation report)))
      (test-assert (string-append "partial completes: " (car fixture)) (math-report-complete? report))
      (test-equal "partial has the expected simplified result"
        (math-source->term (caddr fixture)) (math-report-final report))
      (test-assert "both partial and simplification passes are explicit"
        (equal? '(partial simplify) (map math-report-name (math-report-passes report))))
      (test-assert "partial certificates independently replay"
        (math-verification-valid? (math-verify-derivation certificate)))
      (test-assert "every pass independently replays"
        (every (lambda (pass) (math-verification-valid? (math-verify-derivation (math-report-derivation pass))))
               (math-report-passes report)))
      (test-assert "generated partial steps preserve their source"
        (and (= (+ 1 (length (math-derivation-steps certificate)))
                (length (math-derivation-sources certificate)))
             (every math-origin? (math-derivation-sources certificate))))))
  '(("7" "x" "0") ("x" "x" "1") ("y" "x" "0") ("a" "z" "0")
    ("x^3+2x*y+a" "x" "3*x^2+2*y") ("x^2*y" "y" "x^2")
    ("sin(x*y)" "x" "y*cos(x*y)") ("exp(a*z)" "z" "a*exp(a*z)")
    ("cos(x)" "x" "-sin(x)") ("x^0" "x" "0")))
(test-equal "parameters are not differentiation coordinates" 'unsupported-expression
  (math-report-reason (derive "x" "a")))
(test-equal "a generic scalar declaration does not authorize a partial coordinate" 'missing-premise
  (math-report-reason (math-partial "x" "x" #:assumptions '("scalar(x)"))))
(test-equal "an expression cannot serve as a coordinate" 'unsupported-expression
  (math-report-reason (derive "x" "x+y")))
(test-equal "partial derivatives require a scalar expression" 'type-error
  (math-report-reason (math-partial v "x" #:assumptions axes)))
(test-equal "logarithm retains its real domain" 'missing-premise
  (math-report-reason (derive "log(x)" "x")))
(test-equal "zero factors cannot conceal an undefined expression" 'division-by-zero
  (math-report-reason (derive "0*(1/0)" "x")))
(test-equal "unsupported operators do not become inert partial answers" 'unsupported-expression
  (math-report-reason (derive "opaque(x)" "x")))
(define unknown-facts '("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar); differentiable(f)"))
(let ((report (math-partial "x*f(x,y)" "x" #:assumptions unknown-facts)))
  (test-equal "unknown multivariate derivatives remain explicit in computed products"
    (math-source->term "x*darg(f(x,y),1)+f(x,y)") (math-report-final report))
  (test-assert "symbolic partial results still replay"
    (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
  (test-assert "symbolic results preserve C1 requirements"
    (member '(differentiable (* (var x) (f (var x) (var y))))
            (math-value-requirements (math-report-result report))))
  (test-equal "reusing a symbolic result cannot discard C1" 'missing-premise
    (math-report-reason (math-simplify (math-report-result report)
      #:assumptions '("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar)")))))
(test-equal "undeclared regularity is a diagnostic, not an authorization" 'missing-premise
  (math-report-reason (math-partial "f(x,y)" "x"
    #:assumptions '("coordinate(x); coordinate(y); function(f,scalar,scalar,scalar)"))))
(test-equal "second unknown derivatives do not follow from C1" 'missing-premise
  (math-report-reason (math-partial "partial(f(x,y),x)" "y" #:assumptions unknown-facts)))
(test-equal "a scalar field symbol is not silently an independent coordinate"
  (math-source->term "partial(u,x)")
  (math-report-final (math-partial "u" "x" #:assumptions '("coordinate(x); scalar(u); differentiable(u)"))))
(let ((result (math-report-result (math-partial "a" "x" #:assumptions axes))))
  (test-assert "a parameter's role survives reduction to zero"
    (member '(parameter (var a)) (math-value-requirements result)))
  (test-equal "reusing zero cannot silently reinterpret a parameter as a field" 'missing-premise
    (math-report-reason (math-simplify result #:assumptions '("coordinate(x); scalar(a); differentiable(a)")))))
(test-equal "declared field derivatives stay symbolic in their space"
  (math-source->term "darg(f(x,y,z),1)")
  (math-report-final (math-partial "f(x,y,z)" "x"
    #:assumptions (list r3 (◆:field 'f #:space r3) "differentiable(f)"))))
(test-equal "elementary chain rules can retain an unknown inner partial"
  (math-source->term "cos(f(x,y))*darg(f(x,y),1)")
  (math-report-final (math-partial "sin(f(x,y))" "x" #:assumptions unknown-facts)))
(test-equal "operator-named coordinates are not renamed by the partial adapter"
  '(num "1")
  (math-report-final (math-partial "differentiate" "differentiate"
                      #:assumptions '("coordinate(differentiate)"))))
(test-equal "variable exponents remain outside the computable domain" 'unsupported-expression
  (math-report-reason (derive "x^y" "x")))
(test-equal "ordinary differentiation uses the same multivariate function derivative"
  (math-report-final (math-partial "f(x,y)" "x" #:assumptions unknown-facts))
  (math-report-final (math-differentiate "f(x,y)" "x" #:assumptions unknown-facts)))
(test-assert "an authored partial step uses the registered relation"
  (math-verification-valid? (math-check-chain '("partial(x*y,x)" "partial(x,x)*y+x*partial(y,x)")
    #:assumptions (list r3) #:steps '(partial-product))))
(test-assert "target search can use partial rules"
  (pair? (math-report-candidates (math-search "partial(x,x)" "1"
    #:assumptions (list r3) #:rules '(partial-variable) #:max-depth 1))))
(test-equal "partial rules do not rewrite ordinary derivative terms" '()
  (math-rewrite-answers (math-assumptions (list r3)) (math-source->term "differentiate(x,x)") math-partial-rules #f))
(test-error "partial rules do not offer inverse integration" #t
  (math-search-backward "1" "partial(x,x)" #:assumptions (list r3) #:rules '(partial-variable)))
(test-equal "zero step budget preserves the partial input" 'step-limit
  (math-report-reason (math-partial "x^3" "x" #:assumptions (list r3) #:max-steps 0)))
(test-equal "term-size budgets are enforced" 'term-size-limit
  (math-report-reason (math-partial "x^3" "x" #:assumptions (list r3) #:max-term-size 1)))
(test-assert "article commands render partial provenance and native columns"
  (let ((rendered (html (document
    "@use-modules[((grove math semantic) #:prefix ◆:)]\n@(define r3 (◆:space 'x 'y 'z)) @with-assumptions[r3]{@math{@◆:components[\"x\" \"y\" \"z\" #:space r3]} @partial[\"x\"]{x^3+2x*y}}"))))
    (and (string-contains rendered "<mtable>")
         (string-contains rendered "∂")
         (string-contains rendered "data-rule=\"partial-power\"")
         (string-contains rendered "notes/components.plant:"))))
(test-end "Cartesian components and partial derivatives")
