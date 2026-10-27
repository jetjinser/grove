;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove math)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove math syntax)
  #:use-module ((grove math formula) #:prefix φ:)
  #:use-module (grove math expression)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module ((grove math notation) #:prefix notation:)
  #:use-module (grove math pipeline)
  #:use-module ((grove math derivation)
                #:select (make-math-derivation make-math-derivation-step))
  #:use-module (grove relation)
  #:use-module (grove render html)
  #:use-module (grove ir)
  #:export (run-math-tests))

(define (search-candidates . arguments)
  (math-report-candidates (apply φ:search arguments)))

(define (backward-candidates target source . options)
  (math-report-candidates
    (apply φ:search
      (append (list source target) options (list #:backward? #t)))))

(define (diagnostic-codes verification)
  (map math-diagnostic-code (math-verification-diagnostics verification)))

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
    (test-equal "Formula DSL reads to symbolic Scheme terms"
      '(curl (curl (var E)))
      (math-value-term (φ:read "curl(curl(E))"))))
  (test-equal "precedence is resolved before term lowering"
    '(+ (var x) (* (var y) (num "2")))
    (φ:term "x + y * 2"))
  (let* ((report
           (math-differentiate (φ:read "x^3+2*x") (φ:read "x")
             #:assumptions (φ:assumptions "coordinate(x)")))
         (notation (math-report-result->notation report)))
    (test-equal "a completed report lowers directly to inspectable Notation IR"
      '(math-row
         (math-row
           (math-token (properties (token-kind . mn)) "3")
           (math-sup
             (math-row (math-token (properties (token-kind . mi)) "x"))
             (math-row (math-token (properties (token-kind . mn)) "2"))))
         (math-token (properties (token-kind . mo)) "+")
         (math-token (properties (token-kind . mn)) "2"))
      (math-notation->datum notation))
    (test-error "math report result conversion rejects rendered content"
      #t (math-report-result notation)))
  (test-equal "underscore tokenizes as a script marker between identifiers"
    '(identifier subscript identifier eof)
    (map math-token-kind (tokenize-math "E_x")))
  (test-error "Semantic Formula DSL rejects layout-only subscripts" #t (φ:read "E_x"))
  (test-equal "unary minus, powers, and fractions have distinct term forms"
    '(+ (- (^ (var x) (num "2"))) (/ (var y) (num "3")))
    (φ:term "-x^2 + y/3"))
  (test-equal "equations parse as first-class mathematical terms"
    '(= (div (var E)) (num "0"))
    (φ:term "div(E)=0"))
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
  (test-equal "zero minus a vector is typed as vector negation"
    'vector
    (math-typed-term-type
      (math-infer-type '((vector (var E)))
                       '(- (zero vector) (laplacian (var E))))))
  (test-equal "vector zero has an explicit symbolic representation"
    '(zero vector) (φ:term "zero(vector)"))
  (test-equal "scalar zero cannot be subtracted from a vector"
    #f (math-infer-type '((vector (var E))) '(- (num "0") (var E))))
  (test-equal "vector zero supports vector equations and addition"
    '(vector proposition)
    (map (lambda (term)
           (math-typed-term-type (math-infer-type '((vector (var E))) term)))
         '((+ (var E) (zero vector)) (= (var E) (zero vector)))))
  (test-assert "vector zero renders with a vector accent"
    (string-contains
      (sxml->html (content->sxml
        (make-content-element 'math (list (math-term->layout '(zero vector))))))
      "<mover accent=\"true\">"))
  (test-equal "type inference rejects conflicting declarations"
    #f
    (math-infer-type '((scalar (var x)) (vector (var x))) '(var x)))
  (test-equal "type diagnostics identify the required vector declaration"
    "curl requires a vector operand; add vector(E)"
    (math-type-diagnostic '() '(curl (var E))))
  (test-equal "Formula derive returns a report for missing type declarations"
    'type-error (math-report-reason (φ:derive "curl(curl(E))")))
  (test-equal "Formula derive returns a report for a missing regularity assumption"
    'missing-premise
    (math-report-reason (φ:derive "curl(curl(E))"
                                 #:assumptions '("vector(E)"))))
  (test-equal "symbolic terms lower to the separate layout IR"
    'math-row
    (content-element-kind (math-term->layout '(curl (var E)))))
  (test-assert "subscript layout lowers to the MathML msub element"
    (string-contains
      (sxml->html
        (content->sxml
          (notation:math (notation:sub "E" "x"))))
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
    '(- (zero vector) (laplacian (var E)))
    (car (run 1 (result)
           (rewriteo '((vector (var E))
                       (= (div (var E)) (num "0")))
                     '(- (grad (div (var E))) (laplacian (var E)))
                     result))))
  (test-equal "divergence of curl rewrites to scalar zero for a smooth vector field"
    '((num "0") div-curl-zero ())
    (car (run 1 (result rule path)
           (rewrite-stepo '((vector (var E)) (smooth (var E)))
                          '(div (curl (var E))) result rule path))))
  (test-equal "divergence of curl requires a matching smoothness assumption"
    '()
    (run* (result rule path)
      (rewrite-stepo '((vector (var E)))
                     '(div (curl (var E))) result rule path)))
  (test-equal "divergence of curl rejects a scalar field"
    '()
    (run* (result rule path)
      (rewrite-stepo '((scalar (var E)) (smooth (var E)))
                     '(div (curl (var E))) result rule path)))
  (test-equal "named rewrite records the nested operand path"
    '((- (zero vector) (laplacian (var E))) divergence-free (0))
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
           (rewrite-stepo '((scalar (var x))) '(- (num "0") (var x)) result rule path))))
  (test-equal "rewrite relations expose a contract for every current rule"
    '(curl-curl-identity divergence-free div-curl-zero curl-grad-zero div-grad-laplacian
      grad-product div-product curl-product
      subtract-zero add-zero zero-add subtract-right-zero double-negation multiply-one one-multiply
      multiply-zero zero-multiply power-one power-zero
      add-commutative multiply-associative add-associative multiply-commutative
      differentiate-constant differentiate-variable differentiate-independent
      differentiate-negation differentiate-sum differentiate-difference
      differentiate-product differentiate-power differentiate-quotient differentiate-sin
      differentiate-cos differentiate-exp differentiate-log differentiate-function evaluate-constant collect-like-terms
      combine-coefficients normalize-scalar cancel-scalar-factors cancel-common-factor expand-scalar-product
      normalize-function-prime
      partial-constant partial-variable partial-independent partial-negation partial-sum partial-difference
      partial-product partial-power partial-quotient partial-sin partial-cos partial-exp partial-log partial-function-chain
      partial-coordinate-symmetry partial-argument-symmetry
      components-sum components-difference components-negation components-scale-left components-scale-right
      components-quotient components-dot components-cross components-zero
      gradient-components divergence-components curl-components laplacian-scalar-components laplacian-vector-components integrate-polynomial solve-polynomial-poisson expand-polynomial)
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
  (test-assert "div-curl zero checks vector and regularity assumptions"
    (math-rewrite-rule-condition-checked?
      (math-rewrite-rule-ref 'div-curl-zero)))
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
  (let* ((candidate
           (car (search-candidates "div(curl(E))" "0"
                  #:assumptions '("vector(E); smooth(E)"))))
         (verification (math-verify-derivation candidate)))
    (test-equal "div-curl goal search returns the expected terms"
      '((div (curl (var E))) (num "0"))
      (math-derivation-terms candidate))
    (test-equal "div-curl goal search records its named rule"
      '(div-curl-zero)
      (map math-derivation-step-rule
           (math-derivation-steps candidate)))
    (test-assert "div-curl derivation passes independent verification"
      (math-verification-valid? verification))
    (test-equal "div-curl backward search grounds zero from declared vector fields"
      (math-derivation-terms candidate)
      (math-derivation-terms
        (car (backward-candidates "0" "div(curl(E))"
               #:assumptions '("vector(E); smooth(E)")
               #:max-depth 1)))))
  (let* ((derivation
           (φ:derive->content "curl(curl(E))"
             #:assumptions '("vector(E); smooth(E)")))
         (html (sxml->html (content->sxml derivation))))
    (test-assert "lowering creates a display MathML alignment"
      (and (string-contains html "<mtable>")
           (string-contains html "<mo>∇</mo>")
           (string-contains html "<mo>×</mo>")
           (string-contains html "<mo>·</mo>")
           (string-contains html "<mover accent=\"true\">")
           (string-contains html "Δ")
           (string-contains html "Curl Curl Identity"))))
  (let* ((results
           (search-candidates
             "curl(curl(E))" "-laplacian(E)"
             #:assumptions '("vector(E); smooth(E); div(E)=0")
             #:max-depth 3))
         (result (car results)))
    (test-equal "bounded goal search returns the complete derivation chain"
      '((curl (curl (var E)))
        (- (grad (div (var E))) (laplacian (var E)))
        (- (zero vector) (laplacian (var E)))
        (- (laplacian (var E))))
      (math-derivation-terms result))
    (test-equal "goal search records each named rewrite"
      '(curl-curl-identity divergence-free subtract-zero)
      (map math-derivation-step-rule
           (math-derivation-steps result)))
    (test-assert "a search candidate lowers to an annotated MathML derivation"
      (let ((html
              (sxml->html
                (content->sxml (math-derivation->content result)))))
        (and (string-contains html "<mtable>")
             (string-contains html "<mover accent=\"true\">")
             (string-contains html "Divergence Free"))))
    (test-assert "forward search candidates pass independent derivation checking"
      (math-verification-valid? (math-verify-derivation result)))
    (test-equal "verification retains the candidate derivation"
      result
      (math-verification-derivation (math-verify-derivation result)))
    (test-equal "goal search reports no solution below the required depth"
      '()
      (search-candidates
        "curl(curl(E))" "-laplacian(E)"
        #:assumptions '("vector(E); smooth(E); div(E)=0")
        #:max-depth 2))
    (let* ((backward-results
             (backward-candidates
               "-laplacian(E)" "curl(curl(E))"
               #:assumptions '("vector(E); smooth(E); div(E)=0")
               #:max-depth 3))
           (backward-result (car backward-results)))
      (test-equal "backward query returns a forward-ordered candidate proof"
        (math-derivation-terms result)
        (math-derivation-terms backward-result))
      (test-equal "backward query returns rules in proof order"
        '(curl-curl-identity divergence-free subtract-zero)
        (map math-derivation-step-rule
             (math-derivation-steps backward-result)))
      (test-assert "every backward candidate edge satisfies the forward relation"
        (every
          (lambda (step)
            (member
              (list (math-derivation-step-to step)
                    (math-derivation-step-rule step)
                    (math-derivation-step-path step))
              (run* (output rule path)
                (rewrite-stepo
                  '((vector (var E))
                    (smooth (var E))
                    (= (div (var E)) (num "0")))
                  (math-derivation-step-from step)
                  output rule path))))
          (math-derivation-steps backward-result)))
      (test-assert "backward search candidates pass independent derivation checking"
        (math-verification-valid? (math-verify-derivation backward-result))))
    (test-equal "goal search reports a finite state budget"
      'state-limit
      (math-report-reason (φ:search
        "curl(curl(E))" "-laplacian(E)"
        #:assumptions '("vector(E); smooth(E); div(E)=0")
        #:max-depth 3 #:max-states 1))))
  (let ((results
          (search-candidates
            "(0 - x) - (0 - y)" "(-x) - (-y)"
            #:assumptions '("scalar(x); scalar(y)")
            #:max-depth 2 #:max-results 2)))
    (test-equal "search enumerates both valid rewrite orders"
      2
      (length results))
    (test-equal "result bound limits the number of returned candidates"
      1
      (length
        (search-candidates
          "(0 - x) - (0 - y)" "(-x) - (-y)"
          #:assumptions '("scalar(x); scalar(y)")
          #:max-depth 2 #:max-results 1))))
  (test-assert "step verification confirms the rule, path, and premises"
    (math-verify-step
      '((vector (var E)) (smooth (var E)))
      '(curl (curl (var E)))
      '(- (grad (div (var E))) (laplacian (var E)))
      'curl-curl-identity '()))
  (for-each
    (lambda (example)
      (let* ((source (car example)) (target (cadr example))
             (candidate (car (search-candidates source target
                               #:assumptions '("scalar(x); vector(E)")
                               #:max-depth 1))))
        (test-assert (string-append "typed simplification: " source)
          (math-verification-valid? (math-verify-derivation candidate)))))
    '(("x + 0" "x") ("0 + x" "x") ("x - 0" "x")
      ("-(-x)" "x") ("x * 1" "x") ("1 * x" "x")
      ("E + zero(vector)" "E") ("zero(vector) + E" "E")
      ("E - zero(vector)" "E") ("-(-E)" "E")
      ("E * 1" "E") ("1 * E" "E")))
  (test-assert "nested simplification converges to a type-preserving result"
    (math-verification-valid?
      (math-verify-derivation
        (car (search-candidates "1 * (-(-E) + zero(vector))" "E"
               #:assumptions '("vector(E)") #:max-depth 3)))))
  (test-equal "wrongly typed zero cannot trigger vector addition simplification"
    '()
    (run* (result rule path)
      (rewrite-stepo '((vector (var E))) '(+ (var E) (num "0"))
                     result rule path)))
  (test-equal "step verification rejects an incorrect operand path"
    #f
    (math-verify-step
      '((vector (var E)) (smooth (var E)))
      '(curl (curl (var E)))
      '(- (grad (div (var E))) (laplacian (var E)))
      'curl-curl-identity '(0)))
  (test-equal "step verification rejects a missing regularity premise"
    #f
    (math-verify-step
      '((vector (var E)))
      '(curl (curl (var E)))
      '(- (grad (div (var E))) (laplacian (var E)))
      'curl-curl-identity '()))
  (test-assert "generic function calls use MathML function application"
    (string-contains
      (sxml->html
        (content->sxml
          (make-content-element 'math (list (math-term->layout '(sin (var x)))))))
      "<mo>⁡</mo>"))
  (for-each
    (lambda (example)
      (let* ((source (car example)) (target (cadr example))
             (candidate (car (search-candidates source target
                               #:assumptions '("scalar(f); smooth(f)")
                               #:max-depth 1)))
             (backward (car (backward-candidates target source
                              #:assumptions '("scalar(f); smooth(f)")
                              #:max-depth 1))))
        (test-assert (string-append "scalar-field identity: " source)
          (and (math-verification-valid? (math-verify-derivation candidate))
               (math-verification-valid? (math-verify-derivation backward))))
        (test-equal "scalar-field inverse search preserves forward term order"
          (math-derivation-terms candidate) (math-derivation-terms backward))
        (test-equal "scalar-field identity requires regularity"
          '() (search-candidates source target #:assumptions '("scalar(f)")
                #:max-depth 1))))
    '(("curl(grad(f))" "zero(vector)")
      ("div(grad(f))" "laplacian(f)")))
  (test-equal "smoothness must refer to the exact scalar field"
    '()
    (search-candidates "curl(grad(f))" "zero(vector)"
      #:assumptions '("scalar(f); smooth(g)") #:max-depth 1))
  (test-equal "scalar-field identity rejects vector fields"
    '()
    (run* (result rule path)
      (rewrite-stepo '((vector (var f)) (smooth (var f)))
                     '(curl (grad (var f))) result rule path)))
  (let* ((candidate
           (car (search-candidates "1 * (curl(grad(f)) + zero(vector))"
                  "zero(vector)" #:assumptions '("scalar(f); smooth(f)")
                  #:max-depth 3)))
         (html (sxml->html (content->sxml
                            (math-derivation->content candidate)))))
    (test-assert "vector identities compose with typed nested simplification"
      (math-verification-valid? (math-verify-derivation candidate)))
    (test-assert "composed derivation renders MathML operators and vector zero"
      (and (string-contains html "<mtable>")
           (string-contains html "<mo>∇</mo>")
           (string-contains html "<mo>×</mo>")
           (string-contains html "<mn>0</mn>")
           (string-contains html "curl-grad-zero"))))
  (test-assert "div-grad identity composes with scalar simplification"
    (math-verification-valid?
      (math-verify-derivation
        (car (search-candidates "div(grad(f)) + 0" "laplacian(f)"
               #:assumptions '("scalar(f); smooth(f)") #:max-depth 2)))))
  (let* ((report (φ:search "(x + 0) + (y + 0)" "x + y"
                   #:assumptions '("scalar(x); scalar(y)")
                   #:rules '(add-zero) #:max-states 4)))
    (test-equal "state-limited search preserves candidates already found"
      1 (length (math-report-candidates report)))
    (test-equal "state-limited report states why it stopped"
      'state-limit (math-report-reason report))
    (test-equal "search counts explored states" 4 (math-report-states report))
    (test-assert "partial candidates still verify"
      (every (lambda (candidate)
               (math-verification-valid? (math-verify-derivation candidate)))
             (math-report-candidates report)))
    (test-assert "limited report does not claim completeness"
      (not (math-report-complete? report))))
  (test-equal "depth cutoff differs from exhausted search"
    'depth-limit
    (math-report-reason
      (φ:search "x + 0" "x" #:assumptions '("scalar(x)") #:max-depth 0)))
  (test-equal "an exhausted dead end is a complete search"
    'exhausted
    (math-report-reason
      (φ:search "x" "y" #:assumptions '("scalar(x); scalar(y)")
        #:rules '() #:max-depth 0)))
  (test-equal "result limit remains visible in the report"
    'result-limit
    (math-report-reason
      (φ:search "(x + 0) + (y + 0)" "x + y"
        #:assumptions '("scalar(x); scalar(y)") #:rules '(add-zero)
        #:max-results 1)))
  (test-equal "inverse generation respects the explicit term size bound"
    'term-size-limit
    (math-report-reason
      (φ:search "y" "x" #:backward? #t #:assumptions '("scalar(x); scalar(y)")
        #:rules '(add-zero) #:max-term-size 3)))
  (test-equal "explicit inverse arithmetic rule selection enables bounded expansion"
    '((+ (var x) (num "0")) (var x))
    (math-derivation-terms
      (car (math-report-candidates
             (φ:search "x + 0" "x" #:backward? #t #:assumptions '("scalar(x)")
               #:rules '(add-zero) #:max-depth 1)))))
  (test-equal "rule selection excludes other applicable rules"
    '()
    (math-report-candidates
      (φ:search "x + 0" "x" #:assumptions '("scalar(x)") #:rules '(multiply-one))))
  (test-error "unknown selected rules are rejected" #t
    (φ:search "x" "x" #:assumptions '("scalar(x)") #:rules '(unknown-rule)))
  (let* ((report (φ:simplify "curl(curl(E)) + zero(vector)"
                   #:assumptions '("vector(E)")))
         (candidate (car (math-report-candidates report))))
    (test-equal "normalization avoids expanding differential identities"
      '((+ (curl (curl (var E))) (zero vector)) (curl (curl (var E))))
      (math-derivation-terms candidate))
    (test-equal "normalization reports normal form"
      'normal-form (math-report-reason report))
    (test-assert "normalization candidate verifies"
      (math-verification-valid? (math-verify-derivation candidate))))
  (test-equal "normalization retains a partially simplified chain at its step limit"
    'step-limit
    (math-report-reason
      (φ:simplify "1 * (x + 0)" #:assumptions '("scalar(x)") #:max-steps 1)))
  (test-error "normalization rejects rules outside its arithmetic groups" #t
    (φ:simplify "x" #:assumptions '("scalar(x)") #:rules '(curl-curl-identity)))
  (let* ((sources '("curl(curl(E))" "grad(div(E)) - laplacian(E)"
                    "zero(vector) - laplacian(E)" "-laplacian(E)"))
         (assumptions '("vector(E); smooth(E); div(E)=0"))
         (automatic (φ:check-chain sources #:assumptions assumptions))
         (explicit (φ:check-chain sources #:assumptions assumptions
                     #:steps '((curl-curl-identity ()) (divergence-free (0))
                               (subtract-zero ())))))
    (test-assert "authored chains infer and independently verify each adjacent rule"
      (math-verification-valid? automatic))
    (test-assert "authored chains accept explicit rule and operand path annotations"
      (math-verification-valid? explicit))
    (test-equal "inferred authored steps retain rule provenance"
      '(curl-curl-identity divergence-free subtract-zero)
      (map math-derivation-step-rule
           (math-derivation-steps (math-verification-derivation automatic))))
    (test-assert "verified authored chains lower to annotated MathML"
      (string-contains (sxml->html (content->sxml (math-verification->content explicit)))
                       "Divergence Free")))
  (let* ((verification (φ:check-chain '("curl(grad(f))" "zero(vector)")
                         #:assumptions '("scalar(f)")))
         (diagnostic (car (math-verification-diagnostics verification))))
    (test-equal "missing premises are inferred as diagnostics, never accepted as facts"
      '(missing-premise) (diagnostic-codes verification))
    (test-equal "missing-premise diagnostic names the failed step" 1
      (math-diagnostic-step diagnostic))
    (test-equal "missing-premise diagnostic names the rule" 'curl-grad-zero
      (math-diagnostic-rule diagnostic))
    (test-equal "missing-premise diagnostic retains exact missing facts"
      '((c2 (var f))) (math-diagnostic-premises diagnostic))
    (test-assert "missing premise is readable in the author's source syntax"
      (string-contains (math-diagnostic-message diagnostic) "c2(f)"))
    (test-equal "diagnostic hints leave stored assumptions unchanged"
      '((scalar (var f)))
      (math-derivation-assumptions (math-verification-derivation verification)))
    (test-assert "replaying a hinted certificate still rejects the missing premise"
      (not (math-verification-valid?
             (math-verify-derivation (math-verification-derivation verification)))))
    (test-error "failed authored chains cannot be rendered as verified equalities" #t
      (math-verification->content verification)))
  (let* ((verification
           (φ:check-chain '("zero(vector) + curl(grad(f))"
                               "zero(vector) + zero(vector)")
             #:assumptions '("scalar(f)")))
         (diagnostic (car (math-verification-diagnostics verification))))
    (test-equal "nested missing-premise diagnostics retain the actual sibling path"
      '(1) (math-diagnostic-path diagnostic))
    (test-assert "nested diagnostic labels use one-based operand positions"
      (string-contains (math-diagnostic-message diagnostic) "operand 2")))
  (let ((verification
          (φ:check-chain '("x + (y + 0)" "x + y")
            #:assumptions '("scalar(x); scalar(y)") #:steps '((add-zero (1))))))
    (test-assert "explicit second-operand paths describe a real rewrite"
      (math-verification-valid? verification)))
  (test-equal "generated paths do not include skipped siblings"
    '((1))
    (map math-derivation-step-path
      (math-derivation-steps
        (car (search-candidates "x + (y + 0)" "x + y"
               #:assumptions '("scalar(x); scalar(y)") #:rules '(add-zero))))))
  (test-equal "inverse paths do not include skipped siblings"
    '((1))
    (map math-derivation-step-path
      (math-derivation-steps
        (car (backward-candidates "x + y" "x + (y + 0)"
               #:assumptions '("scalar(x); scalar(y)") #:rules '(add-zero)
               #:max-depth 1)))))
  (for-each
    (lambda (example)
      (test-equal (string-append "authored chain diagnoses " (symbol->string (car example)))
        (list (car example))
        (diagnostic-codes
          (φ:check-chain (cadr example) #:assumptions '("scalar(x); scalar(y)")
            #:steps (caddr example)))))
    '((unknown-rule ("x + 0" "x") (unknown))
      (invalid-path ("x + 0" "x") ((add-zero (2))))
      (rule-shape ("x + 0" "x") (multiply-one))
      (target-mismatch ("x + 0" "y") (add-zero))))
  (test-equal "authored chains diagnose type-changing equations"
    '(type-change)
    (diagnostic-codes
      (φ:check-chain '("curl(grad(f))" "0")
        #:assumptions '("scalar(f); smooth(f)") #:steps '(curl-grad-zero))))
  (for-each
    (lambda (example)
      (let ((checked (φ:check-chain (car example) #:assumptions (cadr example))))
        (test-equal "differential rule contracts explain missing authored premises"
          '(missing-premise) (diagnostic-codes checked))
        (test-assert "diagnostic contracts never discharge a differential premise"
          (not (math-verification-valid?
                 (math-verify-derivation (math-verification-derivation checked)))))))
    '((("curl(curl(E))" "grad(div(E)) - laplacian(E)") ("vector(E)"))
      (("grad(div(E))" "zero(vector)") ("vector(E)"))
      (("div(curl(E))" "0") ("vector(E)"))
      (("div(grad(f))" "laplacian(f)") ("scalar(f)"))))
  (test-assert "authored chains diagnose ill-typed operands"
    (memq 'type-error
      (diagnostic-codes
        (φ:check-chain '("curl(grad(f))" "zero(vector)")
          #:assumptions '("vector(f); smooth(f)") #:steps '(curl-grad-zero)))))
  (test-equal "rule selection applies to explicitly annotated authored chains"
    '(disabled-rule)
    (diagnostic-codes
      (φ:check-chain '("x + 0" "x") #:assumptions '("scalar(x)")
        #:steps '(add-zero) #:rules '(multiply-one))))
  (test-equal "disabled rules are not inferred for unannotated chains"
    '(no-rule)
    (diagnostic-codes
      (φ:check-chain '("x + 0" "x") #:assumptions '("scalar(x)") #:rules '())))
  (test-equal "checking continues after the first failed authored step"
    '(1 2)
    (map math-diagnostic-step
      (math-verification-diagnostics
        (φ:check-chain '("x + 0" "y" "x")
          #:assumptions '("scalar(x); scalar(y)") #:steps '(add-zero multiply-one)))))
  (test-error "authored annotations must cover every adjacent pair" #t
    (φ:check-chain '("x + 0" "x") #:assumptions '("scalar(x)") #:steps '()))
  (test-error "Formula chains require a list of source lines" #t
    (φ:check-chain "x + 0"))
  (test-error "authored chains require at least two lines" #t (φ:check-chain '("0")))
  (test-equal "malformed certificates return diagnostics"
    '(invalid-chain)
    (diagnostic-codes (math-verify-derivation (make-math-derivation '() '() '()))))
  (test-equal "single-step verification safely rejects malformed target terms"
    #f
    (math-verify-step '((scalar (var x))) '(+ (var x) (num "0"))
                      '(var) 'add-zero '()))
  (test-equal "reserved unary operators reject extra operands" #f
    (math-term? '(curl (var E) (var F))))
  (test-equal "independent replay rejects mismatched recorded endpoints"
    '(endpoints)
    (diagnostic-codes
      (math-verify-derivation
        (make-math-derivation '((+ (var x) (num "0")) (var x))
          (list (make-math-derivation-step '(var x) '(var x) 'add-zero '()))
          '((scalar (var x)))))))
  (for-each
    (lambda (source)
      (let ((term (φ:term source)))
        (test-equal "diagnostic source notation preserves mathematical structure"
          term (φ:term (math-term->source term)))))
    '("zero(vector)" "div(E)=0" "-(x + y)" "x^2" "1 * (x + 0)"))
  (test-end "grove math pipeline"))

(run-math-tests)
