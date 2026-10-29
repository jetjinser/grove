;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math semantic) #:prefix semantic:)
             ((grove math formula) #:prefix φ:)
             ((grove math notation) #:prefix notation:)
             ((grove math private notation) #:prefix core:)
             ((language scribble commands) #:prefix document:)
             (grove math calculation) (grove math context) (grove math expression)
             (grove math layout)
             (grove render html) (grove render text) (grove ir)
             (language scribble spec) (language scribble parser) (system base language)
             (srfi srfi-13) (srfi srfi-64))

(define (html value) (sxml->html (content->sxml value)))
(define (page source)
  (scribble-evaluate-document (open-input-string source) (default-environment scribble) "semantic.plant"))
(define imports "@use-modules[((grove math semantic) #:prefix semantic:) ((grove math formula) #:prefix φ:) ((grove math notation) #:prefix notation:) (grove math calculation)]\n")
(define polynomial (φ:read (φ:power "x" "3") "+" (φ:multiply "2" "x")))

(test-begin "unified mathematical writing interface")
(let ((name (list->string (map integer->char '(101 804 768)))))
  (test-equal "Semantic variables and Formula use the same composed identifier"
    (math-value-term (semantic:variable name))
    (math-value-term (φ:read name))))
(test-equal "semantic division uses the same notation as an authored fraction"
  (content->sxml (notation:frac "x" "2"))
  (content->sxml (math-value->layout (φ:divide "x" "2"))))
(test-equal "semantic powers use the shared pure notation constructors"
  (content->sxml (core:sup (list (core:token 'mi "x"))
                         (list (core:token 'mn "3"))))
  (content->sxml (math-value->layout (φ:power "x" "3"))))
(test-equal "Formula prime adapts text operands to the Semantic constructor"
  (math-value-term (semantic:prime 'f (semantic:variable 'x)))
  (φ:term (φ:prime 'f "x")))
(test-equal "Formula function-partial adapts an application and preserves its argument index"
  (math-value-term
    (semantic:function-partial
      (semantic:call 'f (semantic:variable 'x) (semantic:variable 'y)) 2))
  (φ:term (φ:function-partial "f(x,y)" 2)))
(test-equal "vector assumptions lower through notation accents"
  (content->sxml (notation:vec "E"))
  (content->sxml (math-value->layout (semantic:variable "E") '((vector (var E))))))
(test-equal "semantic preferences use the shared notation style and grouping"
  (content->sxml (core:style (list (notation:parens "x")) 'bold))
  (content->sxml (math-value->layout
    (φ:styled (φ:group "x") #:variant 'bold))))
(with-math-assumptions (φ:assumptions "scalar(x)")
  (let* ((report (φ:differentiate polynomial "x")) (result (math-report-result report)))
    (test-equal "a constructed expression enters polynomial differentiation"
      (φ:term "3*x^2+2") (math-value-term result))
    (test-assert "computed mathematical values are displayable"
      (string-contains (html (document:math result)) "<msup>"))
    (test-assert "computed values may be reused as input to another pass"
      (math-report-complete? (φ:simplify result)))
    (test-assert "the resulting certificate still independently replays"
      (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
    (test-assert "the same object can participate in goal search"
      (pair? (math-report-candidates
               (φ:search (φ:add polynomial "0") polynomial #:rules '(add-zero) #:max-depth 1 #:max-results 1))))
    (test-assert "object-based authored chains use the same verifier"
      (math-verification-valid? (φ:check-chain
        (list (φ:derivative "x" "x") (semantic:constant 1)))))))
(let ((rendered (html (document:math (φ:add "x" "x")))))
  (test-assert "direct display leaves the author's expression unchanged"
    (and (string-contains rendered "<mi>x</mi><mo>+</mo><mi>x</mi>")
         (not (string-contains rendered "<mn>2</mn>")))))
(let* ((plain (φ:power "x" "3"))
       (styled (φ:styled plain #:variant 'bold)))
  (test-equal "style never changes a mathematical term" (math-value-term plain) (math-value-term styled))
  (test-assert "semantic styles lower to an explicit MathML style"
    (let ((rendered (html (document:math styled))))
      (and (string-contains rendered "<mstyle style=\"font-weight: bold; font-style: normal\">")
           (string-contains rendered "<mi mathvariant=\"normal\">x</mi>"))))
  (with-math-assumptions (φ:assumptions "scalar(x)")
    (test-equal "style never changes a derivative result"
      (math-report-final (φ:differentiate plain "x"))
      (math-report-final (φ:differentiate styled "x")))))
(test-assert "nested grouping preferences survive composition"
  (string-contains
    (html (document:math (φ:read (φ:group "x+y") "+2")))
    "<mo>(</mo>"))
(test-assert "a negative power base renders with required parentheses"
  (string-contains (html (document:math (φ:power (φ:negate "x") "2")))
                   "<mo>(</mo>"))
(test-assert "a valid semantic function is displayable without a calculation rule"
  (string-contains (html (document:math (φ:read "sin(x)"))) "sin"))
(test-equal "unsupported differentiation is distinct from missing semantics"
  'unsupported-expression
  (math-diagnostic-code (car (math-report-diagnostics (φ:differentiate (φ:read "opaque(x)") "x")))))
(test-error "notation operands are not interpreted by appearance" #t
  (φ:calculate (notation:frac "1" "2")))
(test-error "notation decoration does not declare a vector variable" #t
  (φ:curl (notation:vec "E")))
(with-math-assumptions (φ:assumptions "scalar(x); scalar(i)")
  (test-equal "an uninterpreted function cannot silently act as a type-preserving index"
    'unsupported-expression (math-report-reason (φ:simplify (φ:read "sub(x,i)")))))
(test-assert "an uninterpreted function is not displayed as an inferred subscript"
  (string-contains (html (document:math (φ:read "sub(x,i)"))) "<mi>sub</mi>"))
(test-assert "notation can display a semantic object without changing its identity"
  (string-contains (html (notation:math (notation:parens polynomial))) "<msup>"))
(test-equal "prefix commands support multiple complete body arguments"
  '((φ:power (list "x+" (φ:divide "y" "2")) "3"))
  (scribble-parse (open-input-string "@φ:power{x+@φ:divide{y}{2}}{3}")))
(let ((rendered (html (page (string-append imports
  "@(define p @φ:read{@φ:power{x}{3} + 2*x})\n"
  "@math[p]\n"
  "@with-assumptions[\"scalar(x)\"]{\n"
  "@differentiate[\"x\" p]\n"
  "@search-derive[p]{@φ:add[p \"0\"]}\n"
  "@check-derive{\n@φ:derivative{x}{x}\n1\n}\n}")))))
  (test-assert "an article defines its formula once for display and computation"
    (and (string-contains rendered "<msup>")
         (string-contains rendered "data-rule=\"differentiate-power\"")
         (string-contains rendered "1 derivation"))))
(let ((rendered (html (page (string-append imports
  "@differentiate[\"x\" #:assumptions \"scalar(x)\" #:on-error 'show]{\n"
  "  x + @φ:divide{x}{x+1}\n}")))))
  (test-assert "nested semantic expressions retain document source attribution"
    (string-contains rendered "semantic.plant:3:")))
(let ((rendered (html (page (string-append imports
  "@check-derive[#:assumptions \"scalar(x)\" #:on-error 'show]{\n"
  "@φ:derivative{x}{x}\n\n"
  "  2\n}")))))
  (test-assert "object-based handwritten chains retain target line and indentation"
    (string-contains rendered "semantic.plant:5:3")))
(test-end "unified mathematical writing interface")
