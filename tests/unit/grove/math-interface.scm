;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math semantic) #:prefix semantic:)
             ((grove math notation) #:prefix notation:)
             ((grove math notation core) #:prefix core:)
             ((language scribble commands) #:prefix document:)
             (grove math pipeline) (grove math context) (grove math expression)
             (grove math layout) (grove math source)
             (grove render html) (grove render text) (grove ir)
             (language scribble spec) (language scribble parser) (system base language)
             (srfi srfi-13) (srfi srfi-64))

(define (html value) (sxml->html (content->sxml value)))
(define (page source)
  (scribble-evaluate-document (open-input-string source) (default-environment scribble) "semantic.plant"))
(define imports "@use-modules[((grove math semantic) #:prefix semantic:) ((grove math notation) #:prefix notation:) (grove math pipeline)]\n")
(define polynomial (semantic:expression (semantic:power "x" "3") "+" (semantic:multiply "2" "x")))

(test-begin "unified mathematical writing interface")
(test-equal "semantic division uses the same notation as an authored fraction"
  (content->sxml (notation:frac "x" "2"))
  (content->sxml (math-value->layout (semantic:divide "x" "2"))))
(test-equal "semantic powers use the shared pure notation constructors"
  (content->sxml (core:sup (list (core:token 'mi "x"))
                         (list (core:token 'mn "3"))))
  (content->sxml (math-value->layout (semantic:power "x" "3"))))
(test-equal "vector assumptions lower through notation accents"
  (content->sxml (notation:vec "E"))
  (content->sxml (math-value->layout (semantic:variable "E") '((vector (var E))))))
(test-equal "semantic preferences use the shared notation style and grouping"
  (content->sxml (core:style (list (notation:parens "x")) 'bold))
  (content->sxml (math-value->layout
    (semantic:styled (semantic:group "x") #:variant 'bold))))
(with-math-assumptions '("scalar(x)")
  (let* ((report (math-differentiate polynomial "x")) (result (math-report-result report)))
    (test-equal "a constructed expression enters polynomial differentiation"
      (math-source->term "3*x^2+2") (math-value-term result))
    (test-assert "computed mathematical values are displayable"
      (string-contains (html (document:math result)) "<msup>"))
    (test-assert "computed values may be reused as input to another pass"
      (math-report-complete? (math-simplify result)))
    (test-assert "the resulting certificate still independently replays"
      (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
    (test-assert "the same object can participate in goal search"
      (pair? (math-report-candidates
               (math-search (semantic:add polynomial "0") polynomial #:rules '(add-zero) #:max-depth 1 #:max-results 1))))
    (test-assert "object-based authored chains use the same verifier"
      (math-verification-valid? (math-check-chain
        (list (semantic:derivative "x" "x") (semantic:constant 1)))))))
(let ((rendered (html (document:math (semantic:add "x" "x")))))
  (test-assert "direct display leaves the author's expression unchanged"
    (and (string-contains rendered "<mi>x</mi><mo>+</mo><mi>x</mi>")
         (not (string-contains rendered "<mn>2</mn>")))))
(let* ((plain (semantic:power "x" "3"))
       (styled (semantic:styled plain #:variant 'bold)))
  (test-equal "style never changes a mathematical term" (math-value-term plain) (math-value-term styled))
  (test-assert "semantic styles lower to an explicit MathML style"
    (let ((rendered (html (document:math styled))))
      (and (string-contains rendered "<mstyle style=\"font-weight: bold; font-style: normal\">")
           (string-contains rendered "<mi mathvariant=\"normal\">x</mi>"))))
  (with-math-assumptions '("scalar(x)")
    (test-equal "style never changes a derivative result"
      (math-report-final (math-differentiate plain "x"))
      (math-report-final (math-differentiate styled "x")))))
(test-assert "nested grouping preferences survive composition"
  (string-contains
    (html (document:math (semantic:expression (semantic:group "x+y") "+2")))
    "<mo>(</mo>"))
(test-assert "a negative power base renders with required parentheses"
  (string-contains (html (document:math (semantic:power (semantic:negate "x") "2")))
                   "<mo>(</mo>"))
(test-assert "a valid semantic function is displayable without a calculation rule"
  (string-contains (html (document:math (semantic:expression "sin(x)"))) "sin"))
(test-equal "unsupported differentiation is distinct from missing semantics"
  'unsupported-expression
  (math-diagnostic-code (car (math-report-diagnostics (math-differentiate (semantic:expression "sin(x)") "x")))))
(test-error "notation operands are not interpreted by appearance" #t
  (math-calculate (notation:frac "1" "2")))
(test-error "notation decoration does not declare a vector variable" #t
  (semantic:curl (notation:vec "E")))
(with-math-assumptions '("scalar(x); scalar(i)")
  (test-equal "an uninterpreted function cannot silently act as a type-preserving index"
    'type-error (math-report-reason (math-simplify (semantic:expression "sub(x,i)")))))
(test-assert "an uninterpreted function is not displayed as an inferred subscript"
  (string-contains (html (document:math (semantic:expression "sub(x,i)"))) "<mi>sub</mi>"))
(test-assert "notation can display a semantic object without changing its identity"
  (string-contains (html (notation:math (notation:parens polynomial))) "<msup>"))
(test-equal "prefix commands support multiple complete body arguments"
  '((semantic:power (list "x+" (semantic:divide "y" "2")) "3"))
  (scribble-parse (open-input-string "@semantic:power{x+@semantic:divide{y}{2}}{3}")))
(let ((rendered (html (page (string-append imports
  "@(define p @semantic:expression{@semantic:power{x}{3} + 2*x})\n"
  "@math[p]\n"
  "@with-assumptions[\"scalar(x)\"]{\n"
  "@differentiate[\"x\" p]\n"
  "@search-derive[p]{@semantic:add[p \"0\"]}\n"
  "@check-derive{\n@semantic:derivative{x}{x}\n1\n}\n}")))))
  (test-assert "an article defines its formula once for display and computation"
    (and (string-contains rendered "<msup>")
         (string-contains rendered "data-rule=\"differentiate-power\"")
         (string-contains rendered "1 derivation(s)"))))
(let ((rendered (html (page (string-append imports
  "@differentiate[\"x\" #:assumptions \"scalar(x)\" #:on-error 'show]{\n"
  "  x + @semantic:divide{x}{x+1}\n}")))))
  (test-assert "nested semantic expressions retain document source attribution"
    (string-contains rendered "semantic.plant:3:")))
(let ((rendered (html (page (string-append imports
  "@check-derive[#:assumptions \"scalar(x)\" #:on-error 'show]{\n"
  "@semantic:derivative{x}{x}\n\n"
  "  2\n}")))))
  (test-assert "object-based handwritten chains retain target line and indentation"
    (string-contains rendered "semantic.plant:5:3")))
(test-end "unified mathematical writing interface")
