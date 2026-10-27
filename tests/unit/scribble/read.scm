;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit scribble read)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs io ports)
  #:use-module (system base language)
  #:use-module (language scheme spec)
  #:use-module (language scribble parser)
  #:use-module (language scribble source)
  #:use-module (grove private input dependencies)
  #:use-module (ice-9 textual-ports)
  #:use-module ((language scribble spec)
                #:select (scribble scribble-evaluate-document))
  #:use-module (grove input scribble)
  #:use-module (grove render html)
  #:use-module (grove render text)
  #:use-module (grove ir))

(define (read-with-scheme-language env input)
  ((language-reader scheme) (open-input-string input) env))

(define (evaluate-document env input)
  (eval '(use-modules ((grove math notation) #:prefix notation:)) env)
  (scribble-evaluate-document (open-input-string input) env))

(define (exception-contains? thunk text)
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . args)
      (string-contains (format #f "~s" (cons key args)) text))))

(define (substring-count text substring)
  (let loop ((start 0) (count 0))
    (let ((position (string-contains text substring start)))
      (if position
          (loop (+ position (string-length substring)) (+ count 1))
          count))))

(define temporary-source-index 0)

(define (call-with-temporary-source input proc)
  (let ((path
          (format #f "/tmp/grove-scribble-~a-~a.plant"
                  (getpid)
                  temporary-source-index)))
    (set! temporary-source-index (+ temporary-source-index 1))
    (dynamic-wind
      (lambda ()
        (call-with-output-file path
          (lambda (port) (display input port))))
      (lambda () (proc path))
      (lambda ()
        (when (file-exists? path)
          (delete-file path))))))

;; seems no need to test read individually?

(define-syntax-rule (test-read test-name
                               expected
                               scribble-str)
  (test-equal test-name
    expected
    (scribble-read (open-input-string scribble-str))))

(define-syntax-rule (test-read/approximate test-name
                                           expected
                                           scribble-str
                                           error)
  (test-approximate test-name
    expected
    (scribble-read (open-input-string scribble-str))
    error))

(define-syntax-rule (test-read/error test-name
                                     scribble-str)
  (test-error test-name #t
    (scribble-read (open-input-string scribble-str))))

(test-begin "scribble read")

(let* ((document (evaluate-document (default-environment scribble)
                   "@with-assumptions[\"scalar(x)\"]{
 @differentiate[\"x\"]{x*x}
 @with-assumptions[\"scalar(y)\"]{@derive{x+y-x}}
 @check-derive{differentiate(x,x)\n1}
 @search-derive[\"1\"]{differentiate(x,x)}
}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "scoped assumptions reach every command before its body is evaluated"
    (and (string-contains html "collect-like-terms")
         (string-contains html "differentiate-variable")
         (string-contains html "1 derivation"))))
(test-assert "a scoped document does not supply premises to the next document"
  (exception-contains?
    (lambda () (evaluate-document (default-environment scribble) "@derive{x+x}"))
    "no type declaration"))
(let* ((document (evaluate-document (default-environment scribble)
                   "@with-assumptions[\"scalar(x)\"]{@derive[#:assumptions \"scalar(y)\"]{x+y-x}}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "explicit command premises augment the local context"
    (string-contains html "collect-like-terms")))

(let* ((document (evaluate-document (default-environment scribble)
                   "@search-derive[\"1\" #:assumptions \"scalar(x)\"]{differentiate(x,x)}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "@search-derive presents candidates and the search stopping reason"
    (and (string-contains html "Search finished:")
         (string-contains html "1 derivation")
         (string-contains html "differentiate-variable"))))

(let* ((document (evaluate-document (default-environment scribble)
                   "@search-derive[\"2\" #:assumptions \"scalar(x)\"]{differentiate(x,x)}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "an unsuccessful search displays its report without aborting the article"
    (and (string-contains html "Search finished: 0 derivations")
         (not (string-contains html "<mtable>")))))

(let* ((document (evaluate-document (default-environment scribble)
                   "@differentiate[\"x\" #:assumptions \"scalar(x)\" #:view 'full]{x^3 + 2*x}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "@differentiate displays both passes as native MathML"
    (and (= (substring-count html "<mtable>") 2)
         (string-contains html "<mfrac>")
         (string-contains html "<msup>")
         (string-contains html "differentiate-power")
         (string-contains html "zero-multiply"))))

(let* ((env (default-environment scribble))
       (document
         (evaluate-document env
           "@check-derive[#:assumptions \"vector(E); smooth(E); div(E)=0\" #:view 'full]{
  curl(curl(E))
  grad(div(E)) - laplacian(E)

  zero(vector) - laplacian(E)
  -laplacian(E)
}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "@check-derive verifies and displays the author's exact math chain"
    (and (= (substring-count html "<mtr>") 4)
         (string-contains html "curl-curl-identity")
         (string-contains html "Divergence Free")
         (string-contains html "subtract-zero"))))

(test-assert "@check-derive reports the missing premise at its authored step"
  (exception-contains?
    (lambda ()
      (evaluate-document (default-environment scribble)
        "@check-derive[#:assumptions \"scalar(f)\"]{curl(grad(f))\nzero(vector)}"))
    "step 1, curl-grad-zero at root: missing premises: c2(f)"))

(test-assert "@check-derive rejects silently skipped intermediate rewrites"
  (exception-contains?
    (lambda ()
      (evaluate-document (default-environment scribble)
        "@check-derive[#:assumptions \"vector(E); smooth(E); div(E)=0\" #:view 'full]{curl(curl(E))\n-laplacian(E)}"))
    "not the supplied target"))

(let* ((env (default-environment scribble))
       (document (evaluate-document env
                   "intro text\n@(define (page-helper) \"helper ran\")\nmore text\n\n@(page-helper)")))
  (test-assert "a top-level definition in a page binds in the page environment"
    (and (procedure? (module-ref env 'page-helper))
         (string-contains (content->text document) "helper ran"))))

(test-assert "a definition inside a command argument stays in expression context"
  (exception-contains?
    (lambda ()
      (evaluate-document (default-environment scribble)
        "@(make-element 'paragraph (list \"x\" (define (f) 1)))"))
    "definition in expression context"))

(let* ((env (default-environment scribble))
       (document (evaluate-document env
         "@derive[#:assumptions \"scalar(f); smooth(f)\" #:view 'full]{curl(grad(f)) + zero(vector)}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "@derive composes scalar-field identities and vector-zero simplification"
    (and (string-contains html "curl-grad-zero")
         (string-contains html "add-zero")
         (string-contains html "<mover accent=\"true\">")
         (= (substring-count html "<mtr>") 3))))

(let* ((env (default-environment scribble))
       (document
         (evaluate-document
           env
           (string-append
             "@notation:display-math{@notation:align{"
             "∇ · @notation:vec{D} &= ρ\n"
             "∇ × @notation:vec{E} &= −@notation:frac{∂@notation:vec{B}}{∂t}\n"
             "∇ · @notation:vec{B} &= 0\n"
             "∇ × @notation:vec{H} &= @notation:vec{J} + @notation:frac{∂@notation:vec{D}}{∂t}"
             "}}\n"
             "@paragraph{Assume a homogeneous, isotropic, charge-free medium "
             "with @notation:math{@notation:vec{D}=ε@notation:vec{E}}, @notation:math{@notation:vec{B}=μ@notation:vec{H}}, "
             "and @notation:math{@notation:vec{J}=σ@notation:vec{E}}. The x-component is "
             "@notation:math{@notation:sub{E}{x}}.}\n"
             "@notation:display-math{@notation:align{"
             "∇ × (∇ × @notation:vec{E}) &= −∇ × @notation:frac{∂@notation:vec{B}}{∂t}\n"
             "&= −μ @notation:frac{∂}{∂t} (∇ × @notation:vec{H})\n"
             "&= −μ @notation:frac{∂}{∂t} @notation:parens{@notation:vec{J} + @notation:frac{∂@notation:vec{D}}{∂t}}\n"
             "&= −μ @notation:parens{@notation:frac{∂@notation:vec{J}}{∂t} + "
             "@notation:frac{@notation:sup{∂}{2}@notation:vec{D}}{@notation:sup{∂t}{2}}}\n"
             "&= −μσ @notation:frac{∂@notation:vec{E}}{∂t} − με "
             "@notation:frac{@notation:sup{∂}{2}@notation:vec{E}}{@notation:sup{∂t}{2}}\n"
             "∇ × (∇ × @notation:vec{E}) &= ∇(∇ · @notation:vec{E}) − ∇²@notation:vec{E}\n"
             "&= −∇²@notation:vec{E}\n"
             "−∇²@notation:vec{E} &= −μσ @notation:frac{∂@notation:vec{E}}{∂t} − με "
             "@notation:frac{@notation:sup{∂}{2}@notation:vec{E}}{@notation:sup{∂t}{2}}\n"
             "∇²@notation:vec{E} &= μσ @notation:frac{∂@notation:vec{E}}{∂t} + με "
             "@notation:frac{@notation:sup{∂}{2}@notation:vec{E}}{@notation:sup{∂t}{2}}"
             "}}")))
       (html (sxml->html (content->sxml document))))
  (test-assert "Maxwell equations and wave derivation emit MathML Core"
    (and (= (substring-count html "<mtable>") 2)
         (>= (substring-count html "<mtr>") 13)
         (string-contains html "<mover accent=\"true\">")
         (string-contains html
                          "<mover accent=\"true\"><mrow>")
         (string-contains html
                          "</mrow><mo>→</mo></mover>")
         (string-contains html "<mfrac>")
         (string-contains html "<msup>")
         (string-contains html "<msub>")
         (string-contains html "<mtd"))))

(test-read "empty"
  (eof-object)
  "")

(test-read "1 space"
  (eof-object)
  " ")

(test-read "n spaces"
  (eof-object)
  "   ")

(test-read "1 newline"
  (eof-object)
  "\n")

(test-read "n newline"
  (eof-object)
  "\n\n\n")

(test-read "mix space and newline"
  (eof-object)
  "\n  \n \n")

(test-read "plain word"
  'world
  "world")

(test-read "plain [C]JK word"
  '中
  "中")

(test-read "plain emoji"
  '🀄
  "🀄")

(test-read "plain text with space"
  'Hello,
  "Hello, world!")

(test-read "integer"
  42
  "42")

(test-read/approximate "float"
  42.42
  "42.42"
  0.01)

(test-read/error "invalid . in source"
  "42.42.")

(test-read/error "too many )'s"
  ")")

(test-read "unquote splicing reads from its input port"
  '(unquote-splicing values)
  ",@values")

(test-read/error "unterminated string literal"
  "\"text")

(test-read/error "dangling escape in string literal"
  "\"text\\")

(let* ((env (default-environment scheme))
       (reader-fluid (module-ref env 'current-reader)))
  (fluid-set! reader-fluid scribble-read)
  (test-equal "at-exp can be enabled in an ordinary Scheme expression"
    '(let ((value 40)) (increment 2))
    (read-with-scheme-language env "(let ((value 40)) @increment[2])"))
  (test-equal "at-exp expressions use ordinary Scheme lexical scope"
    42
    (eval (read-with-scheme-language env
                                     "(let ((n 41)) @+[@n 1])")
          env))
  (test-equal "parenthesized at-exp escape is an ordinary expression"
    42
    (eval (read-with-scheme-language env "@(+ 40 2)") env))
  (test-equal "text bodies can call nested expressions in lexical scope"
    "Hello, Ada!"
    (eval (read-with-scheme-language
            env
            "(let ((name \"Ada\")) @string-append{Hello, @|name|!})")
          env))
  (eval '(define-syntax twice
           (syntax-rules ()
             ((_ n) (+ n n))))
        env)
  (test-equal "ordinary Scheme macros can expand at-exp calls"
    42
    (eval (read-with-scheme-language env "@twice[21]") env))
  (fluid-set! reader-fluid read-syntax)
  (test-equal "native Scheme reader can be restored"
    '(+ 1 2)
    (syntax->datum (read-with-scheme-language env "(+ 1 2)"))))

(let ((env (default-environment scribble)))
  (let* ((document
           (evaluate-document
             env
             "@derive[#:assumptions \"vector(E); smooth(E)\"]{curl(curl(E))}"))
         (html (sxml->html (content->sxml document))))
    (test-assert "@derive connects parsed math, miniKanren rewriting, and MathML"
      (and (string-contains html "<mtable>")
           (string-contains html "<mo>∇</mo>")
           (string-contains html "<mo>×</mo>")
           (string-contains html "<mo>·</mo>")
           (string-contains html "<mi>Δ</mi>")))))

(let ((env (default-environment scribble)))
  (let* ((document
           (evaluate-document
             env
             "@derive[#:assumptions \"vector(E); smooth(E); div(E)=0\" #:view 'full]{curl(curl(E))}"))
         (html (sxml->html (content->sxml document))))
    (test-assert "@derive applies conditional and algebraic rewrites in sequence"
      (and (= (substring-count html "<mtr>") 4)
           (= (substring-count html "<mo>=</mo>") 3)
           (string-contains html "<mn>0</mn>")
           (string-contains html "</mover><mo>−</mo>")
           (string-contains html "<mo>−</mo><mrow><mi>Δ</mi>")))))

(let ((env (default-environment scribble)))
  (let* ((document
           (evaluate-document
             env
             "First paragraph.\n\nSecond @bold{paragraph}.\n"))
         (paragraphs (filter content-element?
                             (content-document-children document))))
    (test-equal "blank lines in direct prose create paragraphs"
      '(paragraph paragraph)
      (map content-element-kind paragraphs))
    (test-equal "inline commands stay inside their prose paragraph"
      '(content-text bold content-text)
      (map (lambda (value)
             (if (content-element? value)
                 (content-element-kind value)
                 'content-text))
           (content-element-children (cadr paragraphs))))))

(let ((env (default-environment scribble)))
  (let* ((document
           (evaluate-document
             env
             "@blockquote{Quoted}\n@itemize{@item{One}@item{Two}}\n@paragraph{@code{x + 1}}\n@code-block{(display \"hi\")}"))
         (elements (filter content-element?
                           (content-document-children document))))
    (test-equal "common content commands evaluate into generic IR elements"
      '(blockquote itemize paragraph code-block)
      (map content-element-kind elements))))

(let ((env (default-environment scribble)))
  (let* ((document
           (evaluate-document
             env
             "@use-modules[(grove ir)]\nBefore.\n@(make-content-element 'custom-block '(\"Panel\") #:flow-kind 'block)\nAfter."))
         (elements (filter content-element?
                           (content-document-children document))))
    (test-equal "custom block semantics split surrounding prose"
      '(paragraph custom-block paragraph)
      (map content-element-kind elements))))

(let ((env (default-environment scribble)))
  (let ((document
          (evaluate-document
            env
            "@use-modules[(forester commands)]\n@(map paragraph '(\"First paragraph.\" \"Second paragraph.\" \"Third paragraph.\"))")))
    (let ((paragraphs (filter content-element?
                              (content-document-children document))))
    (test-equal "Scheme expression lists splice into the document flow"
      '(paragraph paragraph paragraph)
      (map content-element-kind paragraphs))
    (test-assert "commands can be mapped over text without constructing IR"
      (equal? '("First paragraph." "Second paragraph." "Third paragraph.")
        (map (lambda (paragraph)
               (content-verbatim-string
                 (content-element-children paragraph)))
             paragraphs))))))

(let ((env (default-environment scribble)))
  (let ((document
          (evaluate-document
            env
            "@(define (sample-text) (string-append \"First paragraph.\" (make-string 2 (integer->char 10)) \"Second paragraph.\"))\n@(map paragraph (text-paragraphs (sample-text)))")))
    (test-equal "text-paragraphs works with any plain-text command"
      '("First paragraph." "Second paragraph.")
      (map (lambda (element)
             (content-verbatim-string (content-element-children element)))
           (filter content-element?
                   (content-document-children document))))))

(let ((env (default-environment scribble)))
  (let* ((document
           (evaluate-document
             env
             "@use-modules[(forester commands)]\n@(define note-label \"from Scheme\")\n@title{@|note-label|}\n@link[\"target\"]{Target}"))
         (children (content-document-children document))
         (elements (filter content-element? children))
         (title (car elements))
         (link (last elements)))
    (test-equal "source can import a module before using its bindings"
      'paragraph
      (content-element-kind link))
    (test-equal "top-level inline commands are placed in a paragraph"
      '(link)
      (map content-element-kind
           (filter content-element?
                   (content-element-children link))))
    (test-equal "source definitions are available to later at-expressions"
      "from Scheme"
      (content-verbatim-string (content-element-children title)))
    (test-equal "module import expressions do not become document content"
      2
      (length (filter content-element? children))))

  (let* ((document
           (evaluate-document env "@paragraph{@|note-label|}"))
         (paragraph
           (car (content-document-children document))))
    (test-equal "separate documents can share their Scheme environment"
      "from Scheme"
      (content-verbatim-string
        (content-element-children paragraph))))

  (let ((document
          (evaluate-document
            env
            "@(define evaluation-order '())@(set! evaluation-order (cons 'first evaluation-order))@bold{body}@(set! evaluation-order (cons 'last evaluation-order))")))
    (test-equal "non-content Scheme results are ignored"
      '(paragraph)
      (map content-element-kind
           (filter content-element?
                   (content-document-children document))))
    (test-equal "top-level inline content is wrapped in a paragraph"
      '(bold)
      (map content-element-kind
           (content-element-children
             (car (filter content-element?
                          (content-document-children document))))))
    (test-equal "Scheme expressions run in source order"
      '(last first)
      (module-ref env 'evaluation-order))))

(let ((env (default-environment scribble)))
  (test-assert "syntax errors report source file and line/column"
    (exception-contains?
      (lambda ()
        (scribble-evaluate-document
          (open-input-string "@title{unterminated\n") env
          "notes/broken.plant"))
      "notes/broken.plant:2:1"))
  (test-assert "evaluation errors report source file and form number"
    (exception-contains?
      (lambda ()
        (scribble-evaluate-document
          (open-input-string "@undefined-command{}") env
          "notes/eval-error.plant"))
      "notes/eval-error.plant at top-level form 1")))

(let ((read-document (make-scribble-document-reader)))
  (call-with-temporary-source
    "@(define page-local-value 'private)@paragraph{First page}"
    read-document)
  (call-with-temporary-source
    "@paragraph{@|page-local-value|}"
    (lambda (path)
      (test-error "default reader isolates Scheme state per document" #t
        (read-document path)))))

(let ((read-document (make-scribble-document-reader)))
  (call-with-temporary-source
    "@(define-module (tests unit scribble named-page) #:use-module (forester commands) #:export (named-page-label))\n@(define named-page-label \"named module\")\n@title{Named page}\n@link[\"target\"]{Target}"
    read-document)
  (test-equal "define-module selects and exports the document environment"
    "named module"
    (module-ref (resolve-module '(tests unit scribble named-page))
                'named-page-label))
  (call-with-temporary-source
    "@use-modules[(tests unit scribble named-page)]@paragraph{@|named-page-label|}"
    (lambda (path)
      (let* ((document (read-document path))
             (paragraph (car (content-document-children document))))
        (test-equal "a named document module can be imported by later sources"
          "named module"
          (content-verbatim-string
            (content-element-children paragraph)))))))

(let* ((root (mkdtemp "/tmp/grove-scribble-modules-XXXXXX"))
       (module-directory (string-append root "/modules"))
       (module-path (string-append module-directory "/preloaded.plant"))
       (read-document (make-scribble-document-reader)))
  (dynamic-wind
    (lambda ()
      (mkdir module-directory)
      (call-with-output-file module-path
        (lambda (port)
          (display
            "@(define-module (tests unit scribble preloaded-module) #:use-module (forester commands) #:export (preloaded-label))\n@(define preloaded-label \"preloaded module\")"
            port))))
    (lambda ()
      (test-equal "module setup returns the loaded source paths"
        (list module-path)
        ((make-scribble-module-setup module-directory)))
      (call-with-temporary-source
        "@use-modules[(tests unit scribble preloaded-module)]@paragraph{@|preloaded-label|}"
        (lambda (page-path)
          (let* ((document (read-document page-path))
                 (paragraph (car (content-document-children document))))
            (test-equal "preloaded named modules are available to pages"
              "preloaded module"
              (content-verbatim-string
                (content-element-children paragraph)))))))
    (lambda () (system* "rm" "-rf" root))))

(let ((read-document (make-scribble-document-reader)))
  (call-with-temporary-source
    "\nFirst paragraph.\n\nSecond @bold{paragraph}.\n"
    (lambda (path)
      (let* ((document (read-document path))
             (paragraphs (content-document-children document))
             (first-paragraph (car paragraphs))
             (second-paragraph (cadr paragraphs))
             (first-text (car (content-element-children first-paragraph))))
        (test-equal "paragraph IR records its first source line and column"
          `((file . ,path) (line . 2) (column . 1))
          (content-element-property first-paragraph 'source-location))
        (test-equal "text IR retains its source position"
          `((file . ,path) (line . 2) (column . 1))
          (content-text-location first-text))
        (test-equal "inline commands retain the command source position"
          `((file . ,path) (line . 4) (column . 8))
          (content-element-property
            (cadr (content-element-children second-paragraph))
            'source-location))))))

(let ((phases '()))
  (let ((document
          (scribble-evaluate-document (open-input-string "@bold{timed}")
            (default-environment scribble)
            #:timings (lambda (phase seconds)
                        (set! phases (append phases (list (cons phase seconds))))))))
    (test-assert "timed evaluation still produces document content" (content-document? document))
    (test-equal "timings cover distinct reading passes"
      '(parse translate evaluate decode) (map car phases))
    (test-assert "timings report non-negative durations"
      (every (lambda (entry) (and (number? (cdr entry)) (>= (cdr entry) 0))) phases))))


(let ((env (default-environment scribble)))
  (evaluate-document env "@(define literal-newline \"\n\")")
  (test-equal "literal Scheme newlines remain strings, not layout constructors"
    "\n" (module-ref env 'literal-newline)))

(test-read "ellipsis is a Scheme identifier" '... "...")
(test-read/approximate "leading decimal point remains numeric" 0.5 ".5" 0.001)
(test-read "syntax shorthand preserves its datum" '(syntax (x ...)) "#'(x ...)")

(let* ((env (default-environment scribble))
       (document
         (evaluate-document env
           "@(define environment '((x . 3)))
@(define (sum x . rest) (+ x (apply + rest)))
@(define-syntax values-text
   (syntax-rules ()
     ((_ value ...) (verbatim (format #f \"~s\" (list value ...))))))
@values-text[(sum (cdar environment) 4 5) '((x . 3))]")))
  (test-equal "quoted pairs, rest definitions, and ellipsis macros execute in articles"
    "(12 ((x . 3)))" (content->text document)))

(let* ((env (default-environment scribble))
       (source
         "@use-modules[(forester commands) (language scribble source)]
@(define-syntax program
   (lambda (stx)
     (syntax-case stx ()
       ((_ id heading definition)
        (let ((text (source-text (syntax-source #'definition))))
          (with-syntax ((code (datum->syntax stx text)))
            #'(begin definition
                (subtree #:id id (title heading)
                  (code-block #:language 'scheme code)))))))))
@program[\"square-code\" \"Square\"]{@(define (square x) ; 内部注释
  (* x x))}
@verbatim[(number->string (square 7))]")
       (document (evaluate-document env source))
       (rendered (content->text document)))
  (test-assert "a writer macro displays exact code without stripping internal wrappers"
    (string-contains rendered "@(define (square x) ; 内部注释\n  (* x x))"))
  (test-assert "the displayed definition is also executable"
    (string-suffix? "49" rendered)))

(let* ((source "; outside\n@inspect[(define (f λ) ; inside\n  λ)] ; after")
       (port (open-input-string source)))
  (set-port-filename! port "ranges.plant")
  (let* ((form (find pair? (scribble-parse port)))
         (definition (cadr form))
         (properties (source-properties definition))
         (range (source-range properties)))
    (test-equal "reader exposes exact expression text with internal comments"
      "(define (f λ) ; inside\n  λ)" (source-text properties))
    (test-equal "range starts at the Scheme argument, not its enclosing command"
      '(2 10) (list (assq-ref range 'line) (assq-ref range 'column)))
    (test-equal "range offsets are characters rather than UTF-8 bytes"
      (source-text properties)
      (substring source (assq-ref range 'start-offset) (assq-ref range 'end-offset)))))
(let* ((text "λ\t@inspect[(list \"λ\"\t3)]")
       (form (find pair? (scribble-parse (open-input-string text)))))
  (test-equal "Unicode and tab-expanded columns do not corrupt source slices"
    "(list \"λ\"\t3)" (source-text (source-properties (cadr form)))))
(test-equal "missing reader ranges do not invent source text" #f (source-text #f))

(call-with-temporary-source
  "@(define-module (tests literate-provider) #:export (answer))\n@use-modules[(language scribble source)]\n@(define answer \"one\")\nProvider."
  (lambda (provider)
    (call-with-temporary-source
      "@use-modules[(tests literate-provider)]\n@verbatim[answer]"
      (lambda (consumer)
        (let ((executions 0)
              (base-reader (make-scribble-document-reader)))
          (call-with-values
            (lambda ()
              (make-scribble-module-session (list provider)
                #:read-document
                (lambda (path)
                  (when (string=? path provider) (set! executions (+ executions 1)))
                  (base-reader path))))
            (lambda (prepare read-document)
              (prepare)
              (test-equal "module articles record their actual .plant filename"
                provider (module-filename (resolve-module '(tests literate-provider))))
              (let ((trace (make-hash-table)))
                (call-with-input-trace trace (lambda () (read-document provider)))
                (test-assert "prepared document reuse replays module-article dependencies"
                  (and (member (canonicalize-path provider) (trace-inputs trace))
                       (member (canonicalize-path (%search-load-path "language/scribble/source.scm"))
                               (trace-inputs trace)))))
              (let ((first (read-document provider)))
                (test-assert "module article binding and display share one execution"
                  (eq? first (read-document provider)))
                (test-equal "prepared module is not executed again for display" 1 executions))
              (let ((trace (make-hash-table)))
                (call-with-input-trace trace (lambda () (read-document consumer)))
                (test-assert "imported .plant module is traced without blanket dependencies"
                  (member (canonicalize-path provider) (trace-inputs trace))))
              (call-with-output-file provider
                (lambda (port)
                  (display "@(define-module (tests literate-provider) #:export (answer))\n@(define answer \"two\")\nProvider." port)))
              (prepare)
              (test-equal "new preparation reexecutes module articles" 2 executions)
              (test-equal "consumer sees updated bindings after preparation"
                "two" (content->text (read-document consumer))))))))))

(test-end "scribble read")
