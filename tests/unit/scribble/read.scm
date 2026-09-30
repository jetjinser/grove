;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit scribble read)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (system base language)
  #:use-module (language scheme spec)
  #:use-module (language scribble parser)
  #:use-module ((language scribble spec)
                #:select (scribble scribble-evaluate-document))
  #:use-module (grove input scribble)
  #:use-module (grove render html)
  #:use-module (grove ir))

(define (read-with-scheme-language env input)
  ((language-reader scheme) (open-input-string input) env))

(define (evaluate-document env input)
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

(let* ((env (default-environment scribble))
       (document
         (evaluate-document
           env
           (string-append
             "@display-math{@align{"
             "∇ · @vec{D} &= ρ\n"
             "∇ × @vec{E} &= −@frac{∂@vec{B}}{∂t}\n"
             "∇ · @vec{B} &= 0\n"
             "∇ × @vec{H} &= @vec{J} + @frac{∂@vec{D}}{∂t}"
             "}}\n"
             "@paragraph{Assume a homogeneous, isotropic, charge-free medium "
             "with @math{@vec{D}=ε@vec{E}}, @math{@vec{B}=μ@vec{H}}, "
             "and @math{@vec{J}=σ@vec{E}}. The x-component is "
             "@math{@sub{E}{x}}.}\n"
             "@display-math{@align{"
             "∇ × (∇ × @vec{E}) &= −∇ × @frac{∂@vec{B}}{∂t}\n"
             "&= −μ @frac{∂}{∂t} (∇ × @vec{H})\n"
             "&= −μ @frac{∂}{∂t} @parens{@vec{J} + @frac{∂@vec{D}}{∂t}}\n"
             "&= −μ @parens{@frac{∂@vec{J}}{∂t} + "
             "@frac{@sup{∂}{2}@vec{D}}{@sup{∂t}{2}}}\n"
             "&= −μσ @frac{∂@vec{E}}{∂t} − με "
             "@frac{@sup{∂}{2}@vec{E}}{@sup{∂t}{2}}\n"
             "∇ × (∇ × @vec{E}) &= ∇(∇ · @vec{E}) − ∇²@vec{E}\n"
             "&= −∇²@vec{E}\n"
             "−∇²@vec{E} &= −μσ @frac{∂@vec{E}}{∂t} − με "
             "@frac{@sup{∂}{2}@vec{E}}{@sup{∂t}{2}}\n"
             "∇²@vec{E} &= μσ @frac{∂@vec{E}}{∂t} + με "
             "@frac{@sup{∂}{2}@vec{E}}{@sup{∂t}{2}}"
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
             "@heading{A heading}\n@blockquote{Quoted}\n@itemize{@item{One}@item{Two}}\n@paragraph{@code{x + 1}}\n@code-block{(display \"hi\")}"))
         (elements (filter content-element?
                           (content-document-children document))))
    (test-equal "common content commands evaluate into generic IR elements"
      '(heading blockquote itemize paragraph code-block)
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
            "@use-modules[(forester commands)]\n@(map paragraph (text-paragraphs (lipsum \"1-3\")))")))
    (let ((paragraphs (filter content-element?
                              (content-document-children document))))
    (test-equal "Scheme expression lists splice into the document flow"
      '(paragraph paragraph paragraph)
      (map content-element-kind paragraphs))
    (test-assert "commands can be mapped over text without constructing IR"
      (and (string-prefix?
             "Lorem ipsum"
             (content-verbatim-string
               (content-element-children (car paragraphs))))
           (string-prefix?
             "Nam dui ligula"
             (content-verbatim-string
               (content-element-children (cadr paragraphs))))
           (string-prefix?
             "Nulla malesuada porttitor diam"
             (content-verbatim-string
               (content-element-children (caddr paragraphs)))))))))

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

(let ((read-document (make-scribble-document-reader)))
  (call-with-temporary-source
    "@(define-module (tests unit scribble preloaded-module) #:use-module (forester commands) #:export (preloaded-label))\n@(define preloaded-label \"preloaded module\")"
    (lambda (module-path)
      (load-scribble-modules! (list module-path))
      (call-with-temporary-source
        "@use-modules[(tests unit scribble preloaded-module)]@paragraph{@|preloaded-label|}"
        (lambda (page-path)
          (let* ((document (read-document page-path))
                 (paragraph (car (content-document-children document))))
            (test-equal "preloaded named modules are available to pages"
              "preloaded module"
              (content-verbatim-string
                (content-element-children paragraph)))))))))

(test-end "scribble read")
