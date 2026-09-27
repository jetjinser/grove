;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit scribble read)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (system base language)
  #:use-module (language scheme spec)
  #:use-module (language scribble parser)
  #:use-module ((language scribble spec)
                #:select (scribble scribble-evaluate-document))
  #:use-module (grove input scribble)
  #:use-module (grove ir))

(define (read-with-scheme-language env input)
  ((language-reader scheme) (open-input-string input) env))

(define (evaluate-document env input)
  (scribble-evaluate-document (open-input-string input) env))

(define temporary-source-index 0)

(define (call-with-temporary-source input proc)
  (let ((path
          (format #f "/tmp/grove-scribble-~a-~a.grv"
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
             "@use-modules[(forester commands)]\n@(define note-label \"from Scheme\")\n@title{@|note-label|}\n@link[\"target\"]{Target}"))
         (children (content-document-children document))
         (elements (filter content-element? children))
         (title (car elements))
         (link (last elements)))
    (test-equal "source can import a module before using its bindings"
      'link
      (content-element-kind link))
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
      '(bold)
      (map content-element-kind
           (filter content-element?
                   (content-document-children document))))
    (test-equal "Scheme expressions run in source order"
      '(last first)
      (module-ref env 'evaluation-order))))

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

(test-end "scribble read")
