;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit scribble read)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (system base language)
  #:use-module (language scheme spec)
  #:use-module (language scribble parser))

(define (read-with-scheme-language env input)
  ((language-reader scheme) (open-input-string input) env))

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

(test-end "scribble read")
