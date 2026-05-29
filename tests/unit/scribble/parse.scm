;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit scribble parse)
  #:use-module (srfi srfi-64)
  #:use-module (language scribble parser))

(define-syntax-rule (test-parse test-name
                               expected
                               scribble-str)
  (test-equal test-name
    expected
    (scribble-parse (open-input-string scribble-str))))

(define-syntax-rule (test-parse/approximate test-name
                                           expected
                                           scribble-str
                                           error)
  (test-approximate test-name
    expected
    (scribble-parse (open-input-string scribble-str))
    error))

(define-syntax-rule (test-parse/error test-name
                                     scribble-str)
  (test-error test-name #t
    (scribble-parse (open-input-string scribble-str))))


(test-begin "scribble parse")

(test-parse "1 word"
  '("answer")
  "answer")

(test-parse "Multiple words with spaces"
  '("Hello world, this is Scheme!")
  "Hello world, this is Scheme!")

(test-parse "Newlines and indentation"
  '("Line 1" "\n" "Line 2" "\n" "  Indented Line 3")
  "Line 1\nLine 2\n  Indented Line 3")


(test-parse "Command only"
  '(foo)
  "@foo")

(test-parse "Command followed by text"
  '(foo " bar")
  "@foo bar")

(test-parse "Command nested in text"
  '("Hello " bold " world")
  "Hello @bold world")


(test-parse "Command with curly braces"
  '((italic "text"))
  "@italic{text}")

(test-parse "Command with multiple words in body"
  '((bold "inside the block"))
  "@bold{inside the block}")

(test-parse "Multiline body text"
  '((section "Title" "\n" "Subsection"))
  "@section{Title\nSubsection}")


(test-parse "Command with positional arguments"
  '((item #:style 'bold "content"))
  "@item[#:style 'bold]{content}")

(test-parse "Command with positional arguments nested"
  '((item #:style 'bold "Hello " (bold #:class 'noun "world") "!"))
  "@item[#:style 'bold]{Hello @bold[#:class 'noun]{world}!}")

(test-parse "Command with multiple S-expr args, no body"
  '((link "url" "label"))
  "@link[\"url\" \"label\"]")

(test-parse "Complex mixture of S-expr and body"
  '((image "logo.png" #:width 200 "Caption text"))
  "@image[\"logo.png\" #:width 200]{Caption text}")


(test-parse "Nested commands in text body"
  '((bold "This is " (italic "very") " important"))
  "@bold{This is @italic{very} important}")

(test-parse "Deeply nested commands"
  '((p "Click " (link "url" (bold "here")) " now"))
  "@p{Click @link[\"url\"]{@bold{here}} now}")


(test-parse "Escaped at-sign"
  '("Contact us at user@example.com")
  "Contact us at user@\"@\"example.com")

(test-parse "Escaped braces"
  '("Matching pairs \\{\\} inside text")
  "Matching pairs \\{\\} inside text")

(test-parse "At-sign inside S-expr argument"
  '((email "foo@bar.com"))
  "@email[\"foo@bar.com\"]")


(test-parse "Alternative text body with vertical bars"
  '((code "function() { return 0; }"))
  "@code|{function() { return 0; }}|")


(test-parse/error "Unmatched opening brace"
  "@bold{unclosed")

(test-parse/error "Unmatched closing brace"
  "closed}")

(test-parse/error "Unmatched square bracket"
  "@cmd[missing-closing-bracket{body}")

(test-parse/error "Dangling at-sign at EOF"
  "Text ends with @")


(test-parse "Float case 1" '(123.456)   "@123.456")
(test-parse "Float case 2" '((123.456)) "@(123.456)")
(test-parse "Float case 3" '((123.456)) "@(123.456 )")


(test-parse "Empty curly braces body"
  '((foo))
  "@foo{}")

(test-parse "Empty square bracket args"
  '((bar))
  "@bar[]")

(test-parse "Body with only whitespace"
  '((cmd "   "))
  "@cmd{   }")

(test-parse "Body with only newline"
  '((cmd))
  "@cmd{\n}")


(test-parse "Adjacent commands without space"
  '((a "1") (b "2"))
  "@a{1}@b{2}")

(test-parse "Command embedded between text no space"
  '("prefix" (cmd "body") "suffix")
  "prefix@cmd{body}suffix")

(test-parse "Command adjacent to trailing text"
  '((cmd "body") "suffix")
  "@cmd{body}suffix")


(test-parse "Nested command with trailing text in body"
  '((outer (inner "deep") "tail"))
  "@outer{@inner{deep}tail}")

(test-parse "Deeply nested body-only commands"
  '((a (b (c "z"))))
  "@a{@b{@c{z}}}")

(test-parse "Sibling commands inside body"
  '((p (b "1") " " (i "2")))
  "@p{@b{1} @i{2}}")


(test-parse "Nested command inside pipe body"
  '((code "x = @var{y};"))
  "@code|{x = @var{y};}|")

(test-parse "Pipe body containing unescaped braces"
  '((code "if (x) { y(); }"))
  "@code|{if (x) { y(); }}|")

(test-parse "Custom pipe delimiters"
  '((script "alert('hi');"))
  "@script|<<{alert('hi');}>>|")


(test-parse "Line comment before text"
  '("after")
  "@; before\nafter")

(test-parse "Line comment after text"
  '("before ")
  "before @; comment")

(test-parse "Comment only input"
  '()
  "@; only comment")


(test-parse "Unicode plain text"
  '("你好，世界 🌍")
  "你好，世界 🌍")

(test-parse "Unicode command name"
  '(你好)
  "@你好")

(test-parse "Unicode string argument"
  '((echo "你好"))
  "@echo[\"你好\"]")

(test-parse "Emoji in command body"
  '((react "🎉🎊"))
  "@react{🎉🎊}")


(test-parse "Empty input"
  '()
  "")

(test-parse "Only whitespace"
  '("   ")
  "   ")

(test-parse "Only newlines"
  '()
  "\n")

(test-parse "Mixed whitespace characters"
  '(" \t" "\n" " ")
  " \t\n ")

(test-parse "Leading and trailing whitespace"
  '("  hello  ")
  "  hello  ")


(test-parse "S-expr list argument"
  '((cmd (list 1 2 3)))
  "@cmd[(list 1 2 3)]")

(test-parse "S-expr quoted symbol argument"
  '((cmd 'symbol))
  "@cmd['symbol]")

(test-parse "Numeric argument"
  '((cmd 42))
  "@cmd[42]")

(test-parse "Boolean argument"
  '((cmd #t))
  "@cmd[#t]")

(test-parse "Multiple string arguments"
  '((cmd "a" "b" "c"))
  "@cmd[\"a\" \"b\" \"c\"]")

(test-skip 1) ; TODO:
(test-parse "Multiple keyword args without body"
  '((figure #:width 100 #:height 200))
  "@figure[#:width 100 #:height 200]")


(test-parse "Hyphenated command name"
  '((my-cmd "body"))
  "@my-cmd{body}")

(test-parse "Command name with asterisk"
  '((cmd* "body"))
  "@cmd*{body}")


(test-skip 1) ; FIXME:
(test-parse/error "Unmatched pipe opening delimiter"
  "@cmd|{unclosed")

(test-skip 1) ; FIXME:
(test-parse/error "Unmatched custom pipe delimiter"
  "@cmd|<<{unclosed")

(test-skip 1) ; FIXME:
(test-parse/error "Empty command name"
  "@")

(test-parse/error "Unmatched opening square bracket"
  "@cmd[unclosed")

(test-parse/error "Invalid S-expr in square brackets"
  "@cmd[(]")

(test-end "scribble parse")
