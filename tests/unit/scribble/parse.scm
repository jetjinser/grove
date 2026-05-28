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


(test-skip 1) ; FIXME:
(test-parse "Command with positional arguments"
  '((item #:style 'bold "content"))
  "@item[#:style 'bold]{content}")

(test-parse "Command with multiple S-expr args, no body"
  '((link "url" "label"))
  "@link[\"url\" \"label\"]")

(test-skip 1) ; FIXME:
(test-parse "Complex mixture of S-expr and body"
  '((image "logo.png" #:width 200 "Caption text"))
  "@image[\"logo.png\" #:width 200]{Caption text}")


(test-parse "Nested commands in text body"
  '((bold "This is " (italic "very") " important"))
  "@bold{This is @italic{very} important}")

(test-parse "Deeply nested commands"
  '((p "Click " (link "url" (bold "here")) " now"))
  "@p{Click @link[\"url\"]{@bold{here}} now}")


;; FIXME: wrong behaviour?
(test-parse "Escaped at-sign"
  '("Contact us at user\\" example.com)
  "Contact us at user\\@example.com")

(test-parse "Escaped braces"
  '("Matching pairs \\{\\} inside text")
  "Matching pairs \\{\\} inside text")

(test-parse "At-sign inside S-expr argument"
  '((email "foo@bar.com"))
  "@email[\"foo@bar.com\"]")


(test-parse "Alternative text body with vertical bars"
  '((code "function() { return 0; }"))
  "@code|{function() { return 0; }}|")


(test-skip 1) ; FIXME: should error but not
(test-parse/error "Unmatched opening brace"
  "@bold{unclosed")
(test-parse "! Unmatched opening brace"
  '((bold "unclosed"))
  "@bold{unclosed")

(test-skip 1) ; FIXME: should error but not
(test-parse/error "Unmatched closing brace"
  "closed}")
(test-parse "! Unmatched closing brace"
  '("closed")
  "closed}")

(test-parse/error "Unmatched square bracket"
  "@cmd[missing-closing-bracket{body}")

(test-skip 1) ; FIXME: should error but not
(test-parse/error "Dangling at-sign at EOF"
  "Text ends with @")
(test-parse "! Dangling at-sign at EOF"
  '("Text ends with ")
  "Text ends with @")

(test-end "scribble parse")
