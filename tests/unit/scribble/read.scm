(define-module (tests unit scribble read)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (language scribble parser))

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

;; XXX: Is this right?
(test-read/approximate "float"
  42.2
  "42.42"
  0.01)

(test-read/error "invalid . in source"
  "42.42.")

(test-read/error "too many )'s"
  ")")

(test-end "scribble read")
