;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove highlight) (srfi srfi-1) (srfi srfi-64))

;; Temporary grammar source until grammars are packaged with Grove.
;; CI or non-Neovim setups can supply an absolute shared-library path.
(define grammar
  (or (getenv "GROVE_TEST_SCHEME_GRAMMAR")
      (string-append (getenv "HOME")
        "/.local/share/nvim/lazy/nvim-treesitter/parser/scheme.so")))
(define (highlighter query) (make-highlighter grammar "tree_sitter_scheme" query))
(define (styled query source)
  (filter car (highlight-code (highlighter query) source)))

(test-begin "Tree-sitter highlighting")
(define basic
  (highlighter "(symbol) @variable (number) @number (string) @string (comment) @comment
                ((symbol) @keyword (#eq? @keyword \"define\"))"))
(test-equal "later patterns override general captures without duplicating text"
  '((#f . "(") ("keyword" . "define") (#f . " ") ("variable" . "x")
    (#f . " ") ("number" . "2") (#f . ")"))
  (highlight-code basic "(define x 2)"))
(for-each
  (lambda (source)
    (test-equal "all original text survives parsing and range segmentation"
      source (apply string-append (map cdr (highlight-code basic source)))))
  '("" "   \n\t" "(define (平方 ◆:x) ; 中文🙂\n  (+ ◆:x 2))\n"
    "(define x" "\"<&>\" ; comment\n" "(a)(b)"))
(test-equal "Unicode capture offsets are byte offsets, not string indices"
  '(("variable" . "平方") ("variable" . "◆:x"))
  (styled "(symbol) @variable" "(平方 ◆:x)"))
(test-equal "equality predicates filter structural matches"
  '(("keyword" . "define"))
  (styled "((symbol) @keyword (#eq? @keyword \"define\"))" "(define x x)"))
(test-equal "negative equality excludes only the matching value"
  '(("variable" . "x"))
  (styled "((symbol) @variable (#not-eq? @variable \"define\"))" "(define x)"))
(test-equal "capture comparisons and hidden captures work together"
  '(("same" . "x"))
  (styled "((list . (symbol) @_left . (symbol) @same) (#eq? @_left @same))"
          "(x x) (x y)"))
(test-equal "regex predicates use POSIX extended expressions"
  '(("name" . "foo"))
  (styled "((symbol) @name (#match? @name \"^f.*$\"))" "(foo bar)"))
(test-equal "negative regex filters matching names"
  '(("name" . "bar"))
  (styled "((symbol) @name (#not-match? @name \"^f.*$\"))" "(foo bar)"))
(test-equal "any-of accepts several alternatives"
  '(("keyword" . "define") ("keyword" . "lambda"))
  (styled "((symbol) @keyword (#any-of? @keyword \"define\" \"lambda\"))"
          "(define lambda x)"))
(test-equal "single alternative and negative membership are supported"
  '(("name" . "x"))
  (styled "((symbol) @name (#not-any-of? @name \"define\"))" "(define x)"))
(test-equal "multiple predicates must all hold"
  '(("name" . "foo"))
  (styled "((symbol) @name (#match? @name \"^f\") (#not-eq? @name \"far\"))"
          "(foo far bar)"))
(test-equal "explicit priority wins over later query patterns"
  '(("special" . "x"))
  (styled "((symbol) @special (#set! priority 110)) (symbol) @general" "x"))
(test-equal "nested captures split a larger range without losing its remainder"
  '(("whole" . "(") ("number" . "2") ("whole" . " x)"))
  (highlight-code (highlighter "(list) @whole (number) @number") "(2 x)"))
(test-equal "narrower captures win within the same pattern"
  '(("outer" . "(") ("inner" . "x") ("outer" . ")"))
  (highlight-code (highlighter "(list (symbol) @inner) @outer") "(x)"))
(test-equal "higher priority enclosing captures override narrower captures"
  '(("whole" . "(2 x)"))
  (highlight-code
    (highlighter "((list) @whole (#set! priority 110)) (number) @number") "(2 x)"))
(test-equal "hidden-only captures leave the source unstyled"
  '((#f . "(x)")) (highlight-code (highlighter "(symbol) @_name") "(x)"))
(test-equal "empty queries return the original source"
  '((#f . "(x)")) (highlight-code (highlighter "") "(x)"))
(for-each
  (lambda (query) (test-error "unsupported or malformed directives fail at setup" #t
                   (highlighter query)))
  '("((symbol) @x (#unknown? @x))"
    "((symbol) @x (#set! priority nope))"
    "((symbol) @x (#set! conceal \"x\"))"
    "((symbol) @x (#eq? @x))"
    "((symbol) @x (#match? @x \"[\"))"))
(test-error "invalid query syntax is reported" #t (highlighter "(not_a_node) @x"))
(test-error "missing grammar libraries are reported" #t
  (make-highlighter "/tmp/grove-no-such-grammar.so" "tree_sitter_scheme" ""))
(test-equal "a highlighter remains reusable after collection"
  (highlight-code basic "(define x 2)")
  (begin (gc) (highlight-code basic "(define x 2)")))
(test-end "Tree-sitter highlighting")
