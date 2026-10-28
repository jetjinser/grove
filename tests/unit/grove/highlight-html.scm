;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove highlight) (grove ir) (grove build) (grove output)
             (grove render html) (grove render text) (grove render html routes)
             (grove private renderer)
             (grove style) (grove style html) (grove style html element)
             (grove style highlight)
             (forester render html) (forester site) (grove site)
             ((language scribble commands) #:prefix command:)
             (tests unit forester fixtures)
             (srfi srfi-1) (srfi srfi-13) (srfi srfi-64))

(define grammar
  (or (getenv "GROVE_TEST_SCHEME_GRAMMAR")
      (car (highlighter-configuration (assq-ref (default-highlighters) 'scheme)))))
(define query "(symbol) @variable (string) @string
               ((symbol) @keyword (#eq? @keyword \"define\"))")
(define highlights `((scheme . ,(make-highlighter grammar "tree_sitter_scheme" query))))
(define (html block) (sxml->html (content->sxml block #:highlighters highlights)))

(test-begin "HTML code highlighting")
(define document (evaluate-document "@code-block[#:language 'scheme]{(define 平方 \"<&>\")}"))
(define block (car (content-document-children document)))
(test-equal "at-exp records the language without interpreting source"
  'scheme (content-element-property block 'language))
(test-assert "HTML uses packaged Scheme highlighting without explicit configuration"
  (string-contains (sxml->html (content->sxml block)) "data-syntax=\"keyword\""))
(test-assert "an empty mapping disables default highlighting"
  (let* ((port (open-output-string))
         (output (parameterize ((current-error-port port))
                   (sxml->html (content->sxml block #:highlighters '())))))
    (and (not (string-contains output "<span"))
         (string-contains (get-output-string port) "scheme"))))
(test-equal "source remains plain text in Content IR"
  "(define 平方 \"<&>\")" (content-verbatim-string (content-element-children block)))
(test-assert "HTML emits language and syntax attributes with escaped text"
  (let ((output (html block)))
    (and (string-contains output "data-language=\"scheme\"")
         (string-contains output "data-syntax=\"keyword\">define</span>")
         (string-contains output "data-syntax=\"variable\">平方</span>")
         (not (string-contains output "class=\"syntax-"))
         (not (string-contains output "class=\"language-"))
         (string-contains output "&lt;&amp;&gt;")
         (not (string-contains output "<&>")))))
(test-assert "dotted and hyphenated capture names remain distinct"
  (let* ((h (make-highlighter grammar "tree_sitter_scheme"
              "(symbol) @function.builtin
               ((symbol) @function-builtin (#eq? @function-builtin \"x\"))"))
         (output (sxml->html
                   (content->sxml (command:code-block #:language 'scheme "(f x)")
                     #:highlighters `((scheme . ,h))))))
    (and (string-contains output "data-syntax=\"function.builtin\">f</span>")
         (string-contains output "data-syntax=\"function-builtin\">x</span>"))))
(test-equal "blocks without a language remain plain code"
  "<pre translate=\"no\"><code translate=\"no\">&lt;&amp;&gt;</code></pre>"
  (html (command:code-block "<&>")))
(test-assert "unknown languages warn and retain escaped source"
  (let* ((port (open-output-string))
         (output (parameterize ((current-error-port port))
                   (html (command:code-block #:language 'unknown "<&>")))))
    (and (string-contains output "&lt;&amp;&gt;")
         (not (string-contains output "<span"))
         (string-contains (get-output-string port) "unknown"))))
(test-equal "text rendering preserves source without highlight markup"
  "```\n(define 平方 \"<&>\")\n```\n" (content->text block))
(test-assert "existing style attributes apply to the outer pre"
  (string-contains
    (html (command:code-block #:language 'scheme
            #:style (make-content-style 'example
                      (list (make-html-attributes '((id . "sample"))))) "x"))
    "<pre translate=\"no\" class=\"example\" id=\"sample\">"))
(test-assert "string languages are accepted"
  (string-contains
    (html (command:code-block #:language "scheme" "(define x)"))
    "data-language=\"scheme\""))
(test-error "invalid language options fail at the call site" #t
  (command:code-block #:language 42 "x"))
(test-error "invalid renderer mappings fail explicitly" #t
  (content->sxml block #:highlighters '((scheme . invalid))))
(test-assert "incomplete code still renders"
  (string-contains (html (command:code-block #:language 'scheme "(define x"))
                   "data-syntax=\"keyword\""))

(define forest
  (make-forest
    (make-site
      (list (make-site-page "home" "Home" (evaluate-document "@transclude[\"code\"]"))
            (make-site-page "code" "Code" document)))))
(define routes (make-html-routes #:home-page "home"))
(define renderer
  (make-html-renderer
    #:options (make-html-options #:routes routes
                                 #:highlighters highlights)))
(define artifacts ((cached-renderer-procedure renderer) forest))
(test-assert "standalone pages and transclusions both receive the highlighters"
  (every (lambda (path)
           (string-contains
             (artifact-content (find (lambda (artifact) (string=? path (artifact-path artifact))) artifacts))
             "data-syntax=\"keyword\"")) '("index.html" "code.html")))
(test-assert "incremental rendering also retains highlighting"
  (let ((updated ((cached-renderer-render-changed renderer) forest '() '() '())))
    (string-contains
      (artifact-content (find (lambda (artifact) (string=? "code.html" (artifact-path artifact))) updated))
      "data-syntax=\"keyword\"")))
(test-assert "default pages load the highlight stylesheet"
  (string-contains
    (artifact-content (find (lambda (artifact) (string=? "code.html" (artifact-path artifact))) artifacts))
    "href=\"/highlight.css\""))
(test-assert "grammar libraries are tracked renderer dependencies"
  (member (canonicalize-path grammar) ((cached-renderer-dependencies renderer))))
(test-assert "changed grammars prevent reuse of old highlighted pages"
  (let* ((old (map (lambda (artifact)
                    (if (string=? "code.html" (artifact-path artifact))
                        (make-artifact "code.html" "stale"
                                       #:sources (artifact-sources artifact))
                        artifact)) artifacts))
         (updated ((cached-renderer-render-changed renderer)
                   forest (list (canonicalize-path grammar)) '() old)))
    (string-contains
      (artifact-content (find (lambda (artifact) (string=? "code.html" (artifact-path artifact))) updated))
      "data-syntax=\"keyword\"")))
(test-assert "query changes participate in renderer cache configuration"
  (not (equal? ((cached-renderer-configuration renderer) forest)
               ((cached-renderer-configuration
                  (make-html-renderer
                    #:options
                    (make-html-options
                      #:routes routes
                      #:highlighters
                      `((scheme . ,(make-highlighter
                                     grammar "tree_sitter_scheme"
                                     "(symbol) @name"))))))
                forest))))
(test-assert "consumers can compose a stylesheet after the default palette"
  (equal? '("highlight.css" "custom.css")
    (map artifact-path
      (html-style-stylesheets
        (compose-html-styles default-highlight-style
          (make-html-style #:stylesheets (list (make-artifact "custom.css" "pre { --syntax-keyword: red; }"))))))))
(test-end "HTML code highlighting")
