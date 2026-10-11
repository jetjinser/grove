;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove content)
  #:use-module (srfi srfi-64)
  #:use-module (grove element)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove render text)
  #:use-module (grove build)
  #:use-module (grove site)
  #:use-module (forester render html)
  #:use-module (forester site))

(test-begin "grove content")

(test-error "content text values are strings" #t
  (make-content-text 42))
(test-error "content layout kinds are symbols" #t
  (make-content-layout "newline" "\n"))
(test-error "content layout raw values are strings" #t
  (make-content-layout 'newline 10))
(test-error "element kinds are symbols" #t
  (make-content-element "paragraph" '()))
(test-error "content elements reject non-content children" #t
  (make-content-element 'paragraph '(42)))
(test-error "content documents reject non-content children" #t
  (make-content-document '(42)))
(test-error "content elements reject nested documents" #t
  (make-content-element 'paragraph
    (list (make-content-document '("nested")))))
(test-error "element property keys are symbols" #t
  (make-content-element 'paragraph '() #:properties '(("role" . "status"))))
(test-error "element property keys are unique" #t
  (make-content-element 'paragraph '()
    #:properties '((role . "status") (role . "note"))))
(test-error "element flow kinds are constrained" #t
  (make-content-element 'paragraph '() #:flow-kind 'floating))

(let* ((notice
         (make-content-element 'notice '("Hello")
           #:properties '((role . "status"))))
       (document (make-content-document (list notice))))
  (test-assert "arbitrary element kinds stay in the generic IR"
    (and (content-element? notice)
         (eq? (content-element-kind notice) 'notice)
         (equal? (content-element-property notice 'role) "status")))
  (let ((block (make-content-element 'custom-panel '()
                 #:flow-kind 'block)))
    (test-assert "custom elements can declare block flow semantics"
      (and (content-element-block? block)
           (eq? (content-element-flow-kind block) 'block)))
    (test-assert "flow semantics survive ordinary IR reconstruction"
           (content-element-block?
        (make-content-element
          (content-element-kind block)
          (content-element-children block)
          #:properties (content-element-properties block)
          #:flow-kind (content-element-flow-kind block)))))
  (let ((styled
          (make-content-element 'notice '("Hello")
            #:style
            (make-content-style 'callout
              (list (make-html-attributes
                      '((id . "greeting") (class . "loud")
                        (data-kind . "salutation"))))))))
    (test-equal "HTML style names and attributes attach to the default tag"
      '(span (@ (class "notice callout loud")
               (id "greeting") (data-kind "salutation"))
         "Hello")
      (content->sxml styled)))
  (let ((styled
          (make-content-element 'paragraph '("Hello")
            #:style (make-content-style 'callout
                      (list (make-html-tag 'aside)
                            (make-html-attributes '((role . "note"))))))))
    (test-equal "HTML style can select a tag while retaining its style class"
      '(aside (@ (class "callout") (role "note")) "Hello")
      (content->sxml styled)))
  (test-error "HTML attributes require unique string-valued pairs" #t
    (make-html-attributes '((id . "one") (id . "two"))))
  (test-equal "HTML supplies a generic fallback for open element kinds"
    '(article (span (@ (class "notice")) "Hello"))
    (content->sxml document))
  (test-equal "renderers can add per-kind behavior without changing the IR"
    "notice: Hello"
    (content->text document
      #:handlers
      `((notice . ,(lambda (element body)
                     (string-append
                       (symbol->string (content-element-kind element))
                       ": " body)))))))

(let* ((paragraph (make-content-element 'paragraph '("Nested.")))
       (inner-document (make-content-document (list paragraph)))
       (document (make-content-document (list inner-document))))
  (test-equal "document sequences flatten nested documents"
    (list paragraph)
    (content-document-children document)))

(let* ((document
         (make-content-document
           (list (make-content-element 'paragraph '("Details"))
                 (make-content-element 'blockquote '("A short quote."))
                 (make-content-element
                   'itemize
                   (list (make-content-element 'item '("First"))
                         (make-content-element 'item '("Second"))))
                 (make-content-element 'paragraph
                   (list "Run " (make-content-element 'code '("x + 1")) "."))
                 (make-content-element 'code-block '("(display \"hi\")")))))
       (text (content->text document)))
  (test-equal "HTML renders common document structures semantically"
    '(article
       (p "Details")
       (blockquote "A short quote.")
       (ul (li "First") (li "Second"))
       (p "Run " (code "x + 1") ".")
       (pre (code "(display \"hi\")")))
    (content->sxml document))
  (test-equal "plain text keeps structure, bullets, and code readable"
    (string-append
      "Details\n"
      "> A short quote.\n"
      "- First\n- Second\n"
      "Run `x + 1`.\n"
      "```\n(display \"hi\")\n```\n")
    text))

(let* ((wbr (make-element #:tag "wbr" #:void? #t))
       (note (make-element #:tag "my:note"
                           #:attributes '((xmlns:my . "https://example.com/ns"))))
       (callout (make-element #:tag "aside" #:class "callout"
                              #:flow-kind 'block))
       (document
         (make-content-document
           (list (make-content-element
                   'paragraph
                   (list "a" (wbr) "b" (note "注") (callout "提示")))))))
  (test-equal "custom elements render their own shape with no invented class"
    (string-append
      "<article><p>a<wbr/>b"
      "<my:note xmlns:my=\"https://example.com/ns\">注</my:note>"
      "<aside class=\"callout\">提示</aside></p></article>")
    (sxml->html (content->sxml document)))
  (test-equal "text output of custom elements is their body"
    "ab注提示\n"
    (content->text document))
  (test-error "void elements reject content" #t (wbr "oops"))
  (test-error "invalid tag names fail at definition" #t
    (make-element #:tag "1bad")))

(let* ((base (make-content-style 'base (list (make-html-attributes '((role . "region"))))))
       (tagged (make-content-style #f (list (make-html-tag 'aside))))
       (accent (make-content-style 'accent
                 (list (make-html-attributes '((data-print . "high"))))))
       (merged (merge-content-style base tagged accent))
       (element (make-content-element 'paragraph '("Body") #:style merged)))
  (test-equal "merged styles join names and keep every property set"
    '(aside (@ (class "base accent") (role "region")
               (data-print "high"))
         "Body")
    (content->sxml element))
  (test-error "merging requires style values" #t
    (merge-content-style base 'not-a-style)))

(test-equal "the HTML serializer emits declared void elements self-closed"
  "<p>a<wbr/><em>x</em></p>"
  (sxml->html '(p "a" (*void* wbr) (em "x"))))

(test-equal "childless non-void elements get explicit end tags"
  "<div class=\"x\"><span class=\"y\"></span></div>"
  (sxml->html '(div (@ (class "x")) (span (@ (class "y"))))))
(test-equal "childless void tags stay self-closed"
  "<img src=\"/a.svg\" alt=\"A\" />"
  (sxml->html '(img (@ (src "/a.svg") (alt "A")))))
(test-equal "raw-text elements serialize once and unescaped"
  "<script>var x = 1 && 2;</script>"
  (sxml->html '(script "var x = 1 && 2;")))

(test-equal "make-element passes its flow kind through"
  'block
  (content-element-flow-kind
    ((make-element #:tag "figure" #:flow-kind 'block) "body")))
(test-equal "make-element defaults to inline flow"
  'inline
  (content-element-flow-kind
    ((make-element #:tag "span") "body")))

(let ((definition (make-element #:tag "dfn" #:anchor "term-id")))
  (define element (definition "a term"))
  (test-equal "make-element registers its anchor as a property"
    "term-id"
    (content-element-property element 'anchor))
  (test-equal "make-element renders the anchor as the tag's id"
    "<dfn id=\"term-id\">a term</dfn>"
    (sxml->html (content->sxml element))))

(let* ((definition (make-element #:tag "dfn" #:anchor "term-id"))
       (page (make-site-page "glossary" "Glossary"
               (make-content-document
                 (list (make-content-element
                         'paragraph
                         (list (definition "a term")))))))
       (forest (make-forest (make-site (list page)))))
  (test-equal "declared anchors enter the page's anchor registry"
    '(("anchor" . "tree-root-glossary") ("anchor" . "term-id"))
    (page-anchor-facts page))
  (test-assert "the anchored element renders on its page"
    (string-contains
      (assoc-ref (site->html forest
                             #:routes (make-html-routes
                                        #:home-page "glossary"))
                 "glossary")
      "<dfn id=\"term-id\">a term</dfn>")))

(test-end "grove content")
