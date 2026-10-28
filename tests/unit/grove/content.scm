;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove content)
  #:use-module (srfi srfi-64)
  #:use-module (grove render html element)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove render html)
  #:use-module ((grove render html attributes) #:prefix html-attributes:)
  #:use-module (grove render html routes)
  #:use-module (grove render text)
  #:use-module (grove render plain-text)
  #:use-module (grove render rules)
  #:use-module (grove build)
  #:use-module (grove site)
  #:use-module (forester render html)
  #:use-module (forester site))

(test-begin "grove content")

(test-assert "content source locations use one public accessor"
  (let ((public (module-public-interface (resolve-module '(grove ir)))))
    (and (module-variable public 'content-source-location)
         (not (module-variable public 'content-text-location)))))

(test-assert "anchor validation is an internal shared helper"
  (and (not (module-variable
              (module-public-interface (resolve-module '(grove ir)))
              'checked-anchor-id))
       (module-variable
         (module-public-interface (resolve-module '(grove private anchor)))
         'checked-anchor-id)))

(test-assert "HTML void markers stay behind the custom element API"
  (and (not (module-variable
              (module-public-interface (resolve-module '(grove style html)))
              'make-html-void))
       (not (module-variable
              (module-public-interface (resolve-module '(grove style html)))
              'html-void?))
       (module-variable
         (module-public-interface (resolve-module '(grove render html element)))
         'make-element)))

(test-equal "content styles expose scoped HTML attributes"
  '((class . "note") (id . "page-term") (href . "#page-term"))
  (html-attributes:content-style-html-attributes
    (make-content-style 'note
      (list (make-html-attributes
              '((id . "term") (href . "#term")))))
    "page-"))

(test-error "content style names cannot be empty" #t
  (make-content-style ""))

(test-equal "HTML style attributes preserve renderer attributes and merge classes"
  '((class . "tree note") (id . "tree-1") (data-kind . "note"))
  (html-attributes:merge-html-attributes
    '((class . "tree") (id . "tree-1"))
    '((class . "note") (data-kind . "note"))))

(test-error "HTML style attributes cannot replace renderer-owned IDs" #t
  (html-attributes:merge-html-attributes
    '((id . "tree-1")) '((id . "author-id"))))

(test-error "HTML style merge rejects duplicate renderer-owned attributes" #t
  (html-attributes:merge-html-attributes
    '((class . "first") (class . "second")) '()))

(test-equal "HTML style additions compose repeated class attributes in order"
  '((class . "tree first second"))
  (html-attributes:merge-html-attributes
    '((class . "tree")) '((class . "first") (class . "second"))))

(let* ((text-value (string-copy "hello"))
       (text-source (string-copy "page.plant"))
       (location-file (string-copy "page.plant"))
       (location (list (cons 'file location-file) (cons 'line 2)))
       (text (make-content-text text-value text-source #:location location))
       (layout-raw (string-copy "\n  "))
       (layout-source (string-copy "page.plant"))
       (layout (make-content-layout 'space layout-raw layout-source)))
  (string-set! text-value 0 #\x)
  (string-set! text-source 0 #\x)
  (string-set! location-file 0 #\x)
  (string-set! layout-raw 0 #\x)
  (string-set! layout-source 0 #\x)
  (test-equal "text and layout nodes snapshot caller-owned strings and locations"
    '("hello" "page.plant" ((file . "page.plant") (line . 2))
      "\n  " "page.plant")
    (list (content-text-value text)
          (content-text-source text)
          (content-source-location text)
          (content-layout-raw layout)
          (content-layout-source layout)))
  (let ((value (content-text-value text))
        (source (content-text-source text))
        (raw (content-layout-raw layout)))
    (string-set! value 0 #\x)
    (string-set! source 0 #\x)
    (string-set! raw 0 #\x))
  (test-equal "text and layout accessors return string snapshots"
    '("hello" "page.plant" "\n  ")
    (list (content-text-value text)
          (content-text-source text)
          (content-layout-raw layout))))

(let* ((child-text (string-copy "child"))
       (children (list child-text))
       (property-value (string-copy "note"))
       (properties (list (cons 'role property-value)))
       (element-source (string-copy "page.plant"))
       (element (make-content-element 'paragraph children element-source
                   #:properties properties
                   #:style (make-content-style 'emphasis)
                   #:flow-kind 'block))
       (document-source (string-copy "page.plant"))
       (document-children (list element))
       (document (make-content-document document-children document-source)))
  (string-set! child-text 0 #\x)
  (set-car! children "changed")
  (string-set! property-value 0 #\x)
  (set-cdr! (car properties) "changed")
  (string-set! element-source 0 #\x)
  (set-car! document-children #f)
  (string-set! document-source 0 #\x)
  (test-equal "content nodes snapshot child, property, and source inputs"
    '("child" "note" "page.plant" "page.plant" #t)
    (list (content-text-value (car (content-element-children element)))
          (content-element-property element 'role)
          (content-element-source element)
          (content-document-source document)
          (content-element? (car (content-document-children document)))))
  (let ((children (content-element-children element))
        (properties (content-element-properties element))
        (property (content-element-property element 'role))
        (document-children (content-document-children document)))
    (set-car! children #f)
    (set-cdr! (car properties) "changed")
    (string-set! property 0 #\x)
    (string-set! (content-element-source element) 0 #\x)
    (set-car! document-children #f))
  (test-equal "content node accessors return independent snapshots"
    '("child" "note" "page.plant" #t)
    (list (content-text-value (car (content-element-children element)))
          (content-element-property element 'role)
          (content-element-source element)
          (content-element? (car (content-document-children document)))))
  (let* ((new-property (string-copy "updated"))
         (updated (content-with-property element 'role new-property))
         (replaced-children (content-element-with-children element '("replacement")))
         (new-location
          (list (cons 'file (string-copy "other.plant")) (cons 'line 8)))
         (located (content-with-source-location element new-location)))
    (string-set! new-property 0 #\x)
    (string-set! (cdar new-location) 0 #\x)
    (test-equal "content derivation helpers snapshot new values"
      '("updated" ((file . "other.plant") (line . 8)) #f)
      (list (content-element-property updated 'role)
            (content-source-location located)
            (assq 'source-location (content-element-properties located))))
    (test-equal "replacing element children preserves its other fields"
      '(paragraph "replacement" "page.plant" ((role . "note")) emphasis block)
      (list (content-element-kind replaced-children)
            (content-text-value
              (car (content-element-children replaced-children)))
            (content-element-source replaced-children)
            (content-element-properties replaced-children)
            (content-style-name (content-element-style replaced-children))
            (content-element-flow-kind replaced-children)))))

(let* ((location '((file . "included.plant") (line . 12) (column . 4)))
       (original (make-content-document
                   (list (make-content-text "top-level")
                         (make-content-element
                           'paragraph (list "returned document")
                           "paragraph.plant"))
                   "origin.plant"))
       (located (content-with-source-location original location))
       (children (content-document-children located))
       (text (car children))
       (paragraph (cadr children)))
  (test-equal "source locations descend into returned documents and preserve source"
    (list "origin.plant" "paragraph.plant" location
          "included.plant" location #f #f)
    (list (content-document-source located)
          (content-element-source paragraph)
          (content-source-location paragraph)
          (content-text-source text)
          (content-source-location text)
          (content-text-source
            (car (content-element-children paragraph)))
          (content-source-location
            (car (content-element-children paragraph))))))

(test-equal "content source locations are absent on strings, layout, and documents"
  '(#f #f #f)
  (list (content-source-location "plain text")
        (content-source-location (make-content-layout 'newline "\n"))
        (content-source-location (make-content-document '()))))

(test-equal "source location annotation preserves non-content Scheme values"
  'project-setting
  (content-with-source-location
    'project-setting '((file . "project.scm") (line . 3))))

(let* ((location '((file . "computed.plant") (line . 3) (column . 5)))
       (located (content-with-source-location "computed text" location)))
  (test-assert "source locations turn raw text into located content"
    (content-text? located))
  (test-equal "located raw text records its position and source path"
    (list "computed text" #f location (list "computed.plant"))
    (list (content-text-value located)
          (content-text-source located)
          (content-source-location located)
          (content-source-paths located))))

(test-error "content text values are strings" #t
  (make-content-text 42))
(test-error "content text sources must be non-empty paths" #t
  (make-content-text "text" ""))
(test-error "content layout kinds are symbols" #t
  (make-content-layout "newline" "\n"))
(test-error "content layout raw values are strings" #t
  (make-content-layout 'newline 10))
(test-error "content layout sources must be non-empty paths" #t
  (make-content-layout 'newline "\n" ""))
(test-error "content text locations must be source-location alists" #t
  (make-content-text "text" #:location '(file "page.plant")))
(test-error "content source locations require string filenames" #t
  (content-with-source-location
    (make-content-text "text") '((file . 42))))
(test-error "content source locations reject empty filenames" #t
  (content-with-source-location
    (make-content-text "text") '((file . ""))))
(test-error "content source locations require positive line numbers" #t
  (content-with-source-location
    (make-content-text "text") '((file . "page.plant") (line . 0))))
(test-error "content source locations reject duplicate fields" #t
  (content-with-source-location
    (make-content-text "text") '((line . 1) (line . 2))))
(test-error "element kinds are symbols" #t
  (make-content-element "paragraph" '()))
(test-error "content element sources must be non-empty paths" #t
  (make-content-element 'paragraph '() ""))
(test-error "content elements reject non-content children" #t
  (make-content-element 'paragraph '(42)))
(test-error "content documents reject non-content children" #t
  (make-content-document '(42)))
(test-error "content document sources must be non-empty paths" #t
  (make-content-document '() ""))

(test-equal "content-source-paths gathers node and location provenance"
  '("document.plant" "element.plant" "element-location.plant"
    "text.plant" "text-location.plant" "layout.plant")
  (content-source-paths
    (make-content-document
      (list
        (content-with-source-location
          (make-content-element 'paragraph
            (list (make-content-text "body" "text.plant" #:location
                    '((file . "text-location.plant") (line . 2))))
            "element.plant")
          '((file . "element-location.plant") (line . 1)))
        (make-content-layout 'newline "\n" "layout.plant"))
      "document.plant")))

(test-equal "content-source-paths deduplicates repeated paths in order"
  '("page.plant")
  (content-source-paths
    (make-content-document
      (list (make-content-text "a" "page.plant")
            (make-content-element 'paragraph
              (list (make-content-text "b" "page.plant"))))
      "page.plant")))

(test-equal "plain-text projection preserves math structure"
  "x^(2)"
  (content->plain-text
    (make-content-element
      'math-sup
      (list (make-content-element 'math-row '("x"))
            (make-content-element 'math-row '("2"))))))
(test-error "plain-text projection rejects non-content values" #t
  (content->plain-text 42))
(test-error "content elements reject nested documents" #t
  (make-content-element 'paragraph
    (list (make-content-document '("nested")))))
(test-error "element property keys are symbols" #t
  (make-content-element 'paragraph '() #:properties '(("role" . "status"))))
(test-error "element property keys are unique" #t
  (make-content-element 'paragraph '()
    #:properties '((role . "status") (role . "note"))))
(test-error "source location cannot be stored as an element property" #t
  (make-content-element 'paragraph '()
    #:properties '((source-location . ((file . "page.plant"))))))
(test-error "element locations are validated at construction" #t
  (make-content-element 'paragraph '()
    #:location '((file . ""))))
(test-error "source location is not an ordinary element property" #t
  (content-with-property (make-content-element 'paragraph '())
    'source-location '((file . ""))))
(test-error "content-element-property requires an element" #t
  (content-element-property 'not-an-element 'role))
(test-error "content-element-property requires a symbol key" #t
  (content-element-property (make-content-element 'paragraph '()) "role"))
(test-error "source locations use the dedicated accessor" #t
  (content-element-property (make-content-element 'paragraph '())
    'source-location))
(test-error "element flow kinds are constrained" #t
  (make-content-element 'paragraph '() #:flow-kind 'floating))
(test-error "site construction rejects duplicate page IDs" #t
  (make-site
    (list (make-site-page "same" "First" (make-content-document '())
            "first.plant")
          (make-site-page "same" "Second" (make-content-document '())
            "second.plant"))))

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
  (test-error "HTML attributes reject markup delimiters in names" #t
    (make-html-attributes (list (cons (string->symbol "x\" onload=\"bad")
                                     "value"))))
  (test-error "HTML attributes reject spaces in names" #t
    (make-html-attributes (list (cons (string->symbol "bad name") "value"))))
  (test-error "HTML attributes reject empty names" #t
    (make-html-attributes (list (cons (string->symbol "") "value"))))
  (let* ((name (string-copy "notice"))
         (property-value (string-copy "callout"))
         (attributes (list (cons 'data-kind property-value)))
         (tag-name (string-copy "aside"))
         (style-properties
          (list (make-html-attributes attributes) (make-html-tag tag-name)))
         (style (make-content-style name style-properties)))
    (string-set! name 0 #\x)
    (string-set! property-value 0 #\x)
    (string-set! tag-name 0 #\x)
    (set-car! attributes (cons 'changed "changed"))
    (set-car! style-properties 'changed)
    (test-equal "content styles snapshot names, properties, and nested values"
      '("notice" ((data-kind . "callout")) "aside")
      (list (content-style-name style)
            (html-attributes-assoc (car (content-style-properties style)))
            (html-tag-name (cadr (content-style-properties style)))))
    (let ((exposed-name (content-style-name style))
          (exposed-properties (content-style-properties style))
          (exposed-attributes
           (html-attributes-assoc (car (content-style-properties style))))
          (exposed-tag (html-tag-name (cadr (content-style-properties style)))))
      (string-set! exposed-name 0 #\x)
      (set-car! exposed-properties 'changed)
      (set-cdr! (car exposed-attributes) "changed")
      (string-set! exposed-tag 0 #\x))
    (test-equal "content style and HTML value accessors return snapshots"
      '("notice" ((data-kind . "callout")) "aside")
      (list (content-style-name style)
            (html-attributes-assoc (car (content-style-properties style)))
            (html-tag-name (cadr (content-style-properties style))))))
  (test-equal "HTML supplies a generic fallback for open element kinds"
    '(article (span (@ (class "notice")) "Hello"))
    (content->sxml document))
  (test-equal "renderers can add per-kind behavior without changing the IR"
    "notice: Hello"
    (content->text document
      #:rules
      (make-render-rules
        (list
          (cons 'notice
            (lambda (_context element body _next)
              (string-append
                (symbol->string (content-element-kind element)) ": " body))))))))

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
       (p "Run " (code (@ (translate "no")) "x + 1") ".")
       (pre (@ (translate "no"))
            (code (@ (translate "no")) "(display \"hi\")")))
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
    (make-element #:tag "1bad"))
  (test-error "invalid flow kinds fail at definition" #t
    (make-element #:tag "aside" #:flow-kind 'column))
  (test-error "void? must be boolean at definition" #t
    (make-element #:tag "wbr" #:void? 'yes))
  (test-error "invalid anchor IDs fail at definition" #t
    (make-element #:tag "dfn" #:anchor "term id")))

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
    (merge-content-style base 'not-a-style))
  (test-error "a single invalid style is rejected" #t
    (merge-content-style 'not-a-style)))

(let ((empty (merge-content-style)))
  (test-equal "no merged styles produces the identity value"
    '(#f ())
    (list (content-style-name empty)
          (content-style-properties empty))))

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
(test-equal "make-element passes its kind through"
  'sidenote
  (content-element-kind
    ((make-element #:tag "aside" #:kind 'sidenote) "body")))

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
  (test-assert "the anchored element renders on its page"
    (string-contains
      (assoc-ref (site->html forest
                             #:options
                             (make-html-options
                               #:routes (make-html-routes
                                          #:home-page "glossary")))
                 "glossary")
      "<dfn id=\"term-id\">a term</dfn>")))

(test-end "grove content")
