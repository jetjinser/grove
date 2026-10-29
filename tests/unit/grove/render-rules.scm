;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove render-rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render rules)
  #:use-module (grove render html presentation)
  #:use-module (grove render context)
  #:use-module (grove output)
  #:use-module (grove style html)
  #:use-module (grove content transform)
  #:use-module (grove render html image)
  #:use-module (grove render text))

(test-begin "grove renderer rules")

(define (image-element)
  (make-content-element 'image '("A forest")
    #:properties '((source . "forest.jpg"))))

(define (img-rule context element payload next)
  `(img (@ (src ,(content-element-property element 'source))
           (alt ,(content-verbatim-string payload)))))

(define (picture-rule context element payload next)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string payload)))
    (if (string=? source "forest.jpg")
        `(picture
           (source (@ (type "image/avif")
                      (srcset ,(string-append source ".avif"))))
           (img (@ (src ,source) (alt ,alt))))
        (next))))

(define (render-with rules)
  (content->sxml (image-element) #:rules rules))

(define base
  (make-render-rules (list (cons 'image img-rule))))

(define overlay
  (make-render-rules (list (cons 'image picture-rule))))

(let* ((entry (cons 'image img-rule))
       (rules (make-render-rules (list entry))))
  (set-cdr! entry picture-rule)
  (test-eq "rule sets snapshot their input association pairs"
    img-rule (render-rules-ref rules 'image)))

(test-equal "empty is the identity on the left"
  (render-with base)
  (render-with (compose-render-rules empty-render-rules base)))

(test-equal "empty is the identity on the right"
  (render-with base)
  (render-with (compose-render-rules base empty-render-rules)))

(test-equal "render rule composition accepts any number of layers"
  (render-with (compose-render-rules base overlay))
  (render-with (compose-render-rules base empty-render-rules overlay)))

(test-equal "zero render rule layers produce the empty rule set"
  #f
  (render-rules-ref (compose-render-rules) 'image))

(test-error "render rule lookup rejects non-symbol kinds" #t
  (render-rules-ref base "image"))

(test-eq "an explicitly constructed empty rule set is canonical"
  empty-render-rules
  (make-render-rules '()))

(test-eq "composing empty rule sets preserves the canonical empty value"
  empty-render-rules
  (compose-render-rules empty-render-rules empty-render-rules))

(test-assert "overlay wins"
  (pair? (memq 'picture (render-with (compose-render-rules base overlay)))))

(test-assert "composition is associative across overlapping delegating rules"
  (let ((events '()))
    (define (layer name)
      (make-render-rules
        (list (cons 'image
                    (lambda (_context _element _payload next)
                      (set! events (cons name events))
                      (next))))))
    (define (observe rules)
      (set! events '())
      (let ((output (render-with rules)))
        (list output (reverse events))))
    (let* ((a (layer 'a))
           (b (layer 'b))
           (c (layer 'c)))
      (equal? (observe (compose-render-rules a (compose-render-rules b c)))
              (observe (compose-render-rules (compose-render-rules a b) c))))))

(test-assert "reapplying this terminal override preserves this output"
  (let* ((rules (compose-render-rules base overlay))
         (again (compose-render-rules rules overlay)))
    (equal? (render-with rules) (render-with again))))

(test-assert "an overlay can delegate to the base rule"
  (let ((sxml (content->sxml
                (make-content-element 'image '("A missing forest")
                  #:properties '((source . "missing.jpg")))
                #:rules (compose-render-rules base overlay))))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(let ((contextual
        (make-render-rules
          (list (cons 'image
                      (lambda (context element _payload next)
                        `(p ,(render-context-ref context 'page))))))))
  (test-assert "rules receive an explicit context"
    (let ((sxml (content->sxml
                  (image-element)
                  #:rules contextual
                  #:context (make-render-context #:page "ctx"))))
      (and (pair? (memq 'p sxml))
           (string=? "ctx" (cadr (memq 'p sxml)))))))

(let ((rules
        (make-render-rules
          (list (cons 'image
                      (lambda (context _element _payload _next)
                        (if (and (render-context? context)
                                 (not (render-context-has? context 'page)))
                            '(empty-context)
                            '(unexpected-context))))))))
  (test-equal "HTML rules receive an empty typed context by default"
    '(empty-context)
    (content->sxml (image-element) #:rules rules)))

(let ((rules
        (make-render-rules
          (list (cons 'paragraph
                      (lambda (context _element _payload _next)
                        (if (and (render-context? context)
                                 (not (render-context-has? context 'page)))
                            "empty-context"
                            "unexpected-context")))))))
  (test-equal "text rules receive an empty typed context by default"
    "empty-context"
    (content->text (make-content-element 'paragraph '("body"))
      #:rules rules)))

(test-error "HTML rendering rejects a non-context value immediately" #t
  (content->sxml (image-element) #:context #f))
(test-error "text rendering rejects a non-context value immediately" #t
  (content->text "body" #:context #f))

(test-error "text rendering validates rules for an empty document" #t
  (content->text (make-content-document '()) #:rules '()))

(test-error "HTML rendering validates rules for an empty document" #t
  (content->sxml (make-content-document '()) #:rules '()))

(test-error "HTML rendering validates its ID prefix for an empty document" #t
  (content->sxml (make-content-document '()) #:id-prefix #f))

(test-assert "content->sxml accepts a rule set directly"
  (let ((sxml (render-with (compose-render-rules base overlay))))
    (and (pair? (memq 'picture sxml))
         (any (lambda (node)
                (and (pair? node) (eq? (car node) 'source)))
              sxml))))

(test-assert "the default rule still renders when no overlay matches"
  (let ((sxml (render-with base)))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(let* ((math (make-content-element 'math '("x")
                                   #:properties '((tex? . #t))))
       (document (make-content-document (list math)))
       (rules (make-render-rules
                (list (cons 'math
                            (lambda (_context _element _payload _next)
                              '(custom-math "custom")))))))
  (test-equal "HTML rules can replace math rendering"
    '(article (custom-math "custom"))
    (content->sxml document #:rules rules))
  (test-equal "HTML math rules can delegate to the built-in renderer"
    (content->sxml document)
    (content->sxml document
      #:rules
      (make-render-rules
        (list (cons 'math
                    (lambda (_context _element _payload next)
                      (next))))))))

(let* ((math (make-content-element 'math '("x")
                                   #:properties '((tex? . #t))))
       (document (make-content-document (list math)))
       (rules (make-render-rules
                (list (cons 'math
                            (lambda (_context _element payload _next)
                              (string-append "math:" payload)))))))
  (test-equal "text math rules receive the complete backend payload"
    "math:$x$"
    (content->text document #:rules rules)))

(test-error "duplicate kinds are rejected" #t
  (make-render-rules (list (cons 'image img-rule)
                         (cons 'image picture-rule))))
(test-error "rules reject interpreters that cannot accept four arguments" #t
  (make-render-rules (list (cons 'image (lambda (_context) #f)))))
(test-assert "rules accept variadic interpreters that can receive four arguments"
  (render-rules?
    (make-render-rules
      (list (cons 'image (lambda (_context _element _payload . _next) #f))))))

(let* ((style-a (make-html-style
                 #:stylesheets (list (make-artifact "base.css" "base"))))
       (style-b (make-html-style
                 #:stylesheets (list (make-artifact "site.css" "site"))))
       (style-c (make-html-style
                  #:stylesheets (list (make-artifact "extra.css" "extra"))))
       (a (make-html-presentation #:style style-a #:rules base))
       (b (make-html-presentation #:style style-b #:rules overlay))
       (c (make-html-presentation #:style style-c))
       (left (compose-html-presentations a b c))
       (right (compose-html-presentations a
                (compose-html-presentations b c))))
  (test-assert "presentation composition is associative"
    (equal? (render-with (html-presentation-rules left))
            (render-with (html-presentation-rules right))))
  (test-equal "presentation composition accumulates style artifacts in order"
    '("base.css" "site.css" "extra.css")
    (map artifact-path
         (html-style-artifacts (html-presentation-style left))))
  (test-equal "style composition is associative"
    (map artifact-path
         (html-style-artifacts (html-presentation-style left)))
    (map artifact-path
         (html-style-artifacts (html-presentation-style right))))
  (test-eq "zero presentation layers produce the empty presentation"
    empty-html-presentation
    (compose-html-presentations))
  (test-equal "one presentation layer preserves that presentation's head"
    (html-presentation-head a)
    (html-presentation-head (compose-html-presentations a)))
  (test-assert "presentation composition keeps the overlay rules"
    (pair? (memq 'picture
                 (render-with
                   (html-presentation-rules
                     (compose-html-presentations a b)))))))

(let* ((first (make-html-presentation
                #:head '((meta (@ (name "first"))))
                #:site-nav '((span "first"))
                #:title-decorations '(("p" . ((span "a"))))
                #:metadata-items '(("p" . ((li "a"))))))
       (second (make-html-presentation
                 #:head '((meta (@ (name "second"))))
                 #:site-nav '((span "second"))
                 #:title-decorations '(("p" . ((span "b")))
                                       ("q" . ((span "q"))))
                 #:metadata-items '(("p" . ((li "b"))))))
       (home-a (make-html-home-link '("A")))
       (home-b (make-html-home-link '("B")))
       (a first)
       (b second)
       (left (compose-html-presentations
               a (compose-html-presentations b empty-html-presentation)))
       (right (compose-html-presentations
                (compose-html-presentations a b) empty-html-presentation)))
  (test-equal "presentation appends head and navigation nodes"
    '((meta (@ (name "first"))) (meta (@ (name "second"))))
    (html-presentation-head left))
  (test-equal "presentation merges page fragments in declaration order"
    '((span "a") (span "b"))
    (html-presentation-title-decoration left "p"))
  (test-equal "presentation appends page metadata by page ID"
    '((li "a") (li "b"))
    (html-presentation-metadata-items left "p"))
  (test-equal "presentation composition is associative"
    (list (html-presentation-head left)
          (html-presentation-site-nav left)
          (html-presentation-title-decorations left)
          (html-presentation-metadata-item-map left))
    (list (html-presentation-head right)
          (html-presentation-site-nav right)
          (html-presentation-title-decorations right)
          (html-presentation-metadata-item-map right)))
  (test-equal "the rightmost declared home link wins"
    home-b
    (html-presentation-home-link
      (compose-html-presentations
        (make-html-presentation #:home-link home-a)
        (make-html-presentation #:home-link home-b))))
  (test-equal "an absent page has no presentation fragments"
    '()
    (html-presentation-title-decoration left "absent")))

(let* ((head '((meta (@ (name "author")))))
       (presentation (make-html-presentation #:head head)))
  (set-car! head '(script "changed"))
  (test-equal "presentations snapshot caller-owned SXML lists"
    '((meta (@ (name "author"))))
    (html-presentation-head presentation)))

(let* ((nav (make-html-nav-link "home" "Home"
               #:attributes '((title . "Home page"))))
       (home (make-html-home-link '("Home")
                #:attributes '((title . "Home page"))))
       (presentation
        (make-html-presentation
          #:head '((title "Home"))
          #:home-link home
          #:site-nav (list nav)
          #:title-decorations '(("home" . ((span "Home"))))
          #:metadata-items '(("home" . ((li "Home"))))))
       (head (html-presentation-head presentation))
       (site-nav (html-presentation-site-nav presentation))
       (decorations (html-presentation-title-decorations presentation))
       (metadata (html-presentation-metadata-item-map presentation))
       (decoration (html-presentation-title-decoration presentation "home"))
       (metadata-items (html-presentation-metadata-items presentation "home"))
       (home-content (html-home-link-content home))
       (home-attributes (html-home-link-attributes home))
       (nav-attributes (html-nav-link-attributes nav)))
  (set-car! head '(script "changed"))
  (set-car! site-nav 'changed)
  (set-cdr! (car decorations) 'changed)
  (set-cdr! (car metadata) 'changed)
  (set-car! decoration '(script "changed"))
  (set-car! metadata-items '(script "changed"))
  (string-set! (car home-content) 0 #\x)
  (set-cdr! (car home-attributes) "changed")
  (set-cdr! (car nav-attributes) "changed")
  (let ((page-id (html-nav-link-page-id nav))
        (label (html-nav-link-label nav)))
    (string-set! page-id 0 #\x)
    (string-set! label 0 #\x))
  (test-equal "presentation accessors return independent snapshots"
    '((title "Home"))
    (html-presentation-head presentation))
  (test-equal "presentation navigation accessors protect their list"
    (list nav) (html-presentation-site-nav presentation))
  (test-equal "presentation map accessors protect their pair structure"
    '(("home" . ((span "Home"))))
    (html-presentation-title-decorations presentation))
  (test-equal "presentation fragment accessors return independent snapshots"
    '((span "Home"))
    (html-presentation-title-decoration presentation "home"))
  (test-equal "presentation metadata fragment accessors return snapshots"
    '((li "Home"))
    (html-presentation-metadata-items presentation "home"))
  (test-equal "home-link accessors return independent snapshots"
    '("Home") (html-home-link-content home))
  (test-equal "home-link attribute accessors return independent snapshots"
    '((title . "Home page")) (html-home-link-attributes home))
  (test-equal "navigation accessors return independent string snapshots"
    '("home" "Home" ((title . "Home page")))
    (list (html-nav-link-page-id nav)
          (html-nav-link-label nav)
          (html-nav-link-attributes nav))))

(test-error "presentation rejects procedure-valued SXML"
  #t
  (make-html-presentation #:head (list (lambda () '((meta))))))

(test-error "presentation rejects duplicate SXML attributes" #t
  (make-html-presentation
    #:head '((meta (@ (name "viewport") (name "description"))))))

(test-error "presentation rejects malformed dotted SXML nodes" #t
  (make-html-presentation #:head (list (cons 'meta "not a child list"))))

(test-error "presentation rejects invalid SXML tag names" #t
  (make-html-presentation
    #:head (list (list (string->symbol "section><script") "x"))))

(test-error "presentation rejects invalid SXML attribute names" #t
  (make-html-presentation
    #:head
    (list (list 'meta
                (cons '@
                      (list (list (string->symbol "name\" onload") "x")))))))

(test-error "presentation rejects duplicate page IDs" #t
  (make-html-presentation
    #:metadata-items '(("page" . ((li "first")))
                       ("page" . ((li "second"))))))

(test-error "presentation rejects empty title-decoration page IDs" #t
  (make-html-presentation #:title-decorations '(("" . ((span "unused"))))))

(test-error "presentation rejects empty metadata page IDs" #t
  (make-html-presentation #:metadata-items '(("" . ((li "unused"))))))

(test-error "home-link attributes cannot repeat names" #t
  (make-html-home-link '("Home")
    #:attributes '((class . "brand") (class . "logo"))))

(test-error "navigation attributes cannot repeat names" #t
  (make-html-nav-link "home" "Home"
    #:attributes '((class . "first") (class . "second"))))

(test-error "navigation links reject empty page IDs" #t
  (make-html-nav-link "" "Home"))

(test-error "HTML styles reject duplicate artifact paths at construction" #t
  (make-html-style
    #:stylesheets (list (make-artifact "site.css" "first"))
    #:assets (list (make-artifact "site.css" "second"))))

(test-error "composed HTML styles reject conflicting artifact paths" #t
  (compose-html-styles
    (make-html-style #:assets (list (make-artifact "assets" "file")))
    (make-html-style #:assets
      (list (make-artifact "assets/logo.svg" "image")))))

(let* ((content (string-copy "body { color: red; }"))
       (artifact (make-artifact "site.css" content))
       (stylesheets (list artifact))
       (style (make-html-style #:stylesheets stylesheets)))
  (set-car! stylesheets (make-artifact "other.css" "body {}"))
  (string-set! content 0 #\x)
  (test-equal "HTML styles snapshot caller-owned artifact lists and content"
    '("site.css" "body { color: red; }")
    (let ((copy (car (html-style-stylesheets style))))
      (list (artifact-path copy) (artifact-content copy))))
  (let ((exposed (html-style-stylesheets style)))
    (set-car! exposed (make-artifact "changed.css" "body {}")))
  (test-equal "HTML style accessors return independent artifact snapshots"
    "site.css"
    (artifact-path (car (html-style-stylesheets style)))))


(define (append-content-text value)
  (lambda (document)
    (make-content-document
      (append (content-document-children document)
              (list (make-content-text value))))))

(let* ((add-a (append-content-text "a"))
       (add-b (append-content-text "b"))
       (add-c (append-content-text "c"))
       (document (make-content-document '()))
       (composed (compose-content-transforms add-a add-b)))
  (test-error "content transforms reject procedures that cannot accept a document" #t
    (compose-content-transforms (lambda () document)))
  (test-equal "identity is a valid content transform"
    0 (length (content-document-children (identity document))))
  (test-equal "empty transform composition is the identity"
    (content-document-children document)
    (content-document-children
      ((compose-content-transforms) document)))
  (test-equal "content transforms compose in order"
    '("a" "b")
    (map content-text-value
      (content-document-children (composed document))))
  (test-equal "content transform composition is associative"
    '("a" "b" "c")
    (map content-text-value (content-document-children
      ((compose-content-transforms add-a
                                   (compose-content-transforms add-b add-c))
       document))))
  (test-equal "left- and right-associated transforms agree"
    (content-document-children
      ((compose-content-transforms add-a
         (compose-content-transforms add-b add-c))
       document))
    (content-document-children
      ((compose-content-transforms
         (compose-content-transforms add-a add-b)
         add-c)
       document))))

(test-error "content transforms must be procedures" #t
  (compose-content-transforms 'not-a-procedure))

(test-error "content transforms must return a content document" #t
  ((compose-content-transforms (lambda (document) 'not-a-document))
   (make-content-document '())))

(test-error "content transform composition validates each intermediate result" #t
  ((compose-content-transforms
     (lambda (document) 'not-a-document)
     (lambda (document) (make-content-document '())))
   (make-content-document '())))

(test-error "empty content transform composition still validates its input" #t
  ((compose-content-transforms) 'not-a-document))

(define image-sets
  (list
    (make-image-set "forest.jpg"
      #:variants
      (list (make-image-variant "image/avif" "/images/forest.avif")
            (make-image-variant "image/jxl" "/images/forest.jxl"))
      #:fallback "/images/forest.jpg")))

(test-assert "image variants snapshot constructor strings and accessors"
  (let* ((type (string-copy "image/avif"))
         (url (string-copy "/images/forest.avif"))
         (variant (make-image-variant type url)))
    (string-set! type 0 #\x)
    (string-set! url 0 #\x)
    (let ((returned-type (image-variant-type variant))
          (returned-url (image-variant-url variant)))
      (string-set! returned-type 0 #\x)
      (string-set! returned-url 0 #\x))
    (and (string=? (image-variant-type variant) "image/avif")
         (string=? (image-variant-url variant) "/images/forest.avif"))))

(test-assert "image sets snapshot source, variants, and fallback"
  (let* ((source (string-copy "forest.jpg"))
         (fallback (string-copy "/images/forest.jpg"))
         (variants (list (make-image-variant
                           (string-copy "image/avif")
                           (string-copy "/images/forest.avif"))))
         (image-set (make-image-set source
                       #:variants variants #:fallback fallback)))
    (string-set! source 0 #\x)
    (string-set! fallback 0 #\x)
    (set-car! variants (make-image-variant "image/webp" "/forest.webp"))
    (let ((returned-source (image-set-source image-set))
          (returned-fallback (image-set-fallback image-set))
          (returned-variants (image-set-variants image-set)))
      (string-set! returned-source 0 #\x)
      (string-set! returned-fallback 0 #\x)
      (set-car! returned-variants (make-image-variant "image/jpeg" "/x.jpg")))
    (and (string=? (image-set-source image-set) "forest.jpg")
         (string=? (image-set-fallback image-set) "/images/forest.jpg")
         (string=? (image-variant-type (car (image-set-variants image-set)))
                   "image/avif"))))

(test-assert "image-set constructors store source-specific variant values"
  (and (list? image-sets)
       (every image-set? image-sets)
       (string=? (image-set-source (car image-sets)) "forest.jpg")))
(test-error "image variants require a non-empty media type" #t
  (make-image-variant "" "/forest.avif"))
(test-error "image variants require a non-empty URL" #t
  (make-image-variant "image/avif" ""))
(test-error "image sets require a fallback URL" #t
  (make-image-set "forest.jpg"))
(test-error "image-set-rule requires a list of image-set values" #t
  (image-set-rule (list (cons "forest.jpg" (car image-sets)))))
(test-error "image-set-rule rejects duplicate image sources" #t
  (image-set-rule (list (car image-sets) (car image-sets))))

(let* ((rules (make-render-rules
                (list (cons 'image (image-set-rule image-sets)))))
       (sxml (render-with rules)))
  (test-assert "image-set-rule emits a picture"
    (pair? (memq 'picture sxml)))
  (test-assert "image-set-rule keeps every variant"
    (= 2 (length (filter (lambda (node)
                           (and (pair? node) (eq? (car node) 'source)))
                         sxml))))
  (test-assert "image-set-rule uses the fallback in img"
    (any (lambda (node)
           (and (pair? node) (eq? (car node) 'img)))
         sxml)))

(let ((rules (make-render-rules
               (list (cons 'image
                           (lambda (context element _payload next)
                             "PICTURE"))))))
  (test-equal "text renderer accepts renderer rules"
    "PICTURE"
    (content->text (image-element) #:rules rules)))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element _payload next)
                             (next)))))))
  (test-equal "next reaches the built-in HTML interpretation"
    '(p "Hello")
    (content->sxml (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(let* ((base (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element _payload next)
                             (next))))))
       (overlay (make-render-rules
                  (list (cons 'paragraph
                              (lambda (context element _payload next)
                                (next))))))
       (rules (compose-render-rules base overlay)))
  (test-equal "next reaches the base rule then the built-in interpretation"
    '(p "Hello")
    (content->sxml (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element _payload next)
                             (next)))))))
  (test-equal "next reaches the built-in text interpretation"
    "Hello\n"
    (content->text (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(test-error "public content->sxml does not accept stock renderer handlers" #t
  (content->sxml (image-element) #:internal-handlers '()))

(test-error "public content->text does not accept stock renderer handlers" #t
  (content->text (image-element) #:internal-lazy-handlers '()))

(test-end "grove renderer rules")
