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
  #:use-module (grove render rules)
  #:use-module (grove render text)
  #:use-module (forester commands))

(test-begin "grove html rules")

(define (image-element)
  (image "forest.jpg" "A forest"))

(define (img-rule context element children next)
  `(img (@ (src ,(content-element-property element 'source))
           (alt ,(content-verbatim-string children)))))

(define (picture-rule context element children next)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string children)))
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

(test-assert "overlay wins"
  (pair? (memq 'picture (render-with (compose-render-rules base overlay)))))

(test-assert "composition is associative"
  (let* ((a (make-render-rules (list (cons 'image img-rule))))
         (b (make-render-rules
              (list (cons 'link (lambda (context element children next)
                                  '(a))))))
         (c (make-render-rules (list (cons 'image picture-rule)))))
    (equal? (render-with (compose-render-rules a (compose-render-rules b c)))
            (render-with (compose-render-rules (compose-render-rules a b) c)))))

(test-assert "reapplying this terminal override preserves this output"
  (let* ((rules (compose-render-rules base overlay))
         (again (compose-render-rules rules overlay)))
    (equal? (render-with rules) (render-with again))))

(test-assert "an overlay can delegate to the base rule"
  (let ((sxml (content->sxml
                (image "missing.jpg" "A missing forest")
                #:rules (compose-render-rules base overlay))))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(let ((contextual
        (make-render-rules
          (list (cons 'image
                      (lambda (context element children next)
                        `(p ,(render-context-ref context 'page))))))))
  (test-assert "rules receive an explicit context"
    (let ((sxml (content->sxml
                  (image-element)
                  #:rules contextual
                  #:context (make-render-context #:page "ctx"))))
      (and (pair? (memq 'p sxml))
           (string=? "ctx" (cadr (memq 'p sxml)))))))

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

(define-render-rules dsl-rules
  (image (context element children next)
    (picture-rule context element children next)))

(test-assert "define-render-rules expands to a rule value"
  (let ((sxml (render-with dsl-rules)))
    (and (render-rules? dsl-rules)
         (pair? (memq 'picture sxml)))))

(test-error "duplicate kinds are rejected" #t
  (make-render-rules (list (cons 'image img-rule)
                         (cons 'image picture-rule))))

(let* ((style-a (make-html-style
                 #:stylesheets (list (make-artifact "base.css" "base"))))
       (style-b (make-html-style
                 #:stylesheets (list (make-artifact "site.css" "site"))))
       (a (make-html-presentation #:style style-a #:rules base))
       (b (make-html-presentation #:style style-b #:rules overlay))
       (left (compose-html-presentations
               a (compose-html-presentations b empty-html-presentation)))
       (right (compose-html-presentations
                (compose-html-presentations a b) empty-html-presentation)))
  (test-assert "presentation composition is associative"
    (equal? (render-with (html-presentation-rules left))
            (render-with (html-presentation-rules right))))
  (test-equal "presentation composition accumulates style artifacts in order"
    '("base.css" "site.css")
    (map artifact-path
         (html-style-artifacts (html-presentation-style left))))
  (test-equal "style composition is associative"
    (map artifact-path
         (html-style-artifacts (html-presentation-style left)))
    (map artifact-path
         (html-style-artifacts (html-presentation-style right))))
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

(test-error "presentation rejects duplicate page IDs" #t
  (make-html-presentation
    #:metadata-items '(("page" . ((li "first")))
                       ("page" . ((li "second"))))))

(test-error "home-link attributes cannot repeat names" #t
  (make-html-home-link '("Home")
    #:attributes '((class . "brand") (class . "logo"))))

(test-error "navigation attributes cannot repeat names" #t
  (make-html-nav-link "home" "Home"
    #:attributes '((class . "first") (class . "second"))))

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


(let* ((add-text
         (lambda (document)
           (make-content-document
             (append (content-document-children document)
                     (list (make-content-text "x"))))))
       (document (make-content-document '()))
       (composed (compose-content-transforms add-text add-text)))
  (test-equal "identity is a valid content transform"
    0 (length (content-document-children
                (transform-document identity document))))
  (test-equal "empty transform composition is the identity"
    (content-document-children document)
    (content-document-children
      (transform-document (compose-content-transforms) document)))
  (test-equal "content transforms compose in order"
    2 (length (content-document-children
                (transform-document composed document))))
  (test-equal "content transform composition is associative"
    (content-document-children
      (transform-document
        (compose-content-transforms add-text
                                    (compose-content-transforms add-text add-text))
        document))
    (content-document-children
      (transform-document
        (compose-content-transforms
          (compose-content-transforms add-text add-text)
          add-text)
        document))))

(test-error "content transforms must be procedures" #t
  (compose-content-transforms 'not-a-procedure))

(test-error "content transforms must return a content document" #t
  (transform-document (lambda (document) 'not-a-document)
                      (make-content-document '())))

(define-image-sets image-sets
  (image-set "forest.jpg"
    (variant "image/avif" "/images/forest.avif")
    (variant "image/jxl" "/images/forest.jxl")
    (fallback "/images/forest.jpg")))

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
                           (lambda (context element children next)
                             "PICTURE"))))))
  (test-equal "text renderer accepts renderer rules"
    "PICTURE"
    (content->text (image-element) #:rules rules)))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element children next)
                             (next)))))))
  (test-equal "next reaches the built-in HTML interpretation"
    '(p "Hello")
    (content->sxml (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(let* ((base (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element children next)
                             (next))))))
       (overlay (make-render-rules
                  (list (cons 'paragraph
                              (lambda (context element children next)
                                (next))))))
       (rules (compose-render-rules base overlay)))
  (test-equal "next reaches the base rule then the built-in interpretation"
    '(p "Hello")
    (content->sxml (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element children next)
                             (next)))))))
  (test-equal "next reaches the built-in text interpretation"
    "Hello\n"
    (content->text (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(test-error "public content->sxml does not accept stock renderer handlers" #t
  (content->sxml (image-element) #:internal-handlers '()))

(test-error "public content->text does not accept stock renderer handlers" #t
  (content->text (image-element) #:internal-lazy-handlers '()))

(test-end "grove html rules")
