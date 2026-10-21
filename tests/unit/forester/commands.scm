;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester commands)
  #:use-module (srfi srfi-64)
  #:use-module (forester commands)
  #:use-module (forester input scribble)
  #:use-module (forester site)
  #:use-module (grove site)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove render html)
  #:use-module (grove render text)
  #:use-module (grove style html)
  #:use-module (srfi srfi-13)
  #:use-module (tests unit forester fixtures))

(define (label-text element)
  (content-verbatim-string (content-element-children element)))

(test-begin "forester commands")

(let ((element (link "destination")))
  (test-equal "unlabeled links use their target as the label"
    "destination"
    (label-text element))
  (test-equal "links retain their target property"
    "destination"
    (content-element-property element 'target)))

(let ((element (link "destination" "read " "this")))
  (test-equal "links preserve explicitly supplied labels"
    "read this"
    (label-text element))
  (test-equal "links preserve their target with an explicit label"
    "destination"
    (content-element-property element 'target)))

(let ((element (link "destination#section")))
  (test-equal "a fragment link keeps only the page in its target"
    "destination"
    (content-element-property element 'target))
  (test-equal "a fragment link carries the fragment"
    "section"
    (content-element-property element 'fragment)))

(test-equal "links without a fragment have none"
  #f (content-element-property (link "destination") 'fragment))

(test-error "links reject an empty tree before #" #t (link "#section"))
(test-error "links reject an empty fragment after #" #t (link "destination#"))
(test-error "links reject a fragment with whitespace" #t (link "destination#sec tion"))
(test-error "links reject a fragment with a second #" #t (link "destination#a#b"))

(for-each
  (lambda (constructor kind)
    (let ((element (constructor "topic")))
      (test-equal (format #f "unlabeled ~a uses its ID as the label" kind)
        "topic"
        (label-text element))
      (test-equal (format #f "~a retains its ID property" kind)
        "topic"
        (content-element-property element 'id))
      (test-equal (format #f "~a is block-level" kind)
        'block
        (content-element-flow-kind element)))
    (let ((element (constructor "MiXeD-Topic")))
      (test-equal (format #f "~a normalizes its single value" kind)
        "mixed-topic" (content-element-property element 'id))
      (test-equal (format #f "~a derives its text from the canonical value" kind)
        "mixed-topic" (label-text element)))
    (test-error (format #f "~a rejects a separate label" kind) #t
      (constructor "topic" "label")))
  (list tag taxon)
  '(tag taxon))

(for-each
  (lambda (invalid-id)
    (test-error (format #f "topic IDs reject ~s" invalid-id) #t
      (tag invalid-id)))
  '("" "-leading" "trailing-" "not valid"))

(test-error "differently cased tags are duplicate metadata" #t
  (document->tree-page "page"
    (evaluate-document "@title{Page}@tag[\"Logic\"]@tag[\"logic\"]")
    "page.plant"))

(let ((element (image "diagram.svg" "A " "diagram")))
  (test-equal "images retain their source property"
    "diagram.svg"
    (content-element-property element 'source))
  (test-equal "images preserve alternative text"
    "A diagram"
    (label-text element)))

(test-error "images reject an empty source" #t
  (image ""))

(let* ((document (evaluate-document
                  "@paragraph[#:style 'notice]{Read @bold[#:style \"emphasis\"]{this}.}@code-block[#:style 'source]{(display \"hello\")}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "styled at-exp commands keep body text and apply classes"
    (and (string-contains html "<p class=\"notice\">Read ")
         (string-contains html "<strong class=\"emphasis\">this</strong>")
         (string-contains html "<pre translate=\"no\" class=\"source\"><code translate=\"no\">")))
  (test-equal "text output ignores HTML presentation"
    "Read this.\n```\n(display \"hello\")\n```\n" (content->text document)))

(let* ((location '((file . "styled.plant") (line . 4) (column . 2)))
       (original (content-with-source-location
                   (make-content-element 'paragraph '("Source") "styled.plant"
                     #:flow-kind 'block) location))
       (styled (content-with-style original 'notice)))
  (test-equal "styling preserves source location and flow semantics"
    (list "styled.plant" location 'block)
    (list (content-element-source styled)
          (content-element-property styled 'source-location)
          (content-element-flow-kind styled))))

(let ((element (link "destination" #:style 'reference)))
  (test-equal "style keywords do not become link labels"
    "destination" (label-text element))
  (test-equal "named style is normalized into the existing style IR"
    'reference (content-style-name (content-element-style element))))

(test-error "commands reject unknown keyword options" #t
  (evaluate-document "@paragraph[#:class \"notice\"]{Text}"))
(test-error "commands reject invalid styles" #t
  (evaluate-document "@paragraph[#:style 42]{Text}"))
(test-error "commands reject missing keyword values" #t
  (evaluate-document "@paragraph[#:style]"))

(let* ((document (evaluate-document
                  "@notation:display-math[#:style 'formula]{@notation:frac[#:style 'ratio]{x}{y}+@notation:sup[#:style 'power]{z}{2}}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "multi-argument notation commands accept style before brace arguments"
    (and (string-contains html "class=\"math-display formula\"")
         (string-contains html "<mfrac class=\"ratio\">")
         (string-contains html "<msup class=\"power\">"))))

(let* ((document (evaluate-document "@tex-display-math[#:id \"eq-2-12\"]{a^2+b^2=c^2}"))
       (math (car (content-document-children document)))
       (html (sxml->html (content->sxml document))))
  (test-equal "tex display math carries its id"
    "eq-2-12" (content-element-property math 'id))
  (test-assert "tex display math renders the id on its container"
    (string-contains html "class=\"math-display\" id=\"eq-2-12\"")))

(let* ((document (evaluate-document "@tex-math[#:id \"eq-inline\"]{x}"))
       (html (sxml->html (content->sxml document))))
  (test-assert "tex inline math renders the id on its container"
    (string-contains html "class=\"math-inline\" id=\"eq-inline\"")))

(test-error "tex math rejects an id with whitespace" #t
  (evaluate-document "@tex-math[#:id \"a b\"]{x}"))
(test-error "tex math rejects an id with #" #t
  (evaluate-document "@tex-math[#:id \"a#b\"]{x}"))

(test-equal "tex math without an id has no id property"
  #f (content-element-property
       (car (content-document-children
              (evaluate-document "@tex-display-math{x}")))
       'id))

(let* ((document (evaluate-document "@notation:math[#:id \"m-1\"]{x}"))
       (math (car (content-element-children
                    (car (content-document-children document)))))
       (html (sxml->html (content->sxml document))))
  (test-equal "notation math carries its id" "m-1" (content-element-property math 'id))
  (test-assert "notation math renders the id on its container"
    (string-contains html "class=\"math-inline\" id=\"m-1\"")))

(test-error "a style cannot replace a renderer-owned math id" #t
  (let* ((document (evaluate-document "@tex-display-math[#:id \"eq-1\"]{x}"))
         (math (car (content-document-children document)))
         (style (make-content-style 'tagged
                  (list (make-html-attributes '((id . "from-style"))))))
         (styled (content-with-style math style)))
    (sxml->html (content->sxml (make-content-document (list styled))))))


(let* ((document (evaluate-document
                   "@title{Root}@section{@title{Group}@paragraph{Body.}}"))
       (html (sxml->html (content->sxml document)))
       (page (make-site-page "root" "Root" document))
       (forest (make-forest (make-site (list page)))))
  (test-assert "sections render as structural headings"
    (string-contains
      html
      "<section class=\"forest-section\"><h2>Group</h2><p>Body.</p></section>"))
  (test-assert "sections do not create tree pages"
    (= 1 (length (pages forest)))))
  (test-equal "sections keep their title in text output"
    "Group\nBody.\n"
    (content->text
      (evaluate-document "@section{@title{Group}@paragraph{Body.}}")))
(test-end "forester commands")
