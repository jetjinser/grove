;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester commands)
  #:use-module (srfi srfi-64)
  #:use-module (forester commands)
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
    (let ((element (constructor "topic" "display " "name")))
      (test-equal (format #f "~a preserves explicitly supplied labels" kind)
        "display name"
        (label-text element))))
  (list tag taxon)
  '(tag taxon))

(for-each
  (lambda (invalid-id)
    (test-error (format #f "topic IDs reject ~s" invalid-id) #t
      (tag invalid-id)))
  '("" "-leading" "trailing-" "not valid"))

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
         (string-contains html "<pre class=\"source\"><code>")))
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

(test-end "forester commands")
