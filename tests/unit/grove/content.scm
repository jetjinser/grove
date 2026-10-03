;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove content)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove render html)
  #:use-module (grove render text))

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
       (h2 "Details")
       (blockquote "A short quote.")
       (ul (li "First") (li "Second"))
       (p "Run " (code "x + 1") ".")
       (pre (code "(display \"hi\")")))
    (content->sxml document))
  (test-equal "plain text keeps structure, bullets, and code readable"
    (string-append
      "Details\n=======\n"
      "> A short quote.\n"
      "- First\n- Second\n"
      "Run `x + 1`.\n"
      "```\n(display \"hi\")\n```\n")
    text))

(test-end "grove content")
