;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove content)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render text))

(test-begin "grove content")

(let* ((notice
         (make-content-element 'notice '("Hello")
           #:properties '((role . "status"))))
       (document (make-content-document (list notice))))
  (test-assert "arbitrary element kinds stay in the generic IR"
    (and (content-element? notice)
         (eq? (content-element-kind notice) 'notice)
         (equal? (content-element-property notice 'role) "status")))
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

(let* ((document
         (make-content-document
           (list (make-content-element 'heading '("Details"))
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
