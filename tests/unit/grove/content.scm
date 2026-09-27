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

(test-end "grove content")
