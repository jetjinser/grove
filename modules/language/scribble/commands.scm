;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir))

(define-public (bold . body)
  (make-content-element 'bold body))

(define-public (verbatim . body)
  (make-content-element
    'verbatim
    (list (make-content-text (content-verbatim-string body)))))

(define-public (paragraph . body)
  (make-content-element 'paragraph body))

(define-public (heading . body)
  (make-content-element 'heading body))

(define-public (blockquote . body)
  (make-content-element 'blockquote body))

(define-public (itemize . body)
  (make-content-element 'itemize body))

(define-public (item . body)
  (make-content-element 'item body))

(define-public (code . body)
  (make-content-element 'code body))

(define-public (code-block . body)
  (make-content-element 'code-block body))

(define-public (math source-text)
  (make-content-element 'math (list source-text)))

(define-public (display-math source-text)
  (make-content-element 'math (list source-text)
                        #:properties '((display? . #t))))
