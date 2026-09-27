;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir)
  #:export (bold verbatim paragraph heading blockquote
            itemize item code code-block math display-math))

(define (bold . body)
  (make-content-element 'bold body))

(define (verbatim . body)
  (make-content-element
    'verbatim
    (list (make-content-text (content-verbatim-string body)))))

(define (paragraph . body)
  (make-content-element 'paragraph body))

(define (heading . body)
  (make-content-element 'heading body))

(define (blockquote . body)
  (make-content-element 'blockquote body))

(define (itemize . body)
  (make-content-element 'itemize body))

(define (item . body)
  (make-content-element 'item body))

(define (code . body)
  (make-content-element 'code body))

(define (code-block . body)
  (make-content-element 'code-block body))

(define (math source-text)
  (make-content-element 'math (list source-text)))

(define (display-math source-text)
  (make-content-element 'math (list source-text)
                        #:properties '((display? . #t))))
