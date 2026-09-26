;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir)
  #:export (bold verbatim paragraph math display-math))

(define (bold . body)
  (make-content-element 'bold body))

(define (verbatim . body)
  (make-content-element
    'verbatim
    (list (make-content-text (content-verbatim-string body)))))

(define (paragraph . body)
  (make-content-element 'paragraph body))

(define (math source-text)
  (make-content-element 'math (list source-text)))

(define (display-math source-text)
  (make-content-element 'math (list source-text)
                        #:properties '((display? . #t))))
