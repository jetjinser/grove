;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (language scribble content)
  #:export (bold verbatim paragraph link math display-math))

(define (bold . body)
  (make-content-element 'strong body))

(define (verbatim . body)
  (make-content-element
    'pre
    (list (make-content-text (content-verbatim-string body)))))

(define (paragraph . body)
  (make-content-element 'p body))

(define* (link target . label)
  (make-content-link target (if (null? label) (list target) label)))

(define (math source-text)
  (make-content-math source-text #f))

(define (display-math source-text)
  (make-content-math source-text #t))
