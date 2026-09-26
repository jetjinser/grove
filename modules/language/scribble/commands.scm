;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (language scribble content)
  #:export (bold verbatim))

(define (bold . body)
  (make-content-element 'strong body))

(define (verbatim . body)
  (make-content-element
    'pre
    (list (make-content-text (content-verbatim-string body)))))
