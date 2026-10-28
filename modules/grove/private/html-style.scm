;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private html-style)
  #:use-module (grove style)
  #:use-module (grove style html element)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (content-style-html-tag-name
            content-style-html-void?
            make-html-void
            html-void?))

(define-record-type <html-void>
  (%make-html-void)
  html-void?)

(define (make-html-void)
  (%make-html-void))

(define (content-style-html-tag-name style)
  "Return the rightmost HTML tag marker in STYLE, or #f."
  (and style
    (let loop ((properties (content-style-properties style)) (tag #f))
      (if (null? properties)
          tag
          (loop (cdr properties)
            (if (html-tag? (car properties))
                (html-tag-name (car properties))
                tag))))))

(define (content-style-html-void? style)
  "Return whether STYLE contains an HTML void marker."
  (and style (any html-void? (content-style-properties style))))
