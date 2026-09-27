;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester commands)
  #:use-module (grove ir)
  #:export (title link summary published updated))

(define (title . body)
  (make-content-element 'title body))

(define (summary . body)
  (make-content-element 'summary body))

(define (published date)
  (make-content-element 'published (list date)))

(define (updated date)
  (make-content-element 'updated (list date)))

(define* (link target . label)
  (make-content-element 'link
                        (if (null? label) (list target) label)
                        #:properties `((target . ,target))))
