;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render text)
  #:use-module (grove ir)
  #:use-module (srfi srfi-13)
  #:export (content->text))

(define* (content->text content #:key (handlers '()))
  (define (render content)
    (cond
      ((string? content) content)
      ((content-text? content) (content-text-value content))
      ((content-layout? content) (content-layout-raw content))
      ((content-element? content) (render-element content))
      ((content-document? content)
       (string-concatenate
         (map render (content-document-children content))))
      (else (error "not a content node" content))))

  (define (render-element element)
    (let* ((kind (content-element-kind element))
           (body (string-concatenate
                   (map render (content-element-children element))))
           (handler (assq-ref handlers kind)))
      (if handler
          (handler element body)
          (case kind
            ((paragraph) (string-append body "\n"))
            ((math)
             (let ((source-text
                     (content-verbatim-string
                       (content-element-children element))))
               (if (content-element-property element 'display?)
                   (string-append "$$\n" source-text "\n$$\n")
                   (string-append "$" source-text "$"))))
            (else body)))))

  (render content))
