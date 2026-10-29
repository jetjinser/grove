;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private ir plain-text)
  #:use-module (grove ir)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (element->plain-text
            content->plain-text/internal))

(define (element->plain-text kind properties parts)
  (case kind
    ((math-token)
     (let ((text (string-concatenate parts)))
       (if (and (eq? (assq-ref properties 'token-kind) 'mo)
                (member text '("+" "=" "×" "·" "→"
                               "≈" "<" ">" "≤" "≥")))
           (format #f " ~a " text)
           text)))
    ((math-frac)
     (format #f "(~a)/(~a)" (or (first parts) "")
             (or (second parts) "")))
    ((math-sup) (format #f "~a^(~a)" (or (first parts) "")
                        (or (second parts) "")))
    ((math-sub) (format #f "~a_(~a)" (or (first parts) "")
                        (or (second parts) "")))
    ((math-parens) (string-append "(" (string-concatenate parts) ")"))
    ((math-vector) (format #f "vec(~a)" (string-concatenate parts)))
    ((math-column) (string-join parts ", "))
    ((math-align) (string-join parts " ; "))
    (else (string-concatenate parts))))

(define (content->plain-text/internal content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content)
     (element->plain-text
       (content-element-kind content)
       (content-element-properties content)
       (map content->plain-text/internal (content-element-children content))))
    ((content-document? content)
     (string-concatenate
       (map content->plain-text/internal (content-document-children content))))
    (else (error "plain-text projection requires a content value" content))))
