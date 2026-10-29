;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private html-tag)
  #:use-module (srfi srfi-1)
  #:export (valid-html-tag-name?))

(define (valid-html-tag-name? name)
  (and (string? name) (> (string-length name) 0)
       (char-alphabetic? (string-ref name 0))
       (every (lambda (character)
                (or (char-alphabetic? character) (char-numeric? character)
                    (memv character '(#\- #\_ #\:))))
              (string->list (substring name 1)))))
