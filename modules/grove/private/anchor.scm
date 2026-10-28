;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private anchor)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-13)
  #:export (checked-anchor-id))

(define (checked-anchor-id id)
  "Validate an optional anchor ID: #f, or a non-empty string with no whitespace or #."
  (if (not id)
      #f
      (begin
        (unless (and (non-empty-string? id)
                     (not (string-any char-whitespace? id))
                     (not (string-index id #\#)))
          (error "anchor ID must be a non-empty string without whitespace or #"
                 id))
        id)))
