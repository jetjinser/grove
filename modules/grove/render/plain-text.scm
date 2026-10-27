;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render plain-text)
  #:use-module (grove private render plain-text)
  #:export (content->plain-text))

(define (content->plain-text content)
  "Project CONTENT values to plain text, including a readable math form."
  (content->plain-text/internal content))
