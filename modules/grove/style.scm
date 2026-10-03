;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style)
  #:use-module (srfi srfi-9)
  #:export (make-content-style content-style? content-style-name
            content-style-properties))

(define-record-type <content-style>
  (%make-content-style name properties)
  content-style?
  (name content-style-name)
  (properties content-style-properties))

(define* (make-content-style name #:optional (properties '()))
  "Describe presentation intent and renderer-specific style properties."
  (unless (or (not name) (string? name) (symbol? name))
    (error "content style name must be a string, symbol, or #f" name))
  (unless (list? properties)
    (error "content style properties must be a list" properties))
  (%make-content-style name properties))
