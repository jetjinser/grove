;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (proto macros)
  #:export (shout))

(define-syntax-rule (shout text)
  (string-append text "!"))
