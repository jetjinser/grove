;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove string)
  #:use-module (srfi srfi-13)
  #:export (non-empty-string?))

(define (non-empty-string? value)
  (and (string? value) (not (string-null? value))))
