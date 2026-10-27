;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private uri)
  #:use-module (web uri)
  #:use-module (srfi srfi-14)
  #:export (uri-encode-path))

(define uri-path-characters
  (string->char-set
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~!$&'()*+,;=:@/"))

(define (uri-encode-path path)
  "Percent-encode PATH as a URI path, preserving path separators."
  (unless (string? path)
    (error "URI path must be a string" path))
  (uri-encode path #:unescaped-chars uri-path-characters))
