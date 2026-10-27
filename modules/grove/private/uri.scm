;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private uri)
  #:use-module (web uri)
  #:use-module (srfi srfi-14)
  #:use-module (srfi srfi-13)
  #:export (uri-encode-path split-uri-suffix))

(define uri-path-characters
  (string->char-set
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~!$&'()*+,;=:@/"))

(define (uri-encode-path path)
  "Percent-encode PATH as a URI path, preserving path separators."
  (unless (string? path)
    (error "URI path must be a string" path))
  (uri-encode path #:unescaped-chars uri-path-characters))

(define (split-uri-suffix value)
  "Split a URI reference into its path and the query/fragment suffix."
  (unless (string? value)
    (error "URI reference must be a string" value))
  (let ((query (string-index value #\?))
        (fragment (string-index value #\#)))
    (let ((end (cond ((and query fragment) (min query fragment))
                     (query query)
                     (fragment fragment)
                     (else (string-length value)))))
      (values (substring value 0 end) (substring value end)))))
