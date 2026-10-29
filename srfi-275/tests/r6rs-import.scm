;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT
#!r6rs
(import (rnrs) (srfi :275 uri))

(let ((value (string->uri "https://example.org/a?q#f")))
  (unless (and (uri? value)
               (string=? (uri->string value) "https://example.org/a?q#f"))
    (assertion-violation 'r6rs-import "SRFI 275 URI import failed" value)))
