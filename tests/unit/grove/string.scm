;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove string-fun)
  #:use-module (srfi srfi-64)
  #:use-module (grove string-fun))

(test-begin "grove string-fun")

(test-assert "non-empty-string? accepts non-empty strings"
  (non-empty-string? "grove"))
(test-assert "non-empty-string? rejects the empty string"
  (not (non-empty-string? "")))
(test-assert "non-empty-string? rejects non-strings"
  (not (non-empty-string? #f)))

(test-end "grove string-fun")
