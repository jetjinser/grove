;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove source-fixtures)
  #:export (located-text))

;; Compiled string constants do not retain reader locations. Tests asserting
;; source provenance must supply a location, as a .plant reader would.
(define (located-text text)
  (let ((copy (string-copy text)))
    (set-source-properties! copy
      '((filename . "fixture.plant") (line . 0) (column . 0)))
    copy))
