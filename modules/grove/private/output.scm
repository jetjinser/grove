;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private output)
  #:use-module (grove output)
  #:export (artifact-path/raw artifact-content/raw artifact-sources/raw))

;; Internal build and renderer code reads stable artifacts without copying
;; potentially large rendered strings or bytevectors on every access.
(define artifact-path/raw (@@ (grove output) artifact-path/raw))
(define artifact-content/raw (@@ (grove output) artifact-content/raw))
(define artifact-sources/raw (@@ (grove output) artifact-sources/raw))
