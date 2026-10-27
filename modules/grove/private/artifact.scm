;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private artifact)
  #:use-module (srfi srfi-9)
  #:export (%make-artifact
            artifact?
            artifact-path/raw
            artifact-content/raw
            artifact-sources/raw))

;; Artifact values are shared by the public output API and internal build
;; stages. Keep their representation separate from filesystem operations.
(define-record-type <artifact>
  (%make-artifact path content sources)
  artifact?
  (path artifact-path/raw)
  (content artifact-content/raw)
  (sources artifact-sources/raw))
