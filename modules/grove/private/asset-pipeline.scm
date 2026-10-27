;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private asset-pipeline)
  #:use-module (srfi srfi-9)
  #:export (%make-asset-pipeline
            asset-pipeline?
            asset-pipeline-name
            asset-pipeline-version
            asset-pipeline-inputs
            asset-pipeline-procedure))

(define-record-type <asset-pipeline>
  (%make-asset-pipeline name version inputs procedure)
  asset-pipeline?
  (name asset-pipeline-name)
  (version asset-pipeline-version)
  (inputs asset-pipeline-inputs)
  (procedure asset-pipeline-procedure))
