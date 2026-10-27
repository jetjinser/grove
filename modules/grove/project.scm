;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove private project model)
  #:use-module (grove private project lifecycle)
  #:re-export (project?
               make-project
               create-project-source
               collect-project-sources
               project-with-options
               project-input?
               project-input-path
               project-input-value
               project-context?
               project-context-inputs
               project-context-prepared-sources
               project-context-extra-sources
               build-report?
               build-report-sources
               build-report-artifacts
               build-report-added-sources
               build-report-modified-sources
               build-report-removed-sources
               build-report-affected-artifacts
               build-report-render-cache-hit?
               build-project/report
               build-project
               check-project
               analyze-project
               browse-project))
