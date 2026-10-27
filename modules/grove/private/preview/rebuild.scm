;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private preview rebuild)
  #:use-module (grove private project model)
  #:use-module (grove private project incremental)
  #:use-module (grove private project artifacts)
  #:use-module (grove private project publication)
  #:use-module (grove private project renderers)
  #:use-module (grove private cache-dir)
  #:use-module (grove private build-lock)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove private pipeline)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private build-state)
  #:use-module (srfi srfi-1)
  #:export (rebuild-from-model))

;;     Model-level reuse: a long-lived preview worker retains the
;;     model of its last successful build and re-renders with a
;;     freshly loaded project when only load-time assets changed.
;;     Analysis is skipped wholesale; render-with-cache re-runs just
;;     the renderers whose inputs changed, and renderers without an
;;     incremental callback keep their previous artifacts outright -
;;     the retained model and each renderer's identity
;;     make their output byte-identical. The on-disk build state
;;     is refreshed (with content fingerprints) so manual builds stay
;;     incremental.  Cache pruning is skipped here - the retained
;;     state carries no selection statuses, and pruning is hygiene.
(define* (rebuild-from-model project model retained-state changed removed
                              #:key (progress (lambda (stage path) #t)))
  "Render and write using a MODEL built by a previous analysis of PROJECT.
RETAINED-STATE is that build's on-disk state record; CHANGED names sources
modified or added since, and REMOVED names deleted sources. Renderers and
project configuration come from the freshly loaded PROJECT, while MODEL
preserves the prior analysis result."
  (with-build-lock (project-output-directory project)
    (lambda ()
      (let* ((cache-directory
                (grove-cache-directory (project-output-directory project)))
             (state-path
                (string-append cache-directory "/" build-state-name))
             (state-temp-path
                (string-append cache-directory "/" build-state-temp-name))
              (previous-sources (build-state-sources retained-state))
              (previous-artifacts (build-state-artifacts retained-state))
              (pipeline-result
               (parameterize ((current-input-fingerprints (make-hash-table)))
                 (run-asset-pipelines (project-pipelines project)
                                      (current-project-key)
                                      #:cache? (project-cache? project))))
              (pipeline-artifacts (car pipeline-result))
              (pipeline-owners (cdr pipeline-result))
              (retired (retired-pipeline-paths pipeline-artifacts
                                               previous-artifacts))
              (identity (renderer-identity (project-cache? project) model
                                           (project-renderers project)))
              (previous-identity (build-state-identity retained-state))
              (cached-artifacts
                (load-cached-artifacts (project-output-directory project)
                                       previous-artifacts))
             ;;     Capture content before rendering: the retained model
             ;;     reflects these bytes, and a source edited during the
             ;;     render must not enter the state as if the render had
             ;;     seen it.
             (current-sources
                (filter-map
                  (lambda (source)
                    (let ((digest (false-if-exception
                                    (file-fingerprint
                                      (build-state-source-path source)))))
                      (and digest
                           (list (build-state-source-path source)
                                 digest
                                 (build-state-source-kind source)))))
                  previous-sources))
             (render-result
                (begin
                  (progress 'render #f)
                  (render-with-cache model (project-renderers project)
                                     previous-artifacts cached-artifacts
                                     previous-sources changed progress
                                     #:model-retained? #t
                                     #:renderer-identities identity
                                     #:previous-renderer-identities
                                     previous-identity)))
              (artifacts (merge-pipeline-artifacts pipeline-artifacts
                                                   (car render-result) retired))
              (owners (append pipeline-owners (cadr render-result)))
             (affected
               (let ((reused
                       (reused-renderer-artifact-paths
                         artifacts owners (caddr render-result))))
                 (delete-duplicates
                   (filter (lambda (path) (not (member path reused string=?)))
                     (append
                       (affected-artifacts artifacts previous-artifacts changed)
                       (changed-renderer-artifact-paths
                         artifacts owners previous-artifacts identity
                         previous-identity)))
                   string=?)))
             (report-removed
                (filter (lambda (p) (not (member p changed string=?))) removed)))
         (validate-project-artifacts artifacts progress)
         (write-project-build!
           (project-output-directory project) state-path state-temp-path
           current-sources artifacts owners identity progress
           #:fingerprint
           (lambda (path)
             (false-if-exception (file-fingerprint path))))
          (make-build-report
            (map build-state-source-path current-sources)
            artifacts
            '()
            changed
            report-removed
            affected
            #t)))))
