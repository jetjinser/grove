;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project publication)
  #:use-module (grove output)
  #:use-module (grove private build-state)
  #:use-module (grove private input dependencies)
  #:use-module (grove private project source-set)
  #:use-module (grove project diagnostic)
  #:export (validate-project-artifacts
            write-project-build!))

(define (validate-project-artifacts artifacts progress)
  (call-with-project-stage 'validate #f
    (lambda ()
      (progress 'validate #f)
      (validate-artifacts artifacts))))

(define* (write-project-build! directory state-path state-temp-path source-set
                               artifacts owners identity progress
                               #:key (prune (lambda () #t))
                                     (fingerprint input-fingerprint))
  (call-with-project-stage 'write directory
    (lambda ()
      (progress 'write directory)
      (write-artifacts directory artifacts)
      (write-build-state state-path state-temp-path
                         (project-source-set-entries source-set) artifacts
                         owners identity)
      (prune)
      ;; Inputs can change during analysis or rendering. Compare the initial
      ;; snapshot with the analyzed source fingerprints, then check those
      ;; analyzed fingerprints again after output. This lets cache lookups
      ;; share one dependency observation without letting a drifting build
      ;; publish reusable state.
      (unless (project-source-set-stable? source-set
                                          #:fingerprint fingerprint)
        (delete-file state-path)))))
