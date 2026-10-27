;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project publication)
  #:use-module (grove output)
  #:use-module (grove private build-state)
  #:use-module (grove private input dependencies)
  #:use-module (grove project diagnostic)
  #:use-module (srfi srfi-1)
  #:export (validate-project-artifacts
            write-project-build!))

(define (validate-project-artifacts artifacts progress)
  (call-with-project-stage 'validate #f
    (lambda ()
      (progress 'validate #f)
      (validate-artifacts artifacts))))

(define* (write-project-build! directory state-path state-temp-path sources
                               artifacts owners identity progress
                               #:key (earlier-sources '())
                                     (prune (lambda () #t))
                                     (fingerprint input-fingerprint))
  (call-with-project-stage 'write directory
    (lambda ()
      (progress 'write directory)
      (write-artifacts directory artifacts)
      (write-build-state state-path state-temp-path sources artifacts
                         owners identity)
      (prune)
      ;; Inputs can change while outputs are being rendered. Invalidate the
      ;; just-written state so a later build cannot reuse unseen source bytes.
      (when (any (lambda (entry)
                  (not (equal? (cadr entry)
                               (fingerprint (car entry)))))
                (append sources earlier-sources))
        (delete-file state-path)))))
