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
      ;; Inputs can change during analysis or rendering. Compare the initial
      ;; snapshot with the analyzed source fingerprints, then check those
      ;; analyzed fingerprints again after output. This lets cache lookups
      ;; share one dependency observation without letting a drifting build
      ;; publish reusable state.
      (when (or
              (any (lambda (entry)
                     (let ((source
                             (find (lambda (source)
                                     (string=? (car source) (car entry)))
                                   sources)))
                       (not (equal? (cadr entry)
                                    (if source (cadr source)
                                        (fingerprint (car entry)))))))
                   earlier-sources)
              (any (lambda (entry)
                     (not (equal? (cadr entry)
                                  (fingerprint (car entry)))))
                   sources))
        (delete-file state-path)))))
