;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private renderer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (%make-cached-renderer
            cached-renderer?
            cached-renderer-configuration
            cached-renderer-procedure
            cached-renderer-render-changed
            cached-renderer-inputs
            renderer-configurations))

(define-record-type <cached-renderer>
  (%make-cached-renderer configuration procedure render-changed inputs)
  cached-renderer?
  (configuration cached-renderer-configuration)
  (procedure cached-renderer-procedure)
  (render-changed cached-renderer-render-changed)
  (inputs cached-renderer-inputs))

(define (renderer-configurations model renderers)
  ;; Keep peer configurations in order. A plain renderer contributes #f only
  ;; for its own slot and does not hide cache identities around it.
  (map (lambda (renderer)
         (and (cached-renderer? renderer)
              (let ((configuration (cached-renderer-configuration renderer)))
                (if (procedure? configuration)
                    (configuration model)
                    configuration))))
       renderers))
