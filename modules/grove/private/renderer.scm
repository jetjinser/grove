;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private renderer)
  #:use-module (grove private fingerprint)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (%make-cached-renderer
            cached-renderer?
            cached-renderer-configuration
            cached-renderer-procedure
            cached-renderer-render-changed
            cached-renderer-dependencies
            renderer-configurations))

(define-record-type <cached-renderer>
  (%make-cached-renderer configuration procedure render-changed dependencies)
  cached-renderer?
  (configuration cached-renderer-configuration)
  (procedure cached-renderer-procedure)
  (render-changed cached-renderer-render-changed)
  (dependencies cached-renderer-dependencies))

(define (renderer-configurations model renderers)
  ;; Keep peer configurations in order. A plain renderer contributes #f only
  ;; for its own slot and does not hide cache identities around it.
  (map (lambda (renderer)
         (and (cached-renderer? renderer)
              (let* ((configuration (cached-renderer-configuration renderer))
                     (value (if (procedure? configuration)
                                (configuration model)
                                configuration)))
                (unless (fingerprintable-datum? value)
                  (error "renderer configuration must produce acyclic Scheme data"
                         value))
                value)))
       renderers))
