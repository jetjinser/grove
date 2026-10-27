;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build)
  #:use-module (grove private renderer)
  #:use-module (grove private snapshot)
  #:export (render-model
            make-cached-renderer))

(define* (make-cached-renderer procedure #:key (cache? #t) (configuration '()) render-changed inputs)
  (unless (boolean? cache?)
    (error "renderer cache option must be a boolean" cache?))
  (unless (procedure? procedure)
    (error "cached renderer requires a procedure" procedure))
  (unless (or (not render-changed) (procedure? render-changed))
    (error "incremental renderer must be a procedure or #f" render-changed))
  (unless (or (not inputs) (procedure? inputs))
    (error "renderer input collector must be a procedure or #f" inputs))
  (%make-cached-renderer
    cache?
    (if (procedure? configuration) configuration (snapshot-data configuration))
    procedure render-changed inputs))

(define (render-model model renderers)
  "Apply RENDERERS to MODEL in order and concatenate their artifact lists.
This invokes rendering only; it does not collect inputs, cache results, or
write artifacts."
  (apply append
    (map (lambda (renderer)
           ((if (cached-renderer? renderer)
                (cached-renderer-procedure renderer)
                renderer)
            model))
         renderers)))
