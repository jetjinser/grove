;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (render-model
            cached-renderer?
            cached-renderer-cache-key
            cached-renderer-procedure
            cached-renderer-render-changed
            make-cached-renderer
            renderer-cache-keys))

(define-record-type <cached-renderer>
  (%make-cached-renderer cache-key procedure render-changed)
  cached-renderer?
  (cache-key cached-renderer-cache-key)
  (procedure cached-renderer-procedure)
  (render-changed cached-renderer-render-changed))

(define* (make-cached-renderer procedure #:key cache-key render-changed)
  (unless (procedure? procedure)
    (error "cached renderer requires a procedure" procedure))
  (unless (and (string? cache-key) (not (string-null? cache-key)))
    (error "cached renderer requires a non-empty cache key" cache-key))
  (unless (or (not render-changed) (procedure? render-changed))
    (error "incremental renderer must be a procedure or #f" render-changed))
  (%make-cached-renderer cache-key procedure render-changed))

(define (renderer-cache-keys renderers)
  (and (every cached-renderer? renderers)
       (map cached-renderer-cache-key renderers)))

(define (render-model model renderers)
  (apply append
    (map (lambda (renderer)
           ((if (cached-renderer? renderer)
                (cached-renderer-procedure renderer)
                renderer)
            model))
         renderers)))
