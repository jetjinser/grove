;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build)
  #:use-module (grove private snapshot)
  #:use-module (grove string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (render-model
            make-cached-renderer))

(define-record-type <cached-renderer>
  (%make-cached-renderer cache? configuration procedure render-changed inputs)
  cached-renderer?
  (cache? cached-renderer-cache?)
  (configuration cached-renderer-configuration)
  (procedure cached-renderer-procedure)
  (render-changed cached-renderer-render-changed)
  (inputs cached-renderer-inputs))

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

(define (renderer-configurations model renderers)
  ;; Configuration describes data, never procedure identity or memory addresses.
  (and (every (lambda (renderer)
                (and (cached-renderer? renderer) (cached-renderer-cache? renderer))) renderers)
       (map (lambda (renderer)
              (let ((configuration (cached-renderer-configuration renderer)))
                (if (procedure? configuration) (configuration model) configuration)))
            renderers)))

(define (render-model model renderers)
  (apply append
    (map (lambda (renderer)
           ((if (cached-renderer? renderer)
                (cached-renderer-procedure renderer)
                renderer)
            model))
         renderers)))
