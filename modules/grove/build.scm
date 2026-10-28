;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:use-module (grove private renderer)
  #:use-module (grove private procedure)
  #:use-module (grove private snapshot)
  #:use-module (grove output)
  #:use-module (srfi srfi-1)
  #:export (render-model
            make-cached-renderer))

(define* (make-cached-renderer procedure #:key (configuration '()) render-changed dependencies)
"Declare PROCEDURE cacheable for project use.

CONFIGURATION is acyclic Scheme data or a model-to-data procedure returning
such data, included in cache identity. Supported values are booleans,
characters, numbers, symbols, keywords, strings, bytevectors, pairs,
and vectors; opaque objects and cyclic structures are rejected.
DEPENDENCIES, when supplied, is a no-argument procedure returning additional
paths to track, or a list of paths when the dependencies are fixed. These paths
trigger project-level invalidation. Each artifact's `#:sources` declares which
inputs affect that output. Together with RENDER-CHANGED, it enables safe
per-artifact reuse; without that callback Grove reruns the renderer as a whole
when it needs new output. RENDER-CHANGED, when supplied, receives the model,
changed source paths, dirty artifact paths, and this renderer's previous
artifacts. It must return the renderer's complete current artifact list, or #f
to request a full render. Use this constructor when its stable cache identity
is known; a plain renderer procedure runs on each build because procedure
identity cannot be persisted safely."
  (unless (procedure-accepts-arguments? procedure 1)
    (error "cached renderer procedure must accept one model" procedure))
  (unless (or (procedure-accepts-arguments? configuration 1)
              (fingerprintable-datum? configuration))
    (error "renderer configuration must be acyclic Scheme data"
           configuration))
  (unless (or (not render-changed)
              (procedure-accepts-arguments? render-changed 4))
    (error "incremental renderer must accept four arguments or be #f"
           render-changed))
  (unless (or (not dependencies)
              (procedure-accepts-arguments? dependencies 0)
              (filesystem-path-list? dependencies))
    (error "renderer dependencies must be #f, a list of paths, or a path collector"
           dependencies))
  (when (list? dependencies)
    (unless (unique-filesystem-path-list? dependencies)
      (error "renderer dependency paths must be unique" dependencies))
    (set! dependencies (snapshot-data dependencies)))
  (%make-cached-renderer
    (if (procedure? configuration) configuration (snapshot-data configuration))
    procedure render-changed dependencies))

(define (render-model model renderers)
  "Apply RENDERERS to MODEL in order and concatenate their artifact lists.
This invokes rendering only; it does not collect inputs, cache results, or
write artifacts. Every renderer must return a list of artifacts."
  (unless (and (list? renderers)
               (every (lambda (renderer)
                        (or (procedure-accepts-arguments? renderer 1)
                            (cached-renderer? renderer)))
                      renderers))
    (error "render-model requires one-argument renderers or cached renderers"
           renderers))
  (append-map
    (lambda (renderer)
      (let ((produced
              ((if (cached-renderer? renderer)
                   (cached-renderer-procedure renderer)
                   renderer)
               model)))
        (unless (and (list? produced) (every artifact? produced))
          (error "renderer must return a list of artifacts"
                 renderer produced))
        produced))
    renderers))
