;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove pipeline)
  #:use-module (grove private files)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private snapshot)
  #:use-module (grove private procedure)
  #:use-module (grove private string-fun)
  #:use-module (grove output)
  #:use-module (srfi srfi-1)
  #:export (make-asset-pipeline
            pipeline-output))

;;     An asset pipeline derives build assets with Scheme code. Its declared
;;     inputs can name project files or outputs of other pipelines, giving
;;     Grove enough information to schedule and cache a dataflow graph.

(define (unique-pipeline-inputs? inputs)
  (let loop ((remaining inputs) (seen '()))
    (or (null? remaining)
        (and (not (any (lambda (old)
                         (pipeline-input-key=? old (car remaining)))
                       seen))
             (loop (cdr remaining) (cons (car remaining) seen))))))

(define (pipeline-output pipeline path)
  "Refer to PATH produced by PIPELINE, for use in another pipeline's inputs.
The producer must be included in the same project as the consumer."
  (unless (asset-pipeline? pipeline)
    (error "pipeline output requires an asset pipeline producer" pipeline))
  (unless (artifact-path? path)
    (error "pipeline output requires a relative artifact path" path))
  (%make-pipeline-output pipeline (snapshot-data path)))

(define* (make-asset-pipeline #:key name (version "") (inputs '()) procedure
                              (publish-consumed? #f))
  "Construct a file-driven build step named NAME.

PROCEDURE is called with an input lookup. INPUTS is a list of unique
filesystem paths or PIPELINE-OUTPUT references. The lookup accepts one of
those declared inputs and returns a materialized path. A reference creates a
data dependency and Grove schedules its producer first. The procedure returns
a list of Grove artifacts, which may be empty. Their source dependencies are
unioned across the pipeline. An output consumed by another pipeline is
private by default; set PUBLISH-CONSUMED? to #t to also publish this
pipeline's outputs as site artifacts. VERSION distinguishes behavior Grove
cannot track, such as an external toolchain."
  (unless (non-empty-string? name)
    (error "asset pipeline requires a name" name))
  (unless (string? version)
    (error "asset pipeline version must be a string" version))
  (unless (and (list? inputs) (every pipeline-input? inputs)
               (every (lambda (input)
                        (or (not (string? input))
                            (filesystem-path-string? input)))
                      inputs))
    (error "asset pipeline inputs must be paths or pipeline outputs" inputs))
  (unless (unique-pipeline-inputs? inputs)
    (error "asset pipeline inputs must be unique" inputs))
  (unless (boolean? publish-consumed?)
    (error "asset pipeline publish-consumed? option must be a boolean"
           publish-consumed?))
  (unless (procedure-accepts-arguments? procedure 1)
    (error "asset pipeline procedure must accept an input lookup" procedure))
  (%make-asset-pipeline (snapshot-data name)
                       (snapshot-data version)
                       (snapshot-data inputs)
                       procedure
                       publish-consumed?))
