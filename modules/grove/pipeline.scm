;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove pipeline)
  #:use-module (grove private files)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private snapshot)
  #:use-module (grove private procedure)
  #:use-module (grove private string-fun)
  #:export (make-asset-pipeline))

;;     An asset pipeline derives build assets with Scheme code: the
;;     procedure receives an input lookup (declared path -> materialized
;;     path) and returns Grove artifacts, which become ordinary
;;     content-addressed outputs. The
;;     project file, declared inputs, and Scheme files loaded during
;;     execution participate in cache validation. #:version is an explicit
;;     invalidation token for behavior Grove cannot observe, such as changes
;;     in the external toolchain, environment, or runtime arguments.

(define* (make-asset-pipeline #:key name (version "") (inputs '()) procedure)
  "Construct a file-driven build step named NAME.

PROCEDURE is called with an input lookup. The lookup accepts only declared
INPUTS, which must have unique paths, and returns a materialized path. The
procedure returns a list of Grove artifacts, which may be empty. Their source
dependencies are unioned across the pipeline: each returned artifact receives
the declared inputs, tracked runtime reads, and every artifact's source list.
This makes affected-artifact reporting conservative across the pipeline;
split outputs into separate pipelines when they have independent dependencies.
VERSION distinguishes behavior depending on state Grove cannot track, such as
an external toolchain."
  (unless (non-empty-string? name)
    (error "asset pipeline requires a name" name))
  (unless (string? version)
    (error "asset pipeline version must be a string" version))
  (unless (filesystem-path-list? inputs)
    (error "asset pipeline inputs must be a list of paths" inputs))
  (unless (unique-filesystem-path-list? inputs)
    (error "asset pipeline inputs must be unique" inputs))
  (unless (procedure-accepts-arguments? procedure 1)
    (error "asset pipeline procedure must accept an input lookup" procedure))
  (%make-asset-pipeline (snapshot-data name)
                       (snapshot-data version)
                       (snapshot-data inputs)
                       procedure))
