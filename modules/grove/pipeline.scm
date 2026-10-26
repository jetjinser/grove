;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove pipeline)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (make-asset-pipeline
            asset-pipeline?))

;;     An asset pipeline derives build assets with Scheme code: the
;;     procedure receives an input table (declared path -> materialized
;;     path) and an emit sink (artifact path -> bytes), and Grove turns
;;     what it emits into ordinary content-addressed artifacts. The
;;     project file and declared inputs participate in the cache key.
;;     #:version is an explicit invalidation token for behavior that the
;;     project load inputs cannot fingerprint, such as a change in the
;;     external toolchain or runtime arguments.

(define-record-type <asset-pipeline>
  (%make-asset-pipeline name version inputs procedure)
  asset-pipeline?
  (name asset-pipeline-name)
  (version asset-pipeline-version)
  (inputs asset-pipeline-inputs)
  (procedure asset-pipeline-procedure))

(define (valid-input? input)
  (and (string? input) (not (string-null? input))))

(define* (make-asset-pipeline #:key name (version "") inputs procedure)
  (unless (and (string? name) (not (string-null? name)))
    (error "asset pipeline requires a name" name))
  (unless (string? version)
    (error "asset pipeline version must be a string" version))
  (unless (and (list? inputs) (every valid-input? inputs) (not (null? inputs)))
    (error "asset pipeline inputs must be a non-empty list of paths" inputs))
  (unless (procedure? procedure)
    (error "asset pipeline requires a procedure" procedure))
  (%make-asset-pipeline name version inputs procedure))
