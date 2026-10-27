;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove pipeline)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-asset-pipeline))

;;     An asset pipeline derives build assets with Scheme code: the
;;     procedure receives an input table (declared path -> materialized
;;     path) and an emit sink (artifact path -> bytes), and Grove turns
;;     what it emits into ordinary content-addressed artifacts. The
;;     project file, declared inputs, and Scheme files loaded during
;;     execution participate in cache validation. #:version is an explicit
;;     invalidation token for behavior Grove cannot observe, such as changes
;;     in the external toolchain, environment, or runtime arguments.

(define (valid-input? input)
  (and (string? input) (not (string-null? input))))

(define* (make-asset-pipeline #:key name (version "") (inputs '()) procedure)
  "Construct a file-driven build step named NAME.

PROCEDURE is called with an input lookup and an emit callback. The lookup
accepts only declared INPUTS, which must have unique paths, and returns a
materialized path. EMIT accepts a relative artifact path and a string or
bytevector; the procedure must emit at least one artifact, and its own return
value is ignored. VERSION distinguishes behavior depending on state Grove
cannot track, such as an external toolchain."
  (unless (and (string? name) (not (string-null? name)))
    (error "asset pipeline requires a name" name))
  (unless (string? version)
    (error "asset pipeline version must be a string" version))
  (unless (and (list? inputs) (every valid-input? inputs))
    (error "asset pipeline inputs must be a list of paths" inputs))
  (unless (= (length inputs) (length (delete-duplicates inputs string=?)))
    (error "asset pipeline inputs must be unique" inputs))
  (unless (procedure? procedure)
    (error "asset pipeline requires a procedure" procedure))
  (%make-asset-pipeline (snapshot-data name)
                       (snapshot-data version)
                       (snapshot-data inputs)
                       procedure))
