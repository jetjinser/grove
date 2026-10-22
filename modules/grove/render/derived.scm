;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render derived)
  #:use-module (grove render context)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (derived-artifact?
            make-derived-artifact
            derived-artifact-name
            compose-derived-artifacts
            render-derived-artifacts))

;; A derived artifact is an independent projection of the forest.  It is not
;; an HTML interpretation: its procedure receives the render context and the
;; forest and returns artifacts.  Composition is list concatenation.

(define-record-type <derived-artifact>
  (%make-derived-artifact name procedure)
  derived-artifact?
  (name derived-artifact-name)
  (procedure derived-artifact-procedure))

(define* (make-derived-artifact #:key name procedure)
  (unless (and (string? name) (not (string-null? name)))
    (error "derived artifact name must be a non-empty string" name))
  (unless (procedure? procedure)
    (error "derived artifact procedure must be a procedure" procedure))
  (%make-derived-artifact name procedure))

(define (compose-derived-artifacts . artifacts)
  (append-map (lambda (artifact)
                (if (list? artifact) artifact (list artifact)))
              artifacts))

(define (render-derived-artifacts artifacts context forest)
  (unless (every derived-artifact? artifacts)
    (error "not a derived artifact list" artifacts))
  (unless (render-context? context)
    (error "derived artifacts require a render context" context))
  (append-map (lambda (artifact)
                ((derived-artifact-procedure artifact) context forest))
              artifacts))
