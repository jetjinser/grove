;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project state)
  #:export (publish-project-state!
            last-build-model
            last-analysis-inputs))

;; Advisory state shared with the long-lived preview worker. One-shot
;; commands publish it, and the worker uses it only to decide whether an
;; asset-only edit can reuse the last analyzed model.
(define %last-build-model #f)
(define %last-analysis-inputs #f)

(define (publish-project-state! model analysis-inputs)
  (set! %last-build-model model)
  (set! %last-analysis-inputs analysis-inputs))

(define (last-build-model) %last-build-model)
(define (last-analysis-inputs) %last-analysis-inputs)
