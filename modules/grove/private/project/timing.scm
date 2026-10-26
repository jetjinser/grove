;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project timing)
  #:export (current-analysis-timings
            current-analysis-phase-start))

;; Optional callbacks connect analyzer phase boundaries to CLI progress and
;; profiling without making them project configuration.
(define current-analysis-timings (make-parameter #f))
(define current-analysis-phase-start (make-parameter #f))
