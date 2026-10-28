;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math provenance)
  #:use-module (grove math private source)
  #:re-export (math-location?
               math-location-file
               math-location-line
               math-location-column
               math-location-excerpt
               math-location-label
               math-origin?
               math-origin-location
               math-origin-locations
               math-origin-at-path
               math-origin-generated?
               math-origin-rule))
