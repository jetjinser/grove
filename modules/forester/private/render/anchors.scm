;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render anchors)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (ice-9 format)
  #:export (occurrence-anchor))

(define (occurrence-anchor element prefix position)
  "Return the anchor for a rendered tree occurrence.

An authored or analyzed anchor takes precedence. Otherwise, derive the same
position anchor for every renderer from PREFIX and POSITION."
  (or (ir:content-element-property/raw element 'anchor)
      (format #f "~atree-~a" prefix position)))
