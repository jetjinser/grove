;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private ir)
  #:use-module (grove ir)
  #:export (content-text-value/raw
            content-text-source/raw
            content-text-location/raw
            content-layout-raw/raw
            content-layout-source/raw
            content-element-children/raw
            content-element-properties/raw
            content-element-property/raw
            content-element-source/raw
            content-document-children/raw
            content-document-source/raw))

;; Built-in consumers already receive values constructed by Grove. They can
;; read the stable internal fields directly; external callers use the public
;; snapshotting accessors from (grove ir).
(define content-text-value/raw (@@ (grove ir) content-text-value/raw))
(define content-text-source/raw (@@ (grove ir) content-text-source/raw))
(define content-text-location/raw (@@ (grove ir) content-text-location/raw))
(define content-layout-raw/raw (@@ (grove ir) content-layout-raw/raw))
(define content-layout-source/raw (@@ (grove ir) content-layout-source/raw))
(define content-element-children/raw (@@ (grove ir) content-element-children/raw))
(define content-element-properties/raw (@@ (grove ir) content-element-properties/raw))
(define content-element-property/raw (@@ (grove ir) content-element-property/raw))
(define content-element-source/raw (@@ (grove ir) content-element-source/raw))
(define content-document-children/raw (@@ (grove ir) content-document-children/raw))
(define content-document-source/raw (@@ (grove ir) content-document-source/raw))
