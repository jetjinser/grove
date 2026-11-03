;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render anchors)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (ice-9 format)
  #:export (occurrence-anchor
            anchor-child-prefix))

(define (anchor-child-prefix anchor)
  "Return the prefix for IDs rendered inside the occurrence ANCHOR."
  (string-append anchor ":"))

(define (occurrence-anchor element prefix position)
  "Return the anchor for a rendered tree occurrence.

An authored or analyzed anchor takes precedence over the generated position
name. PREFIX scopes either name when this occurrence is rendered inside
another tree, so nested content keeps one consistent occurrence path."
  (string-append prefix
    (or (ir:content-element-property/raw element 'anchor)
        (format #f "tree-~a" position))))
