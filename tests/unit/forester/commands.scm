;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester commands)
  #:use-module (srfi srfi-64)
  #:use-module (forester commands)
  #:use-module (grove ir))

(define (label-text element)
  (content-verbatim-string (content-element-children element)))

(test-begin "forester commands")

(let ((element (link "destination")))
  (test-equal "unlabeled links use their target as the label"
    "destination"
    (label-text element))
  (test-equal "links retain their target property"
    "destination"
    (content-element-property element 'target)))

(let ((element (link "destination" "read " "this")))
  (test-equal "links preserve explicitly supplied labels"
    "read this"
    (label-text element))
  (test-equal "links preserve their target with an explicit label"
    "destination"
    (content-element-property element 'target)))

(for-each
  (lambda (constructor kind)
    (let ((element (constructor "topic")))
      (test-equal (format #f "unlabeled ~a uses its ID as the label" kind)
        "topic"
        (label-text element))
      (test-equal (format #f "~a retains its ID property" kind)
        "topic"
        (content-element-property element 'id))
      (test-equal (format #f "~a is block-level" kind)
        'block
        (content-element-flow-kind element)))
    (let ((element (constructor "topic" "display " "name")))
      (test-equal (format #f "~a preserves explicitly supplied labels" kind)
        "display name"
        (label-text element))))
  (list tag taxon)
  '(tag taxon))

(for-each
  (lambda (invalid-id)
    (test-error (format #f "topic IDs reject ~s" invalid-id) #t
      (tag invalid-id)))
  '("" "-leading" "trailing-" "not valid"))

(let ((element (image "diagram.svg" "A " "diagram")))
  (test-equal "images retain their source property"
    "diagram.svg"
    (content-element-property element 'source))
  (test-equal "images preserve alternative text"
    "A diagram"
    (label-text element)))

(test-error "images reject an empty source" #t
  (image ""))

(test-end "forester commands")
