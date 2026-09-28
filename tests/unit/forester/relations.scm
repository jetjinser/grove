;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester relations)
  #:use-module (srfi srfi-64)
  #:use-module (forester relations)
  #:use-module (grove relation))

(test-begin "forester relations")

(define references
  (make-forest-reference-relation
    '(("a" "ref-1" "b" #f "B")
      ("a" "ref-2" "b" "section" "B section")
      ("a" "ref-3" "c" #f "C")
      ("b" "ref-1" "c" #f "C")
      ("c" "ref-1" "a" #f "A")
      ("d" "ref-1" "b" #f "B"))))

(test-equal "incoming queries retain reference occurrences and order"
  '(("a" "ref-1") ("a" "ref-2") ("d" "ref-1"))
  (forest-incoming-reference-keys references "b"))

(test-equal "fragment queries select the matching reference occurrence"
  '(("a" "ref-2"))
  (forest-incoming-reference-keys references "b" "section"))

(test-equal "outgoing queries return local reference ids"
  '("ref-1" "ref-2" "ref-3")
  (forest-outgoing-reference-ids references "a"))

(test-equal "backlinks return each source page once"
  '("a" "d")
  (forest-backlink-page-ids references "b"))

(test-equal "common backlinks join independently of reference multiplicity"
  '("a")
  (forest-common-backlink-page-ids references "b" "c"))

(test-equal "recursive reachability follows links and terminates through cycles"
  '("b" "c")
  (forest-reachable-page-ids references "a"))

(test-equal "an isolated page has no reachable pages"
  '()
  (forest-reachable-page-ids references "isolated"))

(test-equal "a self-link does not make its page reachable from itself"
  '()
  (forest-reachable-page-ids
    (make-forest-reference-relation
      '(("self" "ref-1" "self" #f "Self")))
    "self"))

(define transclusions
  (make-forest-transclusion-relation
    '(("lecture" "embed-1" "exercise")
      ("lecture" "embed-2" "exercise")
      ("exercise" "embed-1" "definition"))))

(test-equal "incoming transclusion queries retain each occurrence"
  '(("lecture" "embed-1") ("lecture" "embed-2"))
  (forest-incoming-transclusion-ids transclusions "exercise"))

(test-equal "outgoing transclusion queries return targets and local IDs"
  '(("embed-1" "exercise") ("embed-2" "exercise"))
  (forest-outgoing-transclusion-ids transclusions "lecture"))

(test-end "forester relations")
