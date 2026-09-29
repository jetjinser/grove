;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester relations)
  #:use-module (srfi srfi-64)
  #:use-module (forester composition)
  #:use-module (forester relations))

(test-begin "forester relations")

(define references
  (make-reference-relation
    '(("a" "ref-1" "b" #f "B")
      ("a" "ref-2" "b" "section" "B section")
      ("a" "ref-3" "c" #f "C")
      ("b" "ref-1" "c" #f "C")
      ("c" "ref-1" "a" #f "A")
      ("d" "ref-1" "b" #f "B"))))

(test-equal "incoming queries retain reference occurrences and order"
  '(("a" "ref-1") ("a" "ref-2") ("d" "ref-1"))
  (incoming-reference-keys references "b"))

(test-equal "fragment queries select the matching reference occurrence"
  '(("a" "ref-2"))
  (incoming-reference-keys references "b" "section"))

(test-equal "outgoing queries return local reference ids"
  '("ref-1" "ref-2" "ref-3")
  (outgoing-reference-ids references "a"))

(test-equal "backlinks return each source page once"
  '("a" "d")
  (backlink-page-ids references "b"))

(test-equal "common backlinks join independently of reference multiplicity"
  '("a")
  (common-backlink-page-ids references "b" "c"))

(test-equal "recursive reachability follows links and terminates through cycles"
  '("b" "c")
  (reachable-page-ids references "a"))

(test-equal "an isolated page has no reachable pages"
  '()
  (reachable-page-ids references "isolated"))

(test-equal "a self-link does not make its page reachable from itself"
  '()
  (reachable-page-ids
    (make-reference-relation
      '(("self" "ref-1" "self" #f "Self")))
    "self"))

(define composition
  (make-composition
    (list (make-subtree-definition "lecture" "exercise"))
    (list (make-transclusion "lecture" "embed-1" "exercise")
          (make-transclusion "lecture" "embed-2" "exercise")
          (make-transclusion "exercise" "embed-1" "definition"))))

(test-equal "composition queries targets across its relation kinds"
  '("exercise" "exercise" "exercise")
  (target-ids composition "lecture"))

(test-equal "composition API returns typed subtree definitions"
  '(("lecture" "exercise"))
  (map (lambda (definition)
         (list (subtree-definition-parent definition)
               (subtree-definition-child definition)))
       (subtree-definitions composition)))

(test-equal "outgoing composition API returns transclusion occurrences"
  '("embed-1" "embed-2")
  (map transclusion-source-id
       (outgoing-transclusions composition "lecture")))

(test-equal "incoming transclusion queries retain each occurrence"
  '(("lecture" "embed-1") ("lecture" "embed-2"))
  (map (lambda (transclusion)
         (list (transclusion-source-tree-id transclusion)
               (transclusion-source-id transclusion)))
       (incoming-transclusions composition "exercise")))

(test-error "subtree definition API rejects empty tree IDs" #t
  (make-subtree-definition "parent" ""))

(test-error "composition API rejects duplicate transclusion occurrences" #t
  (make-composition
    '()
    (list (make-transclusion "lecture" "embed-1" "exercise")
          (make-transclusion "lecture" "embed-1" "definition"))))

(test-end "forester relations")
