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
    '(("a" "ref-1" "b" "B")
      ("a" "ref-2" "b" "Another B")
      ("a" "ref-3" "c" "C")
      ("b" "ref-1" "c" "C")
      ("c" "ref-1" "a" "A")
      ("d" "ref-1" "b" "B"))))

(test-equal "incoming row queries retain all fields and occurrences"
  '(("a" "ref-1" "b" "B")
    ("a" "ref-2" "b" "Another B")
    ("d" "ref-1" "b" "B"))
  (incoming-reference-rows references "b"))

(test-equal "outgoing row queries preserve relation order"
  '(("a" "ref-1" "b" "B")
    ("a" "ref-2" "b" "Another B")
    ("a" "ref-3" "c" "C"))
  (outgoing-reference-rows references "a"))

(test-equal "incoming queries retain reference occurrences and order"
  '(("a" "ref-1") ("a" "ref-2") ("d" "ref-1"))
  (incoming-reference-keys references "b"))

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
      '(("self" "ref-1" "self" "Self")))
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

(define definitions
  (make-term-definition-relation
    '(("term-loom" "glossary" "term-loom" "loom")
      ("term-loom" "ch01" "term-loom" "织机")
      ("term-tree" "glossary" "term-tree" "tree"))))

(test-equal "term definition queries return one term's occurrences"
  '(("glossary" "term-loom" "loom")
    ("ch01" "term-loom" "织机"))
  (term-definition-rows definitions "term-loom"))

(test-equal "an unqueried term returns no definition rows"
  '() (term-definition-rows definitions "term-ghost"))

(test-equal "term definition conflicts flag multi-page and drifted terms"
  '(("term-loom" ("glossary" "loom") ("ch01" "织机")))
  (term-definition-conflicts definitions))

(define mentions
  (make-term-mention-relation
    '(("term-loom" "ch01")
      ("term-ghost" "ch02")
      ("term-ghost" "ch03"))))

(test-equal "undefined mentions drop defined terms and group pages"
  '(("term-ghost" ("ch02" "ch03")))
  (undefined-term-mentions definitions mentions))

(test-equal "mention lookups return one term's pages"
  '("ch02" "ch03")
  (term-mention-rows mentions "term-ghost"))

(test-end "forester relations")
