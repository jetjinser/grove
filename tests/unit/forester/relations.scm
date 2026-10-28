;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester relations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (forester composition)
  #:use-module (forester relations)
  #:use-module (grove relation dsl))

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

(define content-nodes
  (make-content-node-relation
    '((0 "page" #f paragraph 0 () "outer" 5 5 #f)
      (1 "page" 0 math 0 () "x" 1 1 #t)
      (2 "page" 1 text 0 () "x" 1 1 #t))))

(test-equal "childo returns direct children only"
  '(1)
  (run* (child-id)
    (childo content-nodes 0 child-id)))
(test-equal "root's absent parent is not treated as a content node" '()
  (run* (child-id)
    (childo content-nodes #f child-id)))

(test-assert "ancestor and descendant goals enumerate strict transitive paths"
  (let ((descendants
          (run* (ancestor-id descendant-id)
            (descendanto content-nodes ancestor-id descendant-id)))
        (ancestors
          (run* (ancestor-id descendant-id)
            (ancestoro content-nodes ancestor-id descendant-id))))
    (and (= (length descendants) 3)
         (member '(0 1) descendants equal?)
         (member '(1 2) descendants equal?)
         (member '(0 2) descendants equal?)
         (= (length ancestors) (length descendants))
         (every (lambda (pair) (member pair ancestors equal?)) descendants)
         (not (member '(#f 0) descendants equal?))
         (not (member '(0 0) descendants equal?)))))

(define composition
  (make-composition
    (list (make-subtree-definition "lecture" "exercise"))
    (list (make-transclusion "lecture" "embed-1" "exercise")
          (make-transclusion "lecture" "embed-2" "exercise")
          (make-transclusion "exercise" "embed-1" "definition"))))

(test-equal "composition queries targets across its relation kinds"
  '("exercise" "exercise" "exercise")
  (composition-target-ids composition "lecture"))

(test-equal "composition API returns typed subtree definitions"
  '(("lecture" "exercise"))
  (map (lambda (definition)
         (list (subtree-definition-parent definition)
               (subtree-definition-child definition)))
       (composition-subtree-definitions composition)))

(test-equal "outgoing composition API returns transclusion occurrences"
  '("embed-1" "embed-2")
  (map transclusion-source-id
       (composition-outgoing-transclusions composition "lecture")))

(test-equal "incoming transclusion queries retain each occurrence"
  '(("lecture" "embed-1") ("lecture" "embed-2"))
  (map (lambda (transclusion)
         (list (transclusion-source-tree-id transclusion)
               (transclusion-source-id transclusion)))
       (composition-incoming-transclusions composition "exercise")))

(let* ((parent (string-copy "parent"))
       (child (string-copy "child"))
       (definition (make-subtree-definition parent child)))
  (string-set! parent 0 #\P)
  (string-set! child 0 #\C)
  (test-equal "subtree definitions copy constructor strings"
    '("parent" "child")
    (list (subtree-definition-parent definition)
          (subtree-definition-child definition)))
  (let ((parent-copy (subtree-definition-parent definition)))
    (string-set! parent-copy 0 #\P)
    (test-equal "subtree definition accessors return independent strings"
      "parent" (subtree-definition-parent definition))))

(let* ((source (string-copy "source"))
       (occurrence (string-copy "occurrence"))
       (target (string-copy "target"))
       (transclusion (make-transclusion source occurrence target)))
  (string-set! source 0 #\S)
  (string-set! occurrence 0 #\O)
  (string-set! target 0 #\T)
  (test-equal "transclusions copy constructor strings"
    '("source" "occurrence" "target")
    (list (transclusion-source-tree-id transclusion)
          (transclusion-source-id transclusion)
          (transclusion-target-tree-id transclusion))))

(let* ((definition (make-subtree-definition "parent" "child"))
       (transclusion (make-transclusion "parent" "embed" "target"))
       (definitions (list definition))
       (transclusions (list transclusion))
       (snapshot (make-composition definitions transclusions)))
  (set-car! definitions (make-subtree-definition "other" "page"))
  (set-car! transclusions (make-transclusion "other" "embed" "page"))
  (test-equal "composition copies constructor list structure"
    '(("parent" "child"))
    (map (lambda (item)
           (list (subtree-definition-parent item)
                 (subtree-definition-child item)))
         (composition-subtree-definitions snapshot)))
  (let ((definitions-copy (composition-subtree-definitions snapshot)))
    (set-car! definitions-copy (make-subtree-definition "changed" "page"))
    (test-equal "composition accessors return independent list structure"
      '("child")
      (map subtree-definition-child
           (composition-subtree-definitions snapshot))))
  (let ((outgoing (composition-outgoing-transclusions snapshot "parent")))
    (set-car! outgoing (make-transclusion "changed" "other" "page"))
    (test-equal "transclusion queries return independent list structure"
      '("embed")
      (map transclusion-source-id
           (composition-outgoing-transclusions snapshot "parent")))))

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
  '(("term-loom" ("glossary" "term-loom" "loom")
                 ("ch01" "term-loom" "织机")))
  (term-definition-conflicts definitions))

(define mentions
  (make-term-mention-relation
    '(("term-loom" "ch01" #t)
      ("term-ghost" "ch02" #f)
      ("term-ghost" "ch03" #f))))

(test-equal "undefined mentions drop defined terms and group pages"
  '(("term-ghost" ("ch02" "ch03")))
  (undefined-term-mentions definitions mentions))

(test-equal "mention lookups keep pages with their resolution state"
  '(("ch02" #f) ("ch03" #f))
  (term-mention-rows mentions "term-ghost"))

(test-end "forester relations")
