;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester composition)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (<subtree-definition>
            subtree-definition?
            make-subtree-definition
            subtree-definition-parent
            subtree-definition-child
            <transclusion>
            transclusion?
            make-transclusion
            transclusion-source-tree-id
            transclusion-source-id
            transclusion-target-tree-id
            composition?
            make-composition
            target-ids
            subtree-definitions
            outgoing-transclusions
            incoming-transclusions))

(define-syntax all-of
  (syntax-rules ()
    ((_ pred arg ...)
     (let ((p pred))
       (and (p arg) ...)))))

(define-record-type <subtree-definition>
  (%make-subtree-definition parent child)
  subtree-definition?
  (parent %subtree-definition-parent)
  (child %subtree-definition-child))

(define-record-type <transclusion>
  (%make-transclusion source-tree-id source-id target-tree-id)
  transclusion?
  (source-tree-id %transclusion-source-tree-id)
  (source-id %transclusion-source-id)
  (target-tree-id %transclusion-target-tree-id))

(define (subtree-definition-parent definition)
  (string-copy (%subtree-definition-parent definition)))

(define (subtree-definition-child definition)
  (string-copy (%subtree-definition-child definition)))

(define (transclusion-source-tree-id transclusion)
  (string-copy (%transclusion-source-tree-id transclusion)))

(define (transclusion-source-id transclusion)
  (string-copy (%transclusion-source-id transclusion)))

(define (transclusion-target-tree-id transclusion)
  (string-copy (%transclusion-target-tree-id transclusion)))

(define (make-subtree-definition parent child)
  (unless (all-of non-empty-string? parent child)
    (error "subtree definition requires non-empty tree IDs"
           parent child))
  (%make-subtree-definition (string-copy parent) (string-copy child)))

(define (make-transclusion source-tree-id source-id target-tree-id)
  (unless (all-of non-empty-string? source-tree-id source-id target-tree-id)
    (error "transclusion requires non-empty tree and occurrence IDs"
           source-tree-id source-id target-tree-id))
  (%make-transclusion (string-copy source-tree-id)
                      (string-copy source-id)
                      (string-copy target-tree-id)))

(define-relation-schema make-composition-relation compositiono
  (kind source occurrence target))

(define-record-type <composition>
  (%make-composition relation definitions
                     definitions-by-parent
                     transclusions-by-source
                     transclusions-by-target)
  composition?
  (relation composition-relation)
  (definitions composition-definitions)
  (definitions-by-parent composition-definitions-by-parent)
  (transclusions-by-source composition-transclusions-by-source)
  (transclusions-by-target composition-transclusions-by-target))

(define (make-composition definitions transclusions)
  (unless (and (list? definitions) (every subtree-definition? definitions))
    (error "composition definitions must be subtree definitions" definitions))
  (unless (and (list? transclusions) (every transclusion? transclusions))
    (error "composition transclusions must be transclusion records" transclusions))
  (set! definitions (snapshot-data definitions))
  (set! transclusions (snapshot-data transclusions))
  (unless (= (length definitions)
             (length (delete-duplicates
                       (map (lambda (definition)
                              (list (subtree-definition-parent definition)
                                    (subtree-definition-child definition)))
                            definitions)
                       equal?)))
    (error "composition contains duplicate subtree definitions" definitions))
  (unless (= (length transclusions)
             (length (delete-duplicates
                       (map (lambda (transclusion)
                              (list
                                (transclusion-source-tree-id transclusion)
                                (transclusion-source-id transclusion)))
                            transclusions)
                       equal?)))
    (error "composition contains duplicate transclusion occurrences"
           transclusions))
  (let* ((relation
          (make-composition-relation
            (append
              (map (lambda (definition)
                     (let ((parent (subtree-definition-parent definition))
                           (child (subtree-definition-child definition)))
                       (list "subtree" parent child child)))
                   definitions)
              (map (lambda (transclusion)
                     (list "transclude"
                           (transclusion-source-tree-id transclusion)
                           (transclusion-source-id transclusion)
                           (transclusion-target-tree-id transclusion)))
                   transclusions))))
         ;;     Composition accessors answer per-page queries during
         ;;     rendering, so the buckets are built once here instead of
         ;;     re-running the relation query for every page.
         (definitions-by-parent (make-hash-table))
         (transclusions-by-source (make-hash-table))
         (transclusions-by-target (make-hash-table)))
    ;;     Cons 把行倒序压进桶，所以从后往前遍历，桶即恢复
    ;;     occurrence order；不能用事后统一 reverse：eq? 去重不了
    ;;     相等但不同对象的字符串键，重复键会被 reverse 两次。
    (for-each
      (lambda (definition)
        (let ((parent (subtree-definition-parent definition)))
          (hash-set! definitions-by-parent parent
                     (cons definition
                           (hash-ref definitions-by-parent parent '())))))
      (reverse definitions))
    (for-each
      (lambda (transclusion)
        (let ((source (transclusion-source-tree-id transclusion))
              (target (transclusion-target-tree-id transclusion)))
          (hash-set! transclusions-by-source source
                     (cons transclusion
                           (hash-ref transclusions-by-source source '())))
          (hash-set! transclusions-by-target target
                     (cons transclusion
                           (hash-ref transclusions-by-target target '())))))
      (reverse transclusions))
    (%make-composition relation definitions
                       definitions-by-parent
                       transclusions-by-source
                       transclusions-by-target)))

(define (target-ids composition source-tree-id)
  ;;     Every tree reachable in one hop: defined subtrees first, then
  ;;     transclusion targets, matching the relation's row order.
  (append
    (map subtree-definition-child
         (hash-ref (composition-definitions-by-parent composition)
                   source-tree-id '()))
    (map transclusion-target-tree-id
         (hash-ref (composition-transclusions-by-source composition)
                   source-tree-id '()))))

(define (subtree-definitions composition)
  (snapshot-data (composition-definitions composition)))

(define (outgoing-transclusions composition source-tree-id)
  (snapshot-data
    (hash-ref (composition-transclusions-by-source composition)
              source-tree-id '())))

(define (incoming-transclusions composition target-tree-id)
  (snapshot-data
    (hash-ref (composition-transclusions-by-target composition)
              target-tree-id '())))
