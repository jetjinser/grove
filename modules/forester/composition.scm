;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester composition)
  #:use-module (grove relation dsl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (<subtree-definition>
            subtree-definition?
            make-subtree-definition
            subtree-definition-parent
            subtree-definition-child
            <forest-transclusion>
            forest-transclusion?
            make-forest-transclusion
            forest-transclusion-source-tree-id
            forest-transclusion-source-id
            forest-transclusion-target-tree-id
            forest-composition?
            make-forest-composition
            forest-composition-target-ids
            forest-composition-subtree-definitions
            forest-composition-outgoing-transclusions
            forest-composition-incoming-transclusions))

(define-record-type <subtree-definition>
  (%make-subtree-definition parent child)
  subtree-definition?
  (parent subtree-definition-parent)
  (child subtree-definition-child))

(define-record-type <forest-transclusion>
  (%make-forest-transclusion source-tree-id source-id target-tree-id)
  forest-transclusion?
  (source-tree-id forest-transclusion-source-tree-id)
  (source-id forest-transclusion-source-id)
  (target-tree-id forest-transclusion-target-tree-id))

(define (non-empty-string? value)
  (and (string? value) (not (string-null? value))))

(define (make-subtree-definition parent child)
  (unless (and (non-empty-string? parent) (non-empty-string? child))
    (error "subtree definition requires non-empty tree IDs" parent child))
  (%make-subtree-definition parent child))

(define (make-forest-transclusion source-tree-id source-id target-tree-id)
  (unless (and (non-empty-string? source-tree-id)
               (non-empty-string? source-id)
               (non-empty-string? target-tree-id))
    (error "transclusion requires non-empty tree and occurrence IDs"
           source-tree-id source-id target-tree-id))
  (%make-forest-transclusion source-tree-id source-id target-tree-id))

(define-relation-schema make-composition-relation compositiono
  (kind source occurrence target))

(define-record-type <forest-composition>
  (%make-forest-composition relation definitions transclusions)
  forest-composition?
  (relation composition-relation)
  (definitions composition-definitions)
  (transclusions composition-transclusions))

(define (make-forest-composition definitions transclusions)
  (unless (and (list? definitions) (every subtree-definition? definitions))
    (error "composition definitions must be subtree definitions" definitions))
  (unless (and (list? transclusions) (every forest-transclusion? transclusions))
    (error "composition transclusions must be transclusion records"
           transclusions))
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
                                (forest-transclusion-source-tree-id transclusion)
                                (forest-transclusion-source-id transclusion)))
                            transclusions)
                       equal?)))
    (error "composition contains duplicate transclusion occurrences"
           transclusions))
  (let ((relation
          (make-composition-relation
            (append
              (map (lambda (definition)
                     (let ((parent (subtree-definition-parent definition))
                           (child (subtree-definition-child definition)))
                       (list "subtree" parent child child)))
                   definitions)
              (map (lambda (transclusion)
                     (list "transclude"
                           (forest-transclusion-source-tree-id transclusion)
                           (forest-transclusion-source-id transclusion)
                           (forest-transclusion-target-tree-id transclusion)))
                   transclusions)))))
    (%make-forest-composition relation definitions transclusions)))

(define (forest-composition-target-ids composition source-tree-id)
  (map caddr
       (query (kind occurrence target)
         (compositiono (composition-relation composition)
                       (source source-tree-id)
                       (kind kind)
                       (occurrence occurrence)
                       (target target)))))

(define (forest-composition-subtree-definitions composition)
  (let ((keys
          (query (parent child)
            (compositiono (composition-relation composition)
                          (kind "subtree")
                          (source parent)
                          (target child)))))
    (filter (lambda (definition)
              (member (list (subtree-definition-parent definition)
                            (subtree-definition-child definition))
                      keys equal?))
            (composition-definitions composition))))

(define (forest-composition-outgoing-transclusions composition source-tree-id)
  (let ((occurrences
          (query (occurrence)
            (compositiono (composition-relation composition)
                          (kind "transclude")
                          (source source-tree-id)
                          (occurrence occurrence)))))
    (filter (lambda (transclusion)
              (and (equal? (forest-transclusion-source-tree-id transclusion)
                           source-tree-id)
                   (member (forest-transclusion-source-id transclusion)
                           occurrences equal?)))
            (composition-transclusions composition))))

(define (forest-composition-incoming-transclusions composition target-tree-id)
  (let ((keys
          (query (source occurrence)
            (compositiono (composition-relation composition)
                          (kind "transclude")
                          (source source)
                          (occurrence occurrence)
                          (target target-tree-id)))))
    (filter (lambda (transclusion)
              (member (list
                        (forest-transclusion-source-tree-id transclusion)
                        (forest-transclusion-source-id transclusion))
                      keys equal?))
            (composition-transclusions composition))))
