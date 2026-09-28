;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove relation-dsl)
  #:use-module (srfi srfi-64)
  #:use-module (grove relation dsl))

(define-relation-schema make-edge-relation edgeo (source target))

(define-relation (common-sourceo edges left right source)
  (exists (left-target right-target)
    (all
      (edgeo edges (source source) (target left-target))
      (edgeo edges (source source) (target right-target))
      (== left-target left)
      (== right-target right))))

(define-relation (either-targeto edges source target)
  (either
    ((all (== target 'left)
          (edgeo edges (source source) (target target))))
    ((all (== target 'right)
          (edgeo edges (source source) (target target))))))

(test-begin "grove relation DSL")

(let ((edges
        (make-edge-relation
          '((first left)
            (first right)
            (second left)
            (third elsewhere)))))
  (test-equal "schema constructors retain declared arity when empty"
    2
    (relation-arity (make-edge-relation '())))
  (test-equal "named relations compose with exists and all"
    '(first)
    (query (source)
      (common-sourceo edges 'left 'right source)))
  (test-equal "either branches preserve miniKanren answer order"
    '(left right)
    (query (target)
      (either-targeto edges 'first target))))

(let ((edges
        (make-edge-relation
          '((first left)
            (first right)
            (second left)))))
  (test-equal "omitted schema fields become independent anonymous variables"
    '(first first second)
    (query (source)
      (edgeo edges (source source))))
  (test-error "unknown schema fields are rejected during expansion"
    #t
    (eval '(edgeo edges (missing target)) (current-module)))
  (test-error "duplicate schema fields are rejected during expansion"
    #t
    (eval '(edgeo edges (source left) (source right))
          (current-module)))
  (test-error "duplicate schema columns are rejected during declaration"
    #t
    (eval '(define-relation-schema make-bad-edge-relation bad-edgeo
            (source source))
          (current-module)))
  (test-error "schema constructors reject rows with the wrong column count"
    #t
    (make-edge-relation '((one two three)))))

(test-end "grove relation DSL")
