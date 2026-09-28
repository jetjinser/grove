;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove relation-dsl)
  #:use-module (srfi srfi-64)
  #:use-module (grove relation dsl))

(define-relation (edgeo edges source target)
  (relationo edges source target))

(define-relation (common-sourceo edges left right source)
  (exists (left-target right-target)
    (all
      (edgeo edges source left-target)
      (edgeo edges source right-target)
      (== left-target left)
      (== right-target right))))

(define-relation (either-targeto edges source target)
  (either
    ((all (== target 'left) (edgeo edges source target)))
    ((all (== target 'right) (edgeo edges source target)))))

(test-begin "grove relation DSL")

(let ((edges
        (make-relation
          '((first left)
            (first right)
            (second left)
            (third elsewhere)))))
  (test-equal "named relations compose with exists and all"
    '(first)
    (query (source)
      (common-sourceo edges 'left 'right source)))
  (test-equal "either branches preserve miniKanren answer order"
    '(left right)
    (query (target)
      (either-targeto edges 'first target))))

(test-end "grove relation DSL")
