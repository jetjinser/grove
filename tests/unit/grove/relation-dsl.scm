;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove relation-dsl)
  #:use-module (srfi srfi-64)
  #:use-module (grove relation dsl))

(define-relation-schema make-edge-relation edgeo (source target))
(define-relation-schema make-value-relation valueo (value))

(defrel (common-sourceo edges left right source)
  (fresh (left-target right-target)
    (edgeo edges (source source) (target left-target))
    (edgeo edges (source source) (target right-target))
    (== left-target left)
    (== right-target right)))

(defrel (either-targeto edges source target)
  (conde
    ((== target 'left)
     (edgeo edges (source source) (target target)))
    ((== target 'right)
     (edgeo edges (source source) (target target)))))

(defrel (long-stringo values value)
  (stringo value)
  (valueo values (value value))
  (project (value)
    (if (> (string-length value) 3)
        succeed
        fail)))

(test-begin "grove relation DSL")

(test-assert "relation DSL does not re-export relation indexes"
  (not (module-variable
         (module-public-interface (resolve-module '(grove relation dsl)))
         'relation-indexes)))
(test-assert "relation DSL does not shadow miniKanren with aliases"
  (let ((interface
          (module-public-interface (resolve-module '(grove relation dsl)))))
    (not (or (module-variable interface 'define-relation)
             (module-variable interface 'exists)
             (module-variable interface 'all)
             (module-variable interface 'either)
             (module-variable interface 'query)))))

(let ((edges
        (make-edge-relation
          '((first left)
            (first right)
            (second left)
            (third elsewhere)))))
  (test-equal "schema constructors retain declared arity when empty"
    2
    (relation-arity (make-edge-relation '())))
  (test-equal "named relations compose with fresh goals"
    '(first)
    (run* (source)
      (common-sourceo edges 'left 'right source)))
  (test-equal "conde branches preserve miniKanren answer order"
    '(left right)
    (run* (target)
      (either-targeto edges 'first target)))
  (test-equal "project runs host-language checks on ground terms"
    '("longer")
    (run* (value)
      (long-stringo (make-value-relation '(("few") ("longer"))) value))))

(let ((edges
        (make-edge-relation
          '((first left)
            (first right)
            (second left)))))
  (test-equal "omitted schema fields become independent anonymous variables"
    '(first first second)
    (run* (source)
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
