;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (grove math catalog) (grove math rewrite) (grove math rule)
             (grove relation dsl) (grove render html)
             (srfi srfi-1) (srfi srfi-13) (srfi srfi-64))
(test-begin "inference rule catalog")
(define (html name) (sxml->html (content->sxml (math-rule->content name))))
(test-equal "enumeration preserves the complete registry"
  (map math-rewrite-rule-name math-rewrite-rules)
  (map car (query (name group modes) (ruleo name group modes))))
(test-equal "group filtering queries actual registry facts" 8
  (length (query (name modes) (ruleo name 'vector-calculus modes))))
(test-equal "unknown names have no catalog answers" '()
  (query (group modes) (ruleo 'missing-rule group modes)))
(test-equal "schema retains premises and executable type guards"
  '(((differentiable (unquote f)) (differentiable (unquote e)))
    ((math-typeo facts f 'scalar) (math-typeo facts e 'vector)))
  (cddr (math-rewrite-rule-schema (math-rewrite-rule-ref 'curl-product))))
(test-assert "every registered rule renders a native inference fraction"
  (every (lambda (rule) (string-contains (html (math-rewrite-rule-name rule)) "<mfrac>"))
         math-rewrite-rules))
(test-assert "computed rules disclose their algorithmic nature"
  (string-contains (html 'normalize-scalar) "not a fixed rewrite schema"))
(test-assert "power rules expose the guard which computes their result"
  (and (string-contains (html 'differentiate-power) "Executable guards")
       (string-contains (html 'differentiate-power) "natural-value")))
(test-assert "vector rules display mathematical premises and types"
  (and (string-contains (html 'curl-product) "differentiable")
       (string-contains (html 'curl-product) "vector")
       (string-contains (html 'curl-product) "→")))
(test-error "unknown rule rendering is rejected" #t (math-rule->content 'missing-rule))
(test-end "inference rule catalog")
