;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math catalog) (grove math rewrite) (grove math rule)
             (grove relation dsl) (grove render html)
             (srfi srfi-1) (srfi srfi-13) (srfi srfi-64))
(test-begin "inference rule catalog")
(define (html name) (sxml->html (content->sxml (math-rule->content name))))
(test-equal "enumeration preserves the complete registry"
  (map math-rewrite-rule-name math-rewrite-rules)
  (map car (query (name group modes documentation) (ruleo name group modes documentation))))
(test-equal "group filtering queries actual registry facts" 8
  (length (query (name modes documentation) (ruleo name 'vector-calculus modes documentation))))
(test-equal "mode filtering sees every checkable rule"
  (length math-rewrite-rules)
  (length (query (name) (rule-modeo name 'check))))
(test-equal "operation filtering uses the input side of declared schemas"
  '(curl-curl-identity div-curl-zero curl-grad-zero curl-product curl-components)
  (query (name) (rule-operationo name 'curl)))
(test-equal "premise filtering returns the actual declared premise"
  '((curl-product (differentiable (unquote f)))
    (curl-product (differentiable (unquote e))))
  (query (name premise) (rule-premiseo name 'differentiable premise)
         (== name 'curl-product)))
(test-equal "algorithm rules without fixed schemas are absent from pattern queries" '()
  (query (name) (rule-operationo name 'differentiate)
         (== name 'differentiate-polynomial)))
(test-equal "unknown names have no catalog answers" '()
  (query (group modes documentation) (ruleo 'missing-rule group modes documentation)))
(test-equal "the catalog distinguishes checker-covered laws from algorithms"
  '(semantically-checked unsupported)
  (list (car (query (status) (rule-semantic-checko 'add-commutative status)))
        (car (query (status) (rule-semantic-checko 'differentiate-power status)))))
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
(test-assert "rule pages state the limited semantic-check coverage"
  (and (string-contains (html 'add-commutative)
                        "Scalar-polynomial semantic check: semantically-checked")
       (string-contains (html 'differentiate-power)
                        "Scalar-polynomial semantic check: unsupported")))
(test-error "unknown rule rendering is rejected" #t (math-rule->content 'missing-rule))
(test-equal "rule documentation is the execution procedure's native docstring"
  (procedure-documentation
    (math-rewrite-rule-relation (math-rewrite-rule-ref 'curl-product) 'forward))
  (car (query (documentation)
         (fresh (group modes) (ruleo 'curl-product group modes documentation)))))
(test-equal "documentation queries enumerate only documented rules" 7
  (length (query (name documentation)
            (fresh (group modes) (ruleo name group modes documentation))
            (=/= documentation #f))))
(test-equal "undocumented rules remain queryable with #f documentation" '(#f)
  (query (documentation)
    (fresh (group modes) (ruleo 'add-zero group modes documentation))))
(test-assert "explanations appear alongside inference rules"
  (string-contains (html 'curl-product) "reversing it changes the sign"))
(test-equal "native lambda docstrings need no separate metadata"
  "Native documentation."
  (math-rewrite-rule-documentation
    (make-math-rewrite-rule 'documented "X = X" "none" 'identity
      (lambda (facts input output) "Native documentation." #t)
      #:matcher (lambda (input) '()))))
(test-end "inference rule catalog")
