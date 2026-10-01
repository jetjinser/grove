;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math rewrite) (grove math types)
             (grove relation) (tests unit grove cartesian-fixtures)
             (tests unit grove polynomial-oracle) (srfi srfi-1) (srfi srfi-64))
(test-begin "independent Cartesian rule coverage")
(define rules (math-rule-names #:groups '(components spatial)))
(define covered '())
(for-each
  (lambda (source)
    (let ((term (φ:term source)))
      (for-each
        (lambda (answer)
          (set! covered (cons (cadr answer) covered))
          (test-equal "every new relation preserves independent polynomial semantics"
            (evaluate term coordinates) (evaluate (car answer) coordinates)))
        (math-rewrite-answers facts term rules #f))
      (test-equal "bottom-up ground typing agrees with relational typing"
        (delete-duplicates (run* (type) (math-typeo facts term type)))
        (list (math-typed-term-type (math-infer-type facts term))))))
  '("components(x,y,z)+components(z,x,y)" "components(x,y,z)-components(y,z,x)"
    "-components(x,y,z)" "x*components(x,y,z)" "components(x,y,z)*x"
    "components(x,y,z)/2" "dot(components(x,y,z),components(z,x,y))"
    "cross(components(x,y,z),components(z,x,y))" "zero(vector)"
    "grad(x^2+y*z)" "div(components(x^2,y^2,z^2))"
    "curl(components(x*y,y*z,z*x))" "laplacian(x^2*y^2)"
    "laplacian(components(x^2,y^2,z^2))"))
(for-each (lambda (rule) (test-assert (format #f "independent coverage: ~a" rule) (memq rule covered))) rules)
(test-end "independent Cartesian rule coverage")
