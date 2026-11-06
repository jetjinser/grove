;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (grove math rewrite) (grove math types)
             (grove math context)
             (grove math semantic-check) (grove math term)
             (grove relation) (tests unit grove cartesian-fixtures)
             (tests unit grove polynomial-oracle)
             (srfi srfi-1) (srfi srfi-64))
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

;; Exercise the independent Cartesian checker against actual Grove calculations
;; and the structurally separate sparse-polynomial oracle.  One mixed-sign
;; fixture per operation keeps this end-to-end cross-check independent without
;; repeating the same expensive calculation over several coefficient variants.

(for-each
  (lambda (source)
    (let* ((report (φ:compute source #:assumptions (list basis)))
           (result (math-report-final report))
           (oracle (make-evaluator coordinates))
           (semantic (φ:check-polynomial-cartesian source (math-term->source result))))
      (test-assert (format #f "Grove computes the cross-check fixture: ~a" source)
        (math-report-finished? report))
      (test-equal (format #f "oracle validates Grove result: ~a" source)
        (oracle (φ:term source)) (oracle result))
      (test-eq (format #f "semantic checker independently validates Grove result: ~a" source)
        'semantically-checked (math-semantic-check-status semantic))))
  '("grad(x^2+2*y*z-x*y)"
    "div(components(x^2*y,2*y^2*z,-z^2*x))"
    "curl(components(x*y+z^2,2*y*z+x^2,-z*x+y^2))"
    "dot(components(x,y,z),components(1,2,3))"
    "cross(components(x,2*y,z),components(z,x,3*y))"
    "laplacian(x^2*y^2+3*x*z^3)"
    "laplacian(components(x^3,2*y^3,-z^3))"))

(for-each
  (lambda (entry)
    (let ((check (φ:check-polynomial-cartesian (car entry) (cadr entry))))
      (test-eq (format #f "Cartesian identity is checked independently: ~a" (car entry))
        'semantically-checked (math-semantic-check-status check))
      (test-equal (format #f "oracle agrees on Cartesian identity: ~a" (car entry))
        (evaluate (φ:term (car entry)) coordinates)
        (evaluate (φ:term (cadr entry)) coordinates))))
  '(("curl(grad(x^2*y+y^2*z+z^2*x))" "components(0,0,0)")
    ("div(curl(components(x^2*y,y^2*z,z^2*x)))" "0")
    ("div(grad(x^2*y^2+z^3))" "laplacian(x^2*y^2+z^3)")))

(test-eq "Cartesian checker follows a reordered coordinate basis"
  'semantically-checked
  (math-semantic-check-status
    (φ:check-polynomial-cartesian "grad(x+2*y+3*z)" "components(3,1,2)"
      #:axes '(z x y))))
(test-eq "Cartesian checker rejects canonical-order output for a reordered basis"
  'rejected
  (math-semantic-check-status
    (φ:check-polynomial-cartesian "grad(x+2*y+3*z)" "components(1,2,3)"
      #:axes '(z x y))))

(let* ((typed-facts
         (math-assumptions
           (append (list basis)
             (φ:assumptions "scalar(u); differentiable(u); function(f,scalar,scalar); c2(f)"))))
       (typed-sources '("u+x" "u*x" "partial(u,x)" "differentiate(x^3,x)"
                        "prime(f,x)" "laplacian(x^3)" "curl(zero(vector))")))
  (for-each
    (lambda (source)
      (let* ((term (φ:term source))
             (relational
               (delete-duplicates
                 (run* (type) (math-typeo typed-facts term type))))
             (ground (math-infer-type typed-facts term)))
        (test-equal "memoized ground and relational typing stay in parity"
          relational (if ground (list (math-typed-term-type ground)) '()))))
    typed-sources))
(test-end "independent Cartesian rule coverage")
