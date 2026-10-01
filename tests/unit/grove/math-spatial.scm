(use-modules (grove math pipeline) (grove math rewrite) (grove math rule)
             (grove math context) (grove math types) (grove relation)
             (tests unit grove polynomial-oracle)
             ((grove math semantic) #:prefix ◆:) (srfi srfi-1) (srfi srfi-64))
(use-modules (tests unit grove cartesian-fixtures))
(test-begin "Cartesian spatial calculus")
(for-each check '(("grad(x^2+y^2+z^2)" "components(2x,2y,2z)")
                  ("div(components(x,y,z))" "3")
                  ("curl(components(-y,x,0))" "components(0,0,2)")))
(test-equal "missing basis is diagnosed" 'missing-premise
  (math-report-reason (math-compute "grad(x)" #:assumptions '("coordinate(x)"))))
(test-equal "missing regularity is diagnosed" 'missing-premise
  (math-report-reason (math-compute "grad(u)" #:assumptions (list basis "scalar(u)"))))
(test-equal "C1 alone cannot authorize a Laplacian" 'missing-premise
  (math-report-reason (math-compute "laplacian(u)" #:assumptions (list basis "scalar(u); differentiable(u)"))))
(test-equal "limits retain a valid prefix" 'step-limit
  (math-report-reason (math-compute "grad(x^2)" #:max-steps 0 #:assumptions (list basis))))
(test-equal "abstract vectors do not acquire invented components" 'unsupported-expression
  (math-report-reason (math-compute "div(E)" #:assumptions (list basis "vector(E); differentiable(E)"))))
(test-equal "another ordered basis supplies its own axes"
  (math-source->term "components(2u,2v,2w)")
  (math-report-final (math-compute "grad(u^2+v^2+w^2)" #:assumptions (list (◆:space 'u 'v 'w)))))
(let* ((report (math-compute "grad(a)" #:assumptions (list basis "parameter(a)")))
       (result (math-report-result report)))
  (test-equal "parameter gradient is zero" (math-source->term "components(0,0,0)") (math-report-final report))
  (test-equal "erased independence premises remain required" 'missing-premise
    (math-report-reason (math-simplify result #:assumptions (list basis "scalar(a); differentiable(a)")))))
(test-end "Cartesian spatial calculus")
