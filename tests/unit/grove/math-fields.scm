;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math pipeline) (grove math types) (grove math expression)
             (grove math context) (grove math rewrite) (grove math premise)
             (grove math layout) (grove ir) (grove render html)
             ((grove math semantic) #:prefix ◆:)
             (srfi srfi-1) (srfi srfi-64))
(define r3 (◆:space 'x 'y 'z))
(define f (◆:field 'f #:space r3))
(define g (◆:field 'g #:space r3))
(define e (◆:field 'E #:space r3 #:result 'vector))
(define c1 (list r3 f g e "differentiable(f); differentiable(g); differentiable(E)"))
(define smooth (list r3 f g e "smooth(f); smooth(g); smooth(E)"))
(define fx (◆:call f "x" "y" "z"))
(define gx (◆:call g "x" "y" "z"))
(define ex (◆:call e "x" "y" "z"))

(test-begin "Cartesian scalar and vector fields")
(test-equal "field value is a function reference" 'function
  (math-typed-term-type (math-infer-type (math-assumptions c1) (math-value-term f))))
(test-equal "field applications provide their declared scalar type" 'scalar
  (math-typed-term-type (math-infer-type (math-assumptions c1) (math-value-term fx))))
(test-equal "field applications provide their declared vector type" 'vector
  (math-typed-term-type (math-infer-type (math-assumptions c1) (math-value-term ex))))
(test-equal "field declarations alone do not activate space" 'missing-premise
  (math-report-reason (math-simplify "f(x,y,z)"
     #:assumptions '("coordinate(x); coordinate(y); coordinate(z); field(f,scalar,x,y,z)"))))
(test-equal "wrong field arity is a type error" 'type-error
  (math-report-reason (math-simplify "f(x,y)" #:assumptions c1)))
(test-equal "a field reference cannot be used as a scalar value" 'type-error
  (math-report-reason (math-simplify "f+1" #:assumptions c1)))
(test-equal "Cartesian coordinate order cannot silently change" 'invalid-assumption
  (math-report-reason (math-simplify "1"
    #:assumptions (list r3 (◆:field 'h #:space (◆:space 'y 'x 'z))))))
(test-equal "a parameter is not a Cartesian coordinate" 'invalid-assumption
  (math-report-reason (math-simplify "1"
    #:assumptions '("parameter(a); coordinate(y); coordinate(z); space(a,y,z)"))))
(test-equal "duplicate axes are invalid" 'invalid-assumption
  (math-report-reason (math-simplify "1"
    #:assumptions '("coordinate(x); coordinate(z); space(x,x,z)"))))
(test-error "space construction does not reinterpret a parameter as a coordinate" #t
  (◆:space (◆:parameter 'a) 'y 'z))
(test-equal "conflicting field result types are invalid" 'invalid-assumption
  (math-report-reason (math-simplify "1"
    #:assumptions (list r3 f (◆:field 'f #:space r3 #:result 'vector)))))

(for-each
  (lambda (fixture)
    (let* ((report (math-rewrite (car fixture) #:assumptions c1 #:rules (list (caddr fixture))))
           (certificate (math-report-derivation report)))
      (test-equal "field product rule has the specified result"
        (math-source->term (cadr fixture)) (math-report-final report))
      (test-assert "each product rule independently replays"
        (math-verification-valid? (math-verify-derivation certificate)))
      (test-assert "generated field terms retain source provenance"
        (any math-origin-generated? (math-derivation-sources certificate)))
      (test-assert "authored field steps use the same rule"
        (math-verification-valid? (math-check-chain (take fixture 2) #:assumptions c1)))
      (test-assert "bounded target search finds the field product rule"
        (pair? (math-report-candidates (math-search (car fixture) (cadr fixture)
          #:assumptions c1 #:max-depth 1 #:rules (list (caddr fixture))))))
      (test-assert "missing C1 premises cannot authorize the product rule"
        (not (math-verification-valid? (math-check-chain (take fixture 2)
          #:assumptions (list r3 f g e)))))))
  '(("grad(f(x,y,z)*g(x,y,z))"
     "g(x,y,z)*grad(f(x,y,z))+f(x,y,z)*grad(g(x,y,z))" grad-product)
    ("div(f(x,y,z)*E(x,y,z))"
     "dot(grad(f(x,y,z)),E(x,y,z))+f(x,y,z)*div(E(x,y,z))" div-product)
    ("curl(f(x,y,z)*E(x,y,z))"
     "cross(grad(f(x,y,z)),E(x,y,z))+f(x,y,z)*curl(E(x,y,z))" curl-product)))
(test-assert "C1 does not imply the smoothness needed for second derivatives"
  (not (math-verification-valid? (math-check-chain
    '("curl(curl(E(x,y,z)))" "grad(div(E(x,y,z)))-laplacian(E(x,y,z))") #:assumptions c1))))
(test-assert "existing curl-curl identity applies to a declared smooth field"
  (math-verification-valid? (math-check-chain
    '("curl(curl(E(x,y,z)))" "grad(div(E(x,y,z)))-laplacian(E(x,y,z))") #:assumptions smooth)))
(test-equal "dot product has scalar result" 'scalar
  (math-typed-term-type (math-infer-type (math-assumptions c1)
    (math-value-term (◆:dot ex ex)))))
(test-equal "cross product has vector result" 'vector
  (math-typed-term-type (math-infer-type (math-assumptions c1)
    (math-value-term (◆:cross ex ex)))))
(test-equal "scalar multiplication never stands for a vector product" 'type-error
  (math-report-reason (math-simplify "E(x,y,z)*E(x,y,z)" #:assumptions c1)))
(test-equal "dot product rejects scalar inputs" 'type-error
  (math-report-reason (math-simplify "dot(f(x,y,z),E(x,y,z))" #:assumptions c1)))
(test-assert "cross-product orientation cannot be reversed in a claimed step"
  (not (math-verification-valid? (math-check-chain
    '("curl(f(x,y,z)*E(x,y,z))"
      "cross(E(x,y,z),grad(f(x,y,z)))+f(x,y,z)*curl(E(x,y,z))") #:assumptions c1))))
(test-assert "automatic derivation diagnoses missing C1 rather than a generic no-rule"
  (string-contains (sxml->html (content->sxml (math-derive "div(f(x,y,z)*E(x,y,z))"
    #:assumptions (list r3 f e) #:rules '(div-product) #:on-error 'show))) "differentiable"))
(test-assert "C1 inference propagates through field compositions"
  (math-premise-known? (math-assumptions c1)
    (math-source->term "differentiable(f(x^2,y,z)*g(x,y,z))")))
(test-assert "coordinates and parameters are distinct from fields"
  (math-premise-known? (math-assumptions (list r3 "parameter(a)"))
    (math-source->term "differentiable(a*x)")))
(test-equal "field division still checks the original real domain" 'missing-premise
  (math-report-reason (math-rewrite "grad(f(x,y,z)/x)" #:assumptions c1)))
(test-equal "field multiplication budget is enforced" 'step-limit
  (math-report-reason (math-rewrite "curl(f(x,y,z)*E(x,y,z))"
     #:assumptions c1 #:rules '(curl-product) #:max-steps 0)))
(test-assert "field layout accents vector-valued function applications"
  (string-contains (sxml->html (content->sxml
    (make-content-element 'math (list (math-value->layout ex (math-assumptions c1)))))) "<mover"))
(test-assert "field result requirements retain its spatial context"
  (member (math-value-term r3)
    (math-value-requirements (math-report-result (math-rewrite "div(f(x,y,z)*E(x,y,z))"
      #:assumptions c1 #:rules '(div-product))))))
(let ((result (math-report-result (math-rewrite "div(f(x,y,z)*E(x,y,z))"
                #:assumptions c1 #:rules '(div-product)))))
  (test-equal "reused field results retain the product rule's C1 premises" 'missing-premise
    (math-report-reason (math-simplify result #:assumptions (list r3 f e))))
  (test-assert "the original field premises authorize result reuse"
    (math-report-complete? (math-simplify result #:assumptions c1))))
(let* ((report (math-search "differentiate(0*f(x),x)" "0"
                 #:assumptions '("coordinate(x); function(f,scalar,scalar); smooth(f)")
                 #:rules '(differentiate-product differentiate-constant differentiate-function
                           differentiate-variable zero-multiply add-zero zero-add)
                 #:max-depth 7 #:max-results 12 #:max-states 256))
       (candidates (math-report-candidates report)))
  (test-assert "every search path retains the derivative's intrinsic C1 requirement"
    (and (pair? candidates)
         (every (lambda (candidate) (member '(differentiable (var f)) (math-derivation-requirements candidate))) candidates)))
  (test-assert "function differentiation does not promote C1 to smooth"
    (every (lambda (candidate) (not (member '(smooth (var f)) (math-derivation-requirements candidate)))) candidates))
  (test-assert "all paths with their own retained requirements replay"
    (every (lambda (candidate) (math-verification-valid? (math-verify-derivation candidate))) candidates)))
(test-end "Cartesian scalar and vector fields")
