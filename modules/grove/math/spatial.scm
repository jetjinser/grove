;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (grove math spatial)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (srfi srfi-1)
  #:use-module (grove math rule)
  #:use-module (grove math types)
  #:export (math-spatial-rules))

;; Axes come from the active ordered basis, never from identifier spelling.
;; The context pass retains this basis on results even when all entries vanish.
(define (basiso facts x y z)
  (project (facts)
    (let ((basis (find (lambda (fact) (eq? (car fact) 'space)) facts)))
      (if basis (== `(space ,x ,y ,z) basis) fail))))

(define math-spatial-rules
  (list
    (math-rule 'gradient-components "grad(f) = (partial(f,x),partial(f,y),partial(f,z))"
      "C1 scalar field in an ordered Cartesian basis" 'spatial
      (facts f x y z) (grad ,f)
      (components (partial ,f ,x) (partial ,f ,y) (partial ,f ,z))
      ((differentiable ,f))
      ((basiso facts x y z) (math-typeo facts f 'scalar)))
    (math-rule 'divergence-components "div((a,b,c)) = partial(a,x)+partial(b,y)+partial(c,z)"
      "C1 components in an ordered Cartesian basis" 'spatial
      (facts a b c x y z) (div (components ,a ,b ,c))
      (+ (+ (partial ,a ,x) (partial ,b ,y)) (partial ,c ,z))
      ((differentiable (components ,a ,b ,c)))
      ((basiso facts x y z)))
    (math-rule 'curl-components "curl((a,b,c)) = (partial(c,y)-partial(b,z),partial(a,z)-partial(c,x),partial(b,x)-partial(a,y))"
      "C1 components in an oriented ordered Cartesian basis" 'spatial
      (facts a b c x y z) (curl (components ,a ,b ,c))
      (components (- (partial ,c ,y) (partial ,b ,z))
                  (- (partial ,a ,z) (partial ,c ,x))
                  (- (partial ,b ,x) (partial ,a ,y)))
      ((differentiable (components ,a ,b ,c)))
      ((basiso facts x y z)))
    (math-rule 'laplacian-scalar-components "laplacian(f) = div(grad(f))"
      "C2 scalar field in Cartesian space" 'spatial
      (facts f) (laplacian ,f) (div (grad ,f))
      ((c2 ,f)) ((math-typeo facts f 'scalar)))
    (math-rule 'laplacian-vector-components "laplacian((a,b,c)) = (laplacian(a),laplacian(b),laplacian(c))"
      "C2 Cartesian components" 'spatial
      (facts a b c) (laplacian (components ,a ,b ,c))
      (components (laplacian ,a) (laplacian ,b) (laplacian ,c))
      ((c2 (components ,a ,b ,c))) ())))
