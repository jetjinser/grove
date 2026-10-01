;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (grove math pipeline) (grove math premise) (grove math context)
             ((grove math semantic) #:prefix ◆:) (srfi srfi-64))
(test-begin "finite regularity budgets")
(define r3 (◆:space 'x 'y 'z))
(define facts (math-assumptions (list r3 "scalar(u); c2(u)")))
(test-assert "C2 entails C1" (math-premise-known? facts '(differentiable (var u))))
(test-assert "C2 does not entail smoothness" (not (math-premise-known? facts '(smooth (var u)))))
(test-assert "C2 derivative is C1" (math-premise-known? facts '(differentiable (partial (var u) (var x)))))
(test-assert "C2 derivative is not C2" (not (math-premise-known? facts '(c2 (partial (var u) (var x))))))
(test-assert "C2 Laplacian is not C1" (not (math-premise-known? facts '(differentiable (laplacian (var u))))))
(let ((report (math-compute "laplacian(u)" #:assumptions (list r3 "scalar(u); c2(u)"))))
  (test-assert "C2 scalar field supports a symbolic Laplacian" (math-report-complete? report))
  (test-assert "C2 Laplacian certificate replays" (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
  (test-equal "computed second derivative retains C2 requirement" 'missing-premise
    (math-report-reason (math-simplify (math-report-result report) #:assumptions (list r3 "scalar(u); differentiable(u)")))))
(test-assert "C2 suffices for curl-curl identity"
  (math-report-complete? (math-rewrite "curl(curl(E))" #:assumptions '("vector(E); c2(E)") #:rules '(curl-curl-identity))))
(test-equal "C1 does not authorize a second derivative" 'missing-premise
  (math-report-reason (math-partial "partial(u,x)" "x" #:assumptions (list r3 "scalar(u); differentiable(u)"))))
(test-equal "C2 does not authorize a third derivative" 'missing-premise
  (math-report-reason (math-partial "partial(partial(u,x),x)" "x" #:assumptions (list r3 "scalar(u); c2(u)"))))
(test-end "finite regularity budgets")
