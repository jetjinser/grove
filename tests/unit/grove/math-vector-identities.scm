;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (grove math expression) (grove math layout)
             ((grove math semantic) #:prefix ◆:) (grove ir) (grove render html)
             (tests unit grove polynomial-oracle) (srfi srfi-1) (srfi srfi-64))
(test-begin "component vector identity certificates")
(define basis (◆:space 'x 'y 'z))
(for-each
  (lambda (source)
    (let ((report (φ:check-equation source "components(0,0,0)"
                   #:assumptions (list basis "scalar(u); scalar(v); scalar(w); c2(u); c2(v); c2(w)"))))
      (test-assert "component calculus establishes the vector identity" (math-report-complete? report))
      (test-assert "component identity certificate replays"
        (math-verification-valid? (math-verify-derivation (math-report-derivation report))))))
  '("curl(grad(u))"
    "grad(div(components(u,v,w)))-laplacian(components(u,v,w))-curl(curl(components(u,v,w)))"))
(test-end "component vector identity certificates")
