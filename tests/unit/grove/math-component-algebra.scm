;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math rewrite) (grove math rule)
             (grove math context) (grove math types) (grove relation)
             (tests unit grove polynomial-oracle)
             ((grove math semantic) #:prefix ◆:) (srfi srfi-1) (srfi srfi-64))
(use-modules (tests unit grove cartesian-fixtures))
(test-begin "Cartesian component algebra")
(for-each check
  '(("components(x,y,z)+components(z,x,y)" "components(x+z,x+y,y+z)")
    ("components(x,y,z)-components(x,y,z)" "components(0,0,0)")
    ("-components(x,y,z)" "components(-x,-y,-z)")
    ("2*components(x,y,z)" "components(2x,2y,2z)")
    ("components(x,y,z)*2" "components(2x,2y,2z)")
    ("components(2x,2y,2z)/2" "components(x,y,z)")
    ("dot(components(x,y,z),components(x,y,z))" "x^2+y^2+z^2")
    ("cross(components(1,0,0),components(0,1,0))" "components(0,0,1)")))
(test-equal "vector denominator domain is checked" 'missing-premise
  (math-report-reason (φ:compute "components(x,y,z)/x" #:assumptions (list basis))))
(test-end "Cartesian component algebra")
