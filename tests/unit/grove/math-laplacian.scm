(use-modules (grove math calculation) (grove math rewrite) (grove math rule)
             (grove math context) (grove math types) (grove relation)
             (tests unit grove polynomial-oracle)
             ((grove math semantic) #:prefix ◆:) (srfi srfi-1) (srfi srfi-64))
;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (tests unit grove cartesian-fixtures))
(test-begin "Cartesian Laplacian oracle")
(for-each check '(("laplacian(x^2+y^2+z^2)" "6")
                  ("laplacian(components(x^2,y^2,z^2))" "components(2,2,2)")))
(test-end "Cartesian Laplacian oracle")
