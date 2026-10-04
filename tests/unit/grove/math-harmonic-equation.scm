;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(use-modules (tests unit grove cartesian-fixtures) (srfi srfi-64))
(test-begin "harmonic addition preserves Poisson residual")
(check-equation '("laplacian(x^2*y^2/2-x^4/12+y*z)" "y^2"))
(test-end "harmonic addition preserves Poisson residual")
