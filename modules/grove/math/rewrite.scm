;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:export (curl-curlo
            rewrite-curl-curl))

;; Vector-calculus identity: curl(curl(E)) = grad(div(E)) - laplacian(E).
(define (curl-curlo input output)
  (fresh (field)
    (== input `(curl (curl ,field)))
    (== output `(- (grad (div ,field)) (laplacian ,field)))))

(define (rewrite-curl-curl term)
  (let ((answers (run 1 (result) (curl-curlo term result))))
    (and (pair? answers) (car answers))))
