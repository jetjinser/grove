;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private procedure)
  #:use-module (ice-9 optargs)
  #:export (procedure-accepts-arguments?))

(define (procedure-accepts-arguments? value count)
  "Return true when VALUE can be called with COUNT positional arguments."
  (and (procedure? value)
       (integer? count)
       (>= count 0)
       (let ((arity
               (catch #t
                 (lambda () (procedure-minimum-arity value))
                 (lambda _ #f))))
         (and (list? arity)
              (= (length arity) 3)
              (let ((required (car arity))
                    (optional (cadr arity))
                    (rest (caddr arity)))
                (and (<= required count)
                     (or rest (>= (+ required optional) count))))))))
