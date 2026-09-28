;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private minikanren)
  #:export (logic-variable?
            with-projected-terms))

;; Keep the dependency on faster-minikanren's unexported implementation
;; details in one adapter module.
(define (logic-variable? term)
  ((@@ (faster-minikanren mk-guile) var?) term))

(define-syntax with-projected-terms
  (syntax-rules ()
    ((_ (term ...) body ...)
     ((@@ (faster-minikanren mk-guile) project) (term ...) body ...))))
