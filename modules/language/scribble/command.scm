;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble command)
  #:use-module (ice-9 match)
  #:export (command-body))

(define (command-body arguments)
  "Remove lambda* keyword/value pairs from its rest argument, preserving body order."
  (let loop ((arguments arguments) (body '()))
    (match arguments
      (() (reverse body))
      (((? keyword? key) value . rest) (loop rest body))
      (((? keyword? key)) (error "command keyword requires a value" key))
      ((value . rest) (loop rest (cons value body))))))
