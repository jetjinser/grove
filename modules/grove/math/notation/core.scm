;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math notation core)
  #:use-module (grove ir))

;; Pure notation constructors consume layout children. Text decoding and
;; semantic embedding belong to the author-facing adapter, not this layer.
(define-public (token kind value)
  (make-content-element 'math-token (list value)
                        #:properties `((token-kind . ,kind))))
(define-public (row children)
  (make-content-element 'math-row children))
(define-public (frac numerator denominator)
  (make-content-element 'math-frac (list (row numerator) (row denominator))))
(define-public (sup base exponent)
  (make-content-element 'math-sup (list (row base) (row exponent))))
(define-public (sub base index)
  (make-content-element 'math-sub (list (row base) (row index))))
(define-public (vec children)
  (make-content-element 'math-vector children))
(define-public (parens children)
  (make-content-element 'math-parens children))
(define-public (style children variant)
  (make-content-element 'math-style children #:properties `((variant . ,variant))))
(define-public (cell children)
  (make-content-element 'math-cell children))
(define-public (align rows)
  (make-content-element 'math-align rows #:flow-kind 'block))
(define-public (math children)
  (make-content-element 'math children))
(define-public (display-math children)
  (make-content-element 'math children #:properties '((display? . #t))
                        #:flow-kind 'block))
