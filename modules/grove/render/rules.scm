;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render rules)
  #:use-module (grove render html rules)
  #:export (render-rules?
            make-render-rules
            empty-render-rules
            compose-render-rules
            render-rules-ref
            render-rules->handlers
            define-render-rules))

;; Output-agnostic names for the rule algebra.  The implementation is shared
;; with the HTML adapter; a rule receives (context element children next) and
;; returns whatever the target renderer expects.  Text/gemtext handlers use the
;; same shape, with CHILDREN already rendered as text.

(define render-rules? html-rules?)
(define make-render-rules make-html-rules)
(define empty-render-rules empty-html-rules)
(define compose-render-rules compose-html-rules)
(define render-rules-ref html-rules-ref)
(define render-rules->handlers html-rules->handlers)

(define-syntax-rule (define-render-rules name (kind (argument ...) body ...) ...)
  (define-html-rules name (kind (argument ...) body ...) ...))
