;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (grove private snapshot)
  #:export (render-rules?
            make-render-rules
            empty-render-rules
            compose-render-rules
            render-rules-ref
            define-render-rules))

;; A renderer rule is a value, not a callback registration.  A rule maps a
;; content kind to an interpretation:
;;
;;   (context element children next) -> value
;;
;; The value is whatever the target renderer expects: sxml for HTML, a string
;; for text and gemtext.  NEXT is a thunk that invokes the next interpretation
;; for the same kind.  Rule sets compose by explicit layers; the overlay wins
;; and the base remains the fallback.  No global registry, no implicit
;; invocation order.

(define-record-type <render-rules>
  (%make-render-rules rules)
  render-rules?
  (rules render-rules-map))

(define (valid-rule? entry)
  (and (pair? entry)
       (symbol? (car entry))
       (procedure? (cdr entry))))

(define (duplicate-kind? rules)
  (let loop ((remaining rules) (seen '()))
    (and (pair? remaining)
         (or (memq (caar remaining) seen)
             (loop (cdr remaining) (cons (caar remaining) seen))))))

(define (make-render-rules rules)
  "Build a rule set from an association list of kind -> interpreter."
  (unless (and (list? rules) (every valid-rule? rules))
    (error "render rules must be an association list of kind/procedure pairs"
           rules))
  (when (duplicate-kind? rules)
    (error "render rules cannot contain duplicate kinds" rules))
  (%make-render-rules (snapshot-data rules)))

(define empty-render-rules
  (make-render-rules '()))

(define (compose-render-rules base overlay)
  "Compose two rule sets; OVERLAY wins for a kind it interprets."
  (unless (and (render-rules? base) (render-rules? overlay))
    (error "render rules can only be composed from render rules" base overlay))
  (let ((overlay-rules (render-rules-map overlay)))
    (%make-render-rules
      (append
        (map (lambda (entry)
               (cons (car entry)
                     (let ((rule (cdr entry)))
                       (lambda (context element children next)
                         (rule context element children
                               (lambda ()
                                 (let ((fallback
                                         (render-rules-ref base (car entry))))
                                   (if fallback
                                       (fallback context element children next)
                                       (next)))))))))
             overlay-rules)
        (remove (lambda (entry)
                  (assq (car entry) overlay-rules))
                (render-rules-map base))))))

(define (render-rules-ref rules kind)
  "Return the interpreter for KIND, or #f."
  (unless (render-rules? rules)
    (error "not a render rule set" rules))
  (assq-ref (render-rules-map rules) kind))

(define-syntax define-render-rules
  (syntax-rules ()
    ((_ name (kind (argument ...) body ...) ...)
     (define name
       (make-render-rules
         (list (cons 'kind (lambda (argument ...) body ...)) ...))))))
