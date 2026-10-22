;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (html-rules?
            make-html-rules
            empty-html-rules
            compose-html-rules
            html-rules-ref
            html-rules->handlers
            define-html-rules))

;; A renderer rule is a value, not a callback registration.  A rule maps a
;; content kind to an interpretation:
;;
;;   (context element children next) -> sxml
;;
;; NEXT is a thunk that invokes the next interpretation for the same kind.
;; Rule sets compose by explicit layers; the overlay wins and the base remains
;; the fallback.  No global registry, no implicit invocation order.

(define-record-type <html-rules>
  (%make-html-rules rules)
  html-rules?
  (rules html-rules-map))

(define (valid-rule? entry)
  (and (pair? entry)
       (symbol? (car entry))
       (procedure? (cdr entry))))

(define (duplicate-kind? rules)
  (let loop ((remaining rules) (seen '()))
    (and (pair? remaining)
         (or (memq (caar remaining) seen)
             (loop (cdr remaining) (cons (caar remaining) seen))))))

(define (make-html-rules rules)
  "Build a rule set from an association list of kind -> interpreter."
  (unless (and (list? rules) (every valid-rule? rules))
    (error "HTML rules must be an association list of kind/procedure pairs"
           rules))
  (when (duplicate-kind? rules)
    (error "HTML rules cannot contain duplicate kinds" rules))
  (%make-html-rules rules))

(define empty-html-rules
  (make-html-rules '()))

(define (compose-html-rules base overlay)
  "Compose two rule sets; OVERLAY wins for a kind it interprets."
  (unless (and (html-rules? base) (html-rules? overlay))
    (error "HTML rules can only be composed from HTML rules" base overlay))
  (let ((overlay-rules (html-rules-map overlay)))
    (%make-html-rules
      (append
        (map (lambda (entry)
               (cons (car entry)
                     (let ((rule (cdr entry)))
                       (lambda (context element children next)
                         (rule context element children
                               (lambda ()
                                 (let ((fallback
                                         (html-rules-ref base (car entry))))
                                   (if fallback
                                       (fallback context element children next)
                                       (next)))))))))
             overlay-rules)
        (remove (lambda (entry)
                  (assq (car entry) overlay-rules))
                (html-rules-map base))))))

(define (html-rules-ref rules kind)
  "Return the interpreter for KIND, or #f."
  (unless (html-rules? rules)
    (error "not an HTML rule set" rules))
  (assq-ref (html-rules-map rules) kind))

(define* (html-rules->handlers rules #:key (context #f))
  "Adapt RULES to content->sxml handlers, closing over CONTEXT."
  (unless (html-rules? rules)
    (error "not an HTML rule set" rules))
  (map (lambda (entry)
         (cons (car entry)
               (lambda (element children)
                 ((cdr entry) context element children
                  (lambda ()
                    (error "no fallback HTML rule" (car entry)))))))
       (html-rules-map rules)))

(define-syntax define-html-rules
  (syntax-rules ()
    ((_ name (kind (argument ...) body ...) ...)
     (define name
       (make-html-rules
         (list (cons 'kind (lambda (argument ...) body ...)) ...))))))
