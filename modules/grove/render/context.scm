;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render context)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (render-context?
            make-render-context
            render-context-ref
            render-context-has?
            render-context-with
            render-context-without))

;; A render context is an explicit, open-ended set of values passed to
;; interpretations. Projects own their keys; Grove only provides immutable
;; association and overlay operations.
;;
;; The alist stores every key that was provided, including #f.  Absence and a
;; false value are distinct: use RENDER-CONTEXT-HAS? to tell them apart and
;; RENDER-CONTEXT-WITHOUT to remove a key.

(define-record-type <render-context>
  (%make-render-context values)
  render-context?
  (values render-context-values))

(define (keyword-plist->alist entries who)
  "Turn keyword/value ENTRIES into an alist; a repeated key keeps its last
value."
  (let loop ((remaining entries) (values '()))
    (cond
      ((null? remaining) (reverse values))
      ((keyword? (car remaining))
       (unless (pair? (cdr remaining))
         (error "~a expects keyword/value pairs" who entries))
       (let ((key (keyword->symbol (car remaining)))
             (value (cadr remaining)))
         (loop (cddr remaining)
               (cons (cons key value)
                     (remove (lambda (entry) (eq? (car entry) key))
                             values)))))
      (else (error "~a expects keyword/value pairs" who entries)))))

(define* (make-render-context #:rest entries)
  "Build a context from keyword/value ENTRIES."
  (%make-render-context
    (snapshot-data (keyword-plist->alist entries 'make-render-context))))

(define (render-context-ref context key)
  (unless (render-context? context)
    (error "not a render context" context))
  (unless (symbol? key)
    (error "render context keys must be symbols" key))
  (snapshot-data (assq-ref (render-context-values context) key)))

(define (render-context-has? context key)
  "Return #t when KEY is present in CONTEXT, even if its value is #f."
  (unless (render-context? context)
    (error "not a render context" context))
  (unless (symbol? key)
    (error "render context keys must be symbols" key))
  (and (assq key (render-context-values context)) #t))

(define* (render-context-with context #:rest entries)
  "Return CONTEXT with ENTRIES (a keyword plist) overlaid.

Every supplied value, including #f, replaces the inherited value.  Omit a key
to inherit it, or use RENDER-CONTEXT-WITHOUT to remove it."
  (unless (render-context? context)
    (error "not a render context" context))
  (let ((overlay (keyword-plist->alist entries 'render-context-with)))
    (%make-render-context
      (snapshot-data
        (append overlay
                (remove (lambda (entry)
                          (assq (car entry) overlay))
                        (render-context-values context)))))))

(define (render-context-without context . keys)
  "Return CONTEXT without KEYS.  Removing a key makes RENDER-CONTEXT-REF
return #f for it."
  (unless (render-context? context)
    (error "not a render context" context))
  (unless (every symbol? keys)
    (error "render-context-without expects symbol keys" keys))
  (%make-render-context
    (remove (lambda (entry) (memq (car entry) keys))
            (render-context-values context))))
