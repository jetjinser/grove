;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render context)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (render-context?
            make-render-context
            render-context-ref
            render-context-has?
            render-context-with
            render-context-without
            render-context-values
            render-context-forest
            render-context-routes
            render-context-page
            render-context-anchor-prefix
            render-context-site-url
            render-context-default-locale
            render-context-hreflang?
            render-context-language-switcher?
            render-context-translation-groups
            render-context-og-image
            render-context-og-image-alt
            render-context-locale
            render-context-locale-object
            render-context-messages))

;; A render context is an explicit value passed to interpretations.  It is a
;; record over an association list so projects can add namespaced keys without
;; changing the core record, while the common keys stay documented here:
;; forest, routes, page, anchor-prefix, site-url, default-locale, hreflang?,
;; language-switcher?, translation-groups, og-image, og-image-alt, locale,
;; locale-object, messages.
;;
;; The alist stores every key that was provided, including #f.  Absence and a
;; false value are distinct: use RENDER-CONTEXT-HAS? to tell them apart and
;; RENDER-CONTEXT-WITHOUT to remove a key.

(define-record-type <render-context>
  (%make-render-context values)
  render-context?
  (values render-context-values))

(define default-render-context-values
  '((hreflang? . #t)
    (language-switcher? . #t)))

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
  "Build a context from keyword/value ENTRIES.

Known keys get their defaults; unknown keys are kept so projects can add
namespaced data without changing the record."
  (let ((provided (keyword-plist->alist entries 'make-render-context)))
    (%make-render-context
      (append provided
              (filter (lambda (entry)
                        (not (assq (car entry) provided)))
                      default-render-context-values)))))

(define (render-context-ref context key)
  (unless (render-context? context)
    (error "not a render context" context))
  (unless (symbol? key)
    (error "render context keys must be symbols" key))
  (assq-ref (render-context-values context) key))

;; Typed accessors for the documented keys.  RENDER-CONTEXT-REF remains the
;; escape hatch for project-defined namespaced keys.

(define (render-context-forest context)
  (render-context-ref context 'forest))

(define (render-context-routes context)
  (render-context-ref context 'routes))

(define (render-context-page context)
  (render-context-ref context 'page))

(define (render-context-anchor-prefix context)
  (render-context-ref context 'anchor-prefix))

(define (render-context-site-url context)
  (render-context-ref context 'site-url))

(define (render-context-default-locale context)
  (render-context-ref context 'default-locale))

(define (render-context-hreflang? context)
  (render-context-ref context 'hreflang?))

(define (render-context-language-switcher? context)
  (render-context-ref context 'language-switcher?))

(define (render-context-translation-groups context)
  (render-context-ref context 'translation-groups))

(define (render-context-og-image context)
  (render-context-ref context 'og-image))

(define (render-context-og-image-alt context)
  (render-context-ref context 'og-image-alt))

(define (render-context-locale context)
  (render-context-ref context 'locale))

(define (render-context-locale-object context)
  (render-context-ref context 'locale-object))

(define (render-context-messages context)
  (render-context-ref context 'messages))

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
      (append overlay
              (remove (lambda (entry)
                        (assq (car entry) overlay))
                      (render-context-values context))))))

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
