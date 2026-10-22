;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render context)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (render-context?
            make-render-context
            render-context-ref
            render-context-with
            render-context-values))

;; A render context is an explicit value passed to interpretations.  It is a
;; record over an association list so projects can add namespaced keys without
;; changing the core record, while the common keys stay documented here:
;; forest, routes, page, anchor-prefix, site-url, default-locale, hreflang?,
;; language-switcher?, translation-groups, og-image, og-image-alt, locale,
;; locale-object, messages.

(define-record-type <render-context>
  (%make-render-context values)
  render-context?
  (values render-context-values))

(define* (make-render-context #:key (forest #f) (routes #f) (page #f)
                              (anchor-prefix #f) (site-url #f)
                              (default-locale #f) (hreflang? #t)
                              (language-switcher? #t) (translation-groups #f)
                              (og-image #f) (og-image-alt #f)
                              (locale #f) (locale-object #f) (messages #f)
                              #:allow-other-keys)
  (let ((entries (filter (lambda (entry) (cdr entry))
                         `((forest . ,forest)
                           (routes . ,routes)
                           (page . ,page)
                           (anchor-prefix . ,anchor-prefix)
                           (site-url . ,site-url)
                           (default-locale . ,default-locale)
                           (hreflang? . ,hreflang?)
                           (language-switcher? . ,language-switcher?)
                           (translation-groups . ,translation-groups)
                           (og-image . ,og-image)
                           (og-image-alt . ,og-image-alt)
                           (locale . ,locale)
                           (locale-object . ,locale-object)
                           (messages . ,messages)))))
    (%make-render-context entries)))

(define (render-context-ref context key)
  (unless (render-context? context)
    (error "not a render context" context))
  (assq-ref (render-context-values context) key))

(define* (render-context-with context #:rest entries)
  "Return CONTEXT with ENTRIES (a keyword plist) overlaid."
  (unless (render-context? context)
    (error "not a render context" context))
  (let loop ((remaining entries) (overlay '()))
    (cond
      ((null? remaining)
       (%make-render-context
         (append overlay
                 (remove (lambda (entry)
                           (assq (car entry) overlay))
                         (render-context-values context)))))
      ((keyword? (car remaining))
       (let ((key (keyword->symbol (car remaining)))
             (value (cadr remaining)))
         (loop (cddr remaining)
               (if value
                   (cons (cons key value) overlay)
                   overlay))))
      (else (error "render-context-with expects keyword/value pairs"
                   entries)))))
