;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render labels)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (forester backmatter)
  #:export (resolve-chrome-locale
            resolve-chrome-catalog
            chrome-message
            chrome-message-format
            localized-backmatter-labels))

;; Chrome labels read the render context.  The optional-context variants are
;; a compatibility path for callers that have no context yet; they fall back
;; to the default catalog and the default locale instead of dynamic state.

(define (resolve-chrome-locale locale messages)
  (cond
    ((not locale) (message-catalog-default messages))
    ((locale? locale) (locale-tag locale))
    ((string? locale) (locale-tag (make-locale locale)))
    (else (error "renderer locale must be a locale, tag, or #f" locale))))

(define (resolve-chrome-catalog messages)
  (if (eq? messages default-message-catalog)
      messages
      (merge-message-catalogs default-message-catalog messages)))

(define* (chrome-message key #:optional context)
  (let ((context (or context (current-render-context))))
    (if context
        (message (render-context-ref context 'messages)
                 key
                 (render-context-ref context 'locale))
        (message default-message-catalog key "en"))))

(define* (chrome-message-format key arguments #:optional context)
  (let ((context (or context (current-render-context))))
    (if context
        (message-format (render-context-ref context 'messages)
                        key arguments
                        (render-context-ref context 'locale))
        (message-format default-message-catalog key arguments "en"))))

(define* (localized-backmatter-labels #:optional context)
  "Default backmatter headings for the context catalog and locale."
  (let ((context (or context (current-render-context))))
    (let ((catalog (if context
                       (render-context-ref context 'messages)
                       default-message-catalog))
          (locale (if context
                      (render-context-ref context 'locale)
                      "en")))
      (map (lambda (section)
             (cons section
                   (message catalog
                            (string->symbol
                              (string-append "backmatter-" (symbol->string section)))
                            locale)))
           default-backmatter-sections))))
