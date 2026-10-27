;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render labels)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (forester backmatter)
  #:export (resolve-chrome-locale
            resolve-chrome-catalog
            validate-chrome-options
            validate-rules-id
            chrome-message
            chrome-message-format
            localized-backmatter-labels))

;; Chrome labels read their render context explicitly.

(define (validate-chrome-options locale messages)
  (unless (or (not locale) (locale? locale) (string? locale))
    (error "renderer locale must be a locale, tag, or #f" locale))
  (when (string? locale)
    (make-locale locale))
  (unless (message-catalog? messages)
    (error "renderer messages must be a message catalog" messages))
  #t)

(define (validate-rules-id rules-id)
  (unless (or (not rules-id)
              (symbol? rules-id)
              (and (string? rules-id) (not (string-null? rules-id))))
    (error "renderer rules ID must be a symbol, non-empty string, or #f"
           rules-id))
  rules-id)

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

(define (chrome-message key context)
  (message (render-context-ref context 'messages)
           key
           (render-context-ref context 'locale)))

(define (chrome-message-format key arguments context)
  (message-format (render-context-ref context 'messages)
                  key arguments
                  (render-context-ref context 'locale)))

(define (localized-backmatter-labels context)
  "Default backmatter headings for the context catalog and locale."
  (let ((catalog (render-context-ref context 'messages))
        (locale (render-context-ref context 'locale)))
    (map (lambda (section)
           (cons section
                 (message catalog
                          (string->symbol
                            (string-append "backmatter-" (symbol->string section)))
                          locale)))
         default-backmatter-sections)))
