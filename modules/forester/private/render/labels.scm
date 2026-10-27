;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render labels)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (forester backmatter)
  #:export (resolve-interface-locale
            resolve-interface-catalog
            interface-cache-identity
            validate-interface-options
            validate-rules-id
            interface-message
            interface-message-format
            localized-backmatter-labels))

;; Built-in interface messages read their render context explicitly.

(define (validate-interface-options locale messages)
  "Validate interface options and return LOCALE as a locale value or #f."
  (let ((locale
          (cond
            ((not locale) #f)
            ((locale? locale) locale)
            ((string? locale) (make-locale locale))
            (else
             (error "renderer locale must be a locale, tag, or #f" locale)))))
    (unless (message-catalog? messages)
      (error "renderer messages must be a message catalog" messages))
    locale))

(define (validate-rules-id rules-id)
  (unless (or (not rules-id)
              (symbol? rules-id)
              (and (string? rules-id) (not (string-null? rules-id))))
    (error "renderer rules ID must be a symbol, non-empty string, or #f"
           rules-id))
  rules-id)

(define (resolve-interface-locale locale messages)
  (cond
    ((not locale) (message-catalog-default messages))
    ((locale? locale) (locale-tag locale))
    ((string? locale) (locale-tag (make-locale locale)))
    (else (error "renderer locale must be a locale, tag, or #f" locale))))

(define (resolve-interface-catalog messages)
  (if (eq? messages default-message-catalog)
      messages
      (merge-message-catalogs default-message-catalog messages)))

(define (interface-cache-identity locale messages)
  (let ((catalog (resolve-interface-catalog messages)))
    (list (resolve-interface-locale locale catalog)
          (message-catalog-default catalog)
          (message-catalog-entries catalog))))

(define (interface-message key context)
  (message (render-context-ref context 'messages)
           key
           (render-context-ref context 'locale)))

(define (interface-message-format key arguments context)
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
