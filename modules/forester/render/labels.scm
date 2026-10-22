;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render labels)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (forester backmatter)
  #:export (current-chrome-locale
            current-chrome-locale-object
            current-message-catalog
            resolve-chrome-locale
            resolve-chrome-catalog
            call-with-chrome-locale
            with-chrome-locale
            chrome-message
            chrome-message-format
            localized-backmatter-labels))

(define current-chrome-locale (make-parameter "en"))
(define current-chrome-locale-object (make-parameter #f))
(define current-message-catalog (make-parameter default-message-catalog))

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

(define* (call-with-chrome-locale locale messages thunk)
  "Run THUNK with the merged catalog and the resolved locale bound."
  (let ((messages (resolve-chrome-catalog messages)))
    (parameterize ((current-message-catalog messages)
                   (current-chrome-locale
                     (resolve-chrome-locale locale messages))
                   (current-chrome-locale-object
                     (and locale
                          (if (locale? locale) locale (make-locale locale)))))
      (thunk))))

(define* (chrome-message key #:optional context)
  (if context
      (message (render-context-ref context 'messages)
               key
               (render-context-ref context 'locale))
      (message (current-message-catalog) key (current-chrome-locale))))

(define* (chrome-message-format key arguments #:optional context)
  (if context
      (message-format (render-context-ref context 'messages)
                      key arguments
                      (render-context-ref context 'locale))
      (message-format (current-message-catalog) key arguments
                      (current-chrome-locale))))
(define* (localized-backmatter-labels #:optional context)
  "Default backmatter headings for the context catalog and locale."
  (let ((catalog (if context
                     (render-context-ref context 'messages)
                     (current-message-catalog)))
        (locale (if context
                    (render-context-ref context 'locale)
                    (current-chrome-locale))))
    (map (lambda (section)
           (cons section
                 (message catalog
                          (string->symbol
                            (string-append "backmatter-" (symbol->string section)))
                          locale)))
         default-backmatter-sections)))
(define-syntax-rule (with-chrome-locale (locale messages) body ...)
  (call-with-chrome-locale locale messages (lambda () body ...)))
