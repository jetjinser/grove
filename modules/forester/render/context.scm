;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render context)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (grove site)
  #:use-module (forester render labels)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:export (translation-groups
            make-site-render-context))

;; Site-level render contexts are built once and shared by the HTML, text,
;; gemtext, and derived renderers.  Page-level values are derived from them
;; with render-context-with.

(define (translation-groups pages)
  (let ((groups (make-hash-table)))
    (for-each
      (lambda (page)
        (let ((key (assq-ref (site-page-metadata page) 'translation)))
          (when key
            (hash-set! groups key
                       (cons page (or (hash-ref groups key) '()))))))
      pages)
    groups))

(define* (make-site-render-context forest #:key
                                   (routes #f)
                                   (settings #f)
                                   (site-url
                                     (and settings
                                          (site-settings-site-url settings)))
                                   (default-locale
                                     (and settings
                                          (site-settings-default-locale
                                            settings)))
                                   (hreflang? #t)
                                   (language-switcher? #t)
                                   (og-image #f)
                                   (og-image-alt #f)
                                   (locale #f)
                                   (messages default-message-catalog)
                                   (translation-groups #f))
  "Build the site-level render context for FOREST.

LOCALE and MESSAGES are resolved once, here, so renderers no longer read the
dynamic chrome parameters."
  (unless (forest? forest)
    (error "render context requires a forest" forest))
  (make-render-context
    #:forest forest
    #:routes routes
    #:site-url site-url
    #:default-locale default-locale
    #:hreflang? hreflang?
    #:language-switcher? language-switcher?
    #:translation-groups translation-groups
    #:og-image og-image
    #:og-image-alt og-image-alt
    #:locale (resolve-chrome-locale locale messages)
    #:locale-object (and locale
                         (if (locale? locale) locale (make-locale locale)))
    #:messages (resolve-chrome-catalog messages)))
