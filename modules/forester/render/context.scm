;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render context)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (grove site)
  #:use-module (forester private render labels)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:export (translation-groups
            make-site-render-context))

;; Each renderer entry point builds a site-level context for one render pass;
;; page-level values are derived from it with render-context-with. Custom
;; renderers can use the same helper when they need a site render context.

(define derive-translation-groups-later (list 'derive-translation-groups-later))

(define (derive-translation-groups pages)
  (let ((groups (make-hash-table))
        (keys '()))
    (for-each
      (lambda (page)
        (let ((key (assq-ref (site-page-metadata page) 'translation)))
          (when key
            (let ((group (hash-ref groups key #f)))
              (if group
                  (hash-set! groups key (cons page group))
                  (begin
                    (hash-set! groups key (list page))
                    (set! keys (cons key keys))))))))
      pages)
    ;; Ordinary Scheme data stays inside render-context's snapshot boundary.
    ;; Group and page order follow first occurrence in the site's page list.
    (map (lambda (key)
           (cons key (reverse (hash-ref groups key))))
         (reverse keys))))

(define translation-groups derive-translation-groups)

(define* (make-site-render-context forest #:key
                                   (routes #f)
                                   (settings #f)
                                   (site-url
                                     (and (site-settings? settings)
                                          (site-settings-site-url settings)))
                                   (default-locale
                                     (and (site-settings? settings)
                                          (site-settings-default-locale
                                            settings)))
                                   (hreflang? #t)
                                   (language-switcher? #t)
                                   (og-image #f)
                                   (og-image-alt #f)
                                   (locale #f)
                                   (messages default-message-catalog)
                                   (translation-groups derive-translation-groups-later))
  "Build the site-level render context for FOREST.

LOCALE and MESSAGES are resolved once, here, so renderers no longer read the
dynamic chrome parameters."
  (unless (forest? forest)
    (error "render context requires a forest" forest))
  (unless (or (not settings) (site-settings? settings))
    (error "render context settings must be site settings or #f" settings))
  (when (eq? translation-groups derive-translation-groups-later)
    (set! translation-groups (derive-translation-groups (pages forest))))
  (when default-locale
    (set! default-locale (locale-tag (make-locale default-locale))))
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
