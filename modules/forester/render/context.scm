;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render context)
  #:use-module (grove i18n)
  #:use-module (grove render context)
  #:use-module (grove site)
  #:use-module (grove private string-fun)
  #:use-module (forester private render labels)
  #:use-module (forester private web)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (srfi srfi-1)
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

(define (valid-translation-groups? groups)
  (and (list? groups)
       (every (lambda (entry)
                (and (pair? entry)
                     (non-empty-string? (car entry))
                     (list? (cdr entry))
                     (every site-page? (cdr entry))))
              groups)
       (= (length groups)
          (length (delete-duplicates (map car groups) string=?)))))

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

LOCALE and MESSAGES are resolved once, here, so renderers no longer read
dynamic locale or message parameters."
  (unless (forest? forest)
    (error "render context requires a forest" forest))
  (unless (or (not settings) (site-settings? settings))
    (error "render context settings must be site settings or #f" settings))
  (unless (and (boolean? hreflang?) (boolean? language-switcher?))
    (error "render context language switches must be booleans"
           hreflang? language-switcher?))
  (set! locale (validate-interface-options locale messages))
  (unless (or (not site-url) (web-url? site-url))
    (error "render context site URL must be an absolute HTTP(S) base URL or #f"
           site-url))
  (unless (or (not og-image) (non-empty-string? og-image))
    (error "render context Open Graph image must be a non-empty string or #f"
           og-image))
  (unless (or (not og-image-alt) (string? og-image-alt))
    (error "render context Open Graph image alt text must be a string or #f"
           og-image-alt))
  (when (eq? translation-groups derive-translation-groups-later)
    (set! translation-groups (derive-translation-groups (pages forest))))
  (unless (valid-translation-groups? translation-groups)
    (error "render context translation groups must map unique string keys to page lists"
           translation-groups))
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
    #:locale (resolve-interface-locale locale messages)
    #:locale-object locale
    #:messages (resolve-interface-catalog messages)))
