;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render site)
  #:use-module (grove i18n)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (forester private web)
  #:use-module (srfi srfi-9)
  #:export (site-settings?
            make-site-settings
            site-settings-site-url
            site-settings-default-locale))

(define-record-type <site-settings>
  (make-site-settings* site-url default-locale)
  site-settings?
  (site-url site-settings-site-url/raw)
  (default-locale site-settings-default-locale/raw))

(define (site-settings-site-url settings)
  (snapshot-data (site-settings-site-url/raw settings)))

(define (site-settings-default-locale settings)
  (snapshot-data (site-settings-default-locale/raw settings)))

(define* (make-site-settings #:key site-url (default-locale #f))
  "Shared site values for the HTML, sitemap, and RSS renderers.

SITE-URL, when supplied, is an absolute HTTP(S) origin IRI with no credentials,
non-root path, query, or fragment. Unicode domain names are accepted. HTML
retains their IRI spelling; RSS and sitemap map the host to IDNA ASCII and
percent-encode non-ASCII IRI components. Put any deployment path prefix in the
shared HTML routes.
DEFAULT-LOCALE supplies the HTML interface language, x-default alternate
language, and RSS feed language unless a renderer-specific locale overrides
it. Renderer-specific keyword values override these shared defaults. A
settings value may omit SITE-URL, but sitemap and RSS renderers still require
an origin."
  (unless (or (not site-url) (web-origin? site-url))
    (error "Site settings URL must be an absolute HTTP(S) origin IRI or #f"
           site-url))
  (unless (or (not default-locale) (non-empty-string? default-locale))
    (error "Site settings default locale must be a tag or #f"
           default-locale))
  (when default-locale
    (set! default-locale (locale-tag (make-locale default-locale))))
  (make-site-settings* (snapshot-data site-url)
                       (snapshot-data default-locale)))
