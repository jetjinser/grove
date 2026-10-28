;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style html)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove output)
  #:use-module (grove private snapshot)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (html-style?
            make-html-style
            compose-html-styles
            html-style-stylesheets
            html-style-artifacts))

(define-record-type <html-style>
  (%make-html-style stylesheets assets)
  html-style?
  (stylesheets html-style-stylesheets/raw)
  (assets html-style-extra-assets/raw))

(define (snapshot-artifact artifact)
  (let ((path (snapshot-data (out:artifact-path/raw artifact)))
        (content (out:artifact-content/raw artifact))
        (sources (out:artifact-sources/raw artifact)))
    (if (string? content)
        (make-artifact path (snapshot-data content)
                       (snapshot-data sources))
        (make-byte-artifact path (bytevector-copy content)
                            (snapshot-data sources)))))

(define (html-style-stylesheets style)
  "Return STYLE's stylesheet artifacts as independent copies."
  (map snapshot-artifact (html-style-stylesheets/raw style)))

(define* (make-html-style #:key (stylesheets '()) (assets '()))
  "Describe stylesheet and extra asset artifacts contributed by an HTML style.

Both arguments are artifact lists; the constructor snapshots their contents.
Paths across both lists must be unique and must not conflict as files and
directories."
  (unless (and (list? stylesheets)
               (list? assets)
               (every artifact? stylesheets)
               (every artifact? assets))
    (error "HTML style requires artifact lists" stylesheets assets))
  (let ((stylesheets (map snapshot-artifact (snapshot-data stylesheets)))
        (assets (map snapshot-artifact (snapshot-data assets))))
    (validate-artifacts (append stylesheets assets))
    (%make-html-style stylesheets assets)))

(define (compose-html-styles . styles)
  "Combine STYLES in order, appending their stylesheets and extra assets.
With no arguments, return an empty HTML style."
  (unless (every html-style? styles)
    (error "HTML styles can only be composed from HTML styles" styles))
  (make-html-style
    #:stylesheets (append-map html-style-stylesheets styles)
    #:assets (append-map html-style-extra-assets/raw styles)))

(define (html-style-artifacts style)
  "Return STYLE's stylesheet artifacts followed by its extra assets."
  (append (html-style-stylesheets style)
          (map snapshot-artifact (html-style-extra-assets/raw style))))
