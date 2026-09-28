;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style html)
  #:use-module (grove output)
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
  (stylesheets html-style-stylesheets)
  (assets html-style-extra-assets))

(define* (make-html-style #:key (stylesheets '()) (assets '()))
  (unless (and (list? stylesheets)
               (list? assets)
               (every artifact? stylesheets)
               (every artifact? assets))
    (error "HTML style requires artifact lists" stylesheets assets))
  (%make-html-style stylesheets assets))

(define (compose-html-styles . styles)
  (unless (every html-style? styles)
    (error "HTML styles can only be composed from HTML styles" styles))
  (make-html-style
    #:stylesheets (append-map html-style-stylesheets styles)
    #:assets (append-map html-style-extra-assets styles)))

(define (html-style-artifacts style)
  (append (html-style-stylesheets style)
          (html-style-extra-assets style)))
