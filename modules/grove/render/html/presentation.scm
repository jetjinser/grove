;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html presentation)
  #:use-module (grove style html)
  #:use-module (grove render rules)
  #:use-module (srfi srfi-9)
  #:export (html-presentation?
            make-html-presentation
            empty-html-presentation
            compose-html-presentations
            html-presentation-style
            html-presentation-rules))

;; A presentation is a product of independent components: stylesheets/assets
;; and renderer rules.  Composition is componentwise, so the product monoid
;; laws reduce to the laws of each component.

(define-record-type <html-presentation>
  (%make-html-presentation style rules)
  html-presentation?
  (style html-presentation-style)
  (rules html-presentation-rules))

(define* (make-html-presentation #:key (style (make-html-style))
                                 (rules empty-render-rules))
  (unless (html-style? style)
    (error "presentation style must be an HTML style" style))
  (unless (render-rules? rules)
    (error "presentation rules must be render rules" rules))
  (%make-html-presentation style rules))

(define empty-html-presentation
  (make-html-presentation))

(define (compose-html-presentations base overlay)
  "Compose presentations by appending styles and layering rules.

Styles accumulate in order; later rules take precedence and may delegate."
  (unless (and (html-presentation? base) (html-presentation? overlay))
    (error "presentations can only be composed from presentations"
           base overlay))
  (%make-html-presentation
    (compose-html-styles (html-presentation-style base)
                         (html-presentation-style overlay))
    (compose-render-rules (html-presentation-rules base)
                        (html-presentation-rules overlay))))
