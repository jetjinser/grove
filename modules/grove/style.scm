;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-content-style content-style? content-style-name
            content-style-properties merge-content-style))

(define-record-type <content-style>
  (%make-content-style name properties)
  content-style?
  (name content-style-name)
  (properties content-style-properties))

(define* (make-content-style name #:optional (properties '()))
  "Describe presentation intent and renderer-specific style properties."
  (unless (or (not name) (string? name) (symbol? name))
    (error "content style name must be a string, symbol, or #f" name))
  (unless (list? properties)
    (error "content style properties must be a list" properties))
  (%make-content-style name properties))

(define (merge-style-name a b)
  (cond
    ((not a) b)
    ((not b) a)
    (else
     (string-append (if (symbol? a) (symbol->string a) a) " "
                    (if (symbol? b) (symbol->string b) b)))))

(define (merge-content-style . styles)
  "Combine STYLES left to right into one value.  Names join into a single
class list, and later properties win where a renderer reads one property
of a kind, such as the tag.  Styles are values: pass them through custom
commands, derive new ones, and attach the result anywhere a #:style is
accepted."
  (unless (pair? styles)
    (error "merge-content-style requires at least one style" styles))
  (fold
    (lambda (b a)
      (unless (content-style? b)
        (error "merge-content-style combines content styles" b))
      (make-content-style
        (merge-style-name (content-style-name a) (content-style-name b))
        (append (content-style-properties a)
                (content-style-properties b))))
    (car styles)
    (cdr styles)))
