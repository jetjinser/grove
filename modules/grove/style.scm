;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-content-style content-style? content-style-name
            content-style-properties merge-content-style))

(define-record-type <content-style>
  (%make-content-style name properties)
  content-style?
  (name content-style-name/raw)
  (properties content-style-properties/raw))

(define (content-style-name style)
  (snapshot-data (content-style-name/raw style)))

(define (content-style-properties style)
  (snapshot-data (content-style-properties/raw style)))

(define* (make-content-style name #:optional (properties '()))
  "Describe presentation intent and renderer-specific style properties."
  (unless (or (not name)
              (symbol? name)
              (and (string? name) (> (string-length name) 0)))
    (error "content style name must be a non-empty string, symbol, or #f"
           name))
  (unless (list? properties)
    (error "content style properties must be a list" properties))
  (%make-content-style (snapshot-data name)
                       (snapshot-data properties)))

(define (merge-style-name a b)
  (cond
    ((not a) b)
    ((not b) a)
    (else
     (string-append (if (symbol? a) (symbol->string a) a) " "
                    (if (symbol? b) (symbol->string b) b)))))

(define (merge-content-style . styles)
  "Combine STYLES left to right into one value; no styles yields the identity.

Names join into one class list, and later properties win where a renderer
reads one property of a kind, such as the tag. Styles are values: pass them
through custom commands, derive new ones, and attach the result anywhere a
#:style is accepted."
  (unless (every content-style? styles)
    (error "merge-content-style combines content styles" styles))
  (fold
    (lambda (style combined)
      (make-content-style
        (merge-style-name (content-style-name combined)
                          (content-style-name style))
        (append (content-style-properties combined)
                (content-style-properties style))))
    (make-content-style #f)
    styles))
