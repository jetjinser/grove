;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render text)
  #:use-module (grove private render text)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:export (content->text))

(define* (content->text content #:key (rules empty-render-rules)
                        (context (make-render-context)))
  "Render CONTENT to text using public render rules and context."
  (unless (render-rules? rules)
    (error "not a render rule set" rules))
  (unless (render-context? context)
    (error "not a render context" context))
  (content->text/internal content #:rules rules #:context context))
