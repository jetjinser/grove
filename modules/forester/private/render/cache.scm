;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render cache)
  #:use-module (grove render rules)
  #:export (renderer-cacheable?))

(define (renderer-cacheable? rules rules-id cache?)
  "Return whether renderer output has a stable cache identity.

Default rules are stable by construction. Custom RULES are cacheable only
when the caller supplies a RULES-ID; CACHE? can disable caching entirely."
  (and cache?
       (or (eq? rules empty-render-rules)
           (and rules-id #t))))
