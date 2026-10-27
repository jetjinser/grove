;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private topic)
  #:use-module (grove private string-fun)
  #:use-module (ice-9 string-fun)
  #:export (valid-topic-id?
            canonical-topic-id?))

(define char-set:topic-body (char-set-adjoin char-set:letter+digit #\-))

(define (valid-topic-id? id)
  (and (non-empty-string? id)
       (char-set-contains? char-set:letter+digit (string-ref id 0))
       (char-set-contains? char-set:letter+digit
                           (string-ref id (- (string-length id) 1)))
       (string-every char-set:topic-body id)))

(define (canonical-topic-id? id)
  (and (valid-topic-id? id)
       (string=? id (string-downcase id))))
