;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private translations)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (page-locale
            page-translation-group
            default-translation-page))

(define (page-locale page)
  (assq-ref (site-page-metadata page) 'locale))

(define (page-translation-group page groups)
  (let ((key (assq-ref (site-page-metadata page) 'translation)))
    (and key (assoc-ref groups key))))

(define (default-translation-page group locale)
  (and locale
       (find (lambda (page) (equal? (page-locale page) locale)) group)))
