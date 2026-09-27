;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private date)
  #:use-module (srfi srfi-1)
  #:export (forester-date?))

(define (leap-year? year)
  (or (zero? (modulo year 400))
      (and (zero? (modulo year 4))
           (not (zero? (modulo year 100))))))

(define (forester-date? value)
  (and (string? value)
       (= (string-length value) 10)
       (char=? (string-ref value 4) #\-)
       (char=? (string-ref value 7) #\-)
       (every char-numeric?
              (append (string->list (substring value 0 4))
                      (string->list (substring value 5 7))
                      (string->list (substring value 8 10))))
       (let* ((year (string->number (substring value 0 4)))
              (month (string->number (substring value 5 7)))
              (day (string->number (substring value 8 10))))
         (and (> year 0)
              (<= 1 month 12)
              (<= 1 day
                  (vector-ref
                    (if (leap-year? year)
                        '#(31 29 31 30 31 30 31 31 30 31 30 31)
                        '#(31 28 31 30 31 30 31 31 30 31 30 31))
                    (- month 1)))))))
