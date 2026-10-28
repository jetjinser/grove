;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private alist)
  #:use-module (srfi srfi-1)
  #:export (unique-symbol-key-alist?
            unique-string-key-alist?))

(define (unique-key-alist? value key? same-key?)
  (and (list? value)
       (every (lambda (entry)
                (and (pair? entry) (key? (car entry))))
              value)
       (let loop ((entries value) (keys '()))
         (or (null? entries)
             (and (not (any (lambda (key)
                              (same-key? (caar entries) key))
                            keys))
                  (loop (cdr entries) (cons (caar entries) keys)))))))

(define (unique-symbol-key-alist? value)
  "Return #t when VALUE is an alist with unique symbol keys."
  (unique-key-alist? value symbol? eq?))

(define (unique-string-key-alist? value)
  "Return #t when VALUE is an alist with unique string keys."
  (unique-key-alist? value string? string=?))
