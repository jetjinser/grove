;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private alist)
  #:use-module (srfi srfi-1)
  #:export (unique-symbol-key-alist?))

(define (unique-symbol-key-alist? value)
  "Return #t when VALUE is an alist with unique symbol keys."
  (and (list? value)
       (every (lambda (entry)
                (and (pair? entry) (symbol? (car entry))))
              value)
       (let loop ((entries value) (keys '()))
         (or (null? entries)
             (and (not (memq (caar entries) keys))
                  (loop (cdr entries) (cons (caar entries) keys)))))))
