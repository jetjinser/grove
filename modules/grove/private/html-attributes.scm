;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private html-attributes)
  #:use-module (srfi srfi-1)
  #:export (valid-html-attribute-list?))

(define (valid-html-attribute-list? attributes)
  (and (list? attributes)
       (every (lambda (entry)
                (and (pair? entry)
                     (symbol? (car entry))
                     (string? (cdr entry))))
              attributes)
       (= (length attributes)
          (length (delete-duplicates (map car attributes) eq?)))))
