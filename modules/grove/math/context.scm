;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math context)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (current-math-assumptions math-assumptions
            call-with-math-assumptions with-math-assumptions))

(define current-math-assumptions
  (make-parameter '()
    (lambda (facts)
      (unless (and (list? facts) (every math-term? facts))
        (error "math context requires a list of assumption terms" facts))
      (delete-duplicates facts equal?))))

(define (math-assumptions sources)
  (unless (and (list? sources) (every string? sources))
    (error "math assumptions must be source strings" sources))
  (delete-duplicates
    (append (current-math-assumptions)
      (append-map
        (lambda (source)
          (map (lambda (part) (math-expression->term (parse-math part)))
               (filter (lambda (part) (not (string-null? part)))
                       (map string-trim-both (string-split source #\;)))))
        sources)) equal?))

(define* (call-with-math-assumptions sources thunk #:key (replace? #f))
  (parameterize ((current-math-assumptions
                   (if replace? '() (current-math-assumptions))))
    (parameterize ((current-math-assumptions (math-assumptions sources)))
      (thunk))))

(define-syntax-rule (with-math-assumptions sources body ...)
  (call-with-math-assumptions sources (lambda () body ...)))
