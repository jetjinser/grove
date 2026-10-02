;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester backmatter)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (default-backmatter-sections
            check-backmatter-sections
            check-backmatter-labels))

(define default-backmatter-sections '(context backlinks))
(define default-backmatter-labels
  '((context . "Context")
    (backlinks . "Backlinks")))

(define (check-backmatter-sections sections)
  (unless (and (list? sections)
               (every (lambda (section)
                        (memq section '(context backlinks)))
                      sections)
               (= (length sections)
                  (length (delete-duplicates sections eq?))))
    (error "Forester backmatter must be a list of unique sections: context, backlinks"
           sections))
  sections)

(define (check-backmatter-labels labels)
  (unless (and (list? labels)
               (every (lambda (entry)
                        (and (pair? entry)
                             (memq (car entry) '(context backlinks))
                             (string? (cdr entry))
                             (not (string-null? (cdr entry)))))
                      labels)
               (= (length labels)
                  (length (delete-duplicates (map car labels) eq?))))
    (error "Forester backmatter labels must map unique section names to non-empty strings"
           labels))
  (map (lambda (entry)
         (cons (car entry)
               (or (assq-ref labels (car entry)) (cdr entry))))
       default-backmatter-labels))
