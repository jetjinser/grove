;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester backmatter)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (default-backmatter-sections
            default-reference-taxon-id
            check-backmatter-sections
            check-backmatter-labels
            check-reference-taxon-id))

(define default-backmatter-sections '(context backlinks related))
(define default-reference-taxon-id "Reference")
(define default-backmatter-labels
  '((context . "Context")
    (backlinks . "Backlinks")
    (related . "Related")))

(define (check-backmatter-sections sections)
  (unless (and (list? sections)
               (every (lambda (section)
                        (memq section '(context backlinks related)))
                      sections)
               (= (length sections)
                  (length (delete-duplicates sections eq?))))
    (error "Forester backmatter must be a list of unique sections: context, backlinks, related"
           sections))
  sections)

(define (check-backmatter-labels labels)
  (unless (and (list? labels)
               (every (lambda (entry)
                        (and (pair? entry)
                             (memq (car entry) '(context backlinks related))
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

(define (check-reference-taxon-id id)
  (unless (and (string? id) (not (string-null? id)))
    (error "Forester reference taxon ID must be a non-empty string" id))
  id)
