;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester backmatter)
  #:use-module (grove string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26) ; cut(e)
  #:use-module (ice-9 match)
  #:export (default-backmatter-sections
            default-reference-taxon-id
            validate-backmatter-sections
            normalize-backmatter-labels
            check-reference-taxon-id))

(define default-reference-taxon-id "Reference")
(define default-backmatter-labels
  '((context    . "Context")
    (references . "References")
    (backlinks  . "Backlinks")
    (related    . "Related")))
(define default-backmatter-sections (map car default-backmatter-labels))

(define (validate-backmatter-sections sections)
  (unless (and (list? sections)
               (every (cut memq <> default-backmatter-sections)
                      sections)
               (= (length sections)
                  (length (delete-duplicates sections eq?))))
    (error "Forester backmatter must be a list of unique sections"
           default-backmatter-sections
           sections))
  sections)

(define (normalize-backmatter-labels labels)
  (unless (and (list? labels)
               (every (match-lambda
                        [(a . b)
                         (and (memq a default-backmatter-sections)
                              (non-empty-string? b))]
                        [_ #f])
                      labels)
               (= (length labels)
                  (length (delete-duplicates (map car labels) eq?))))
    (error "Forester backmatter labels must map unique section names to non-empty strings"
           labels))
  (map (match-lambda [(a . b) (cons a (or (assq-ref labels a) b))])
       default-backmatter-labels))

(define (check-reference-taxon-id id)
  (unless (non-empty-string? id)
    (error "Forester reference taxon ID must be a non-empty string" id))
  id)
