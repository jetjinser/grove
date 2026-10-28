;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private backmatter)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26) ; cut(e)
  #:export (default-backmatter-sections
            default-reference-taxon-id
            validate-backmatter-sections
            check-reference-taxon-id))

(define default-reference-taxon-id "reference")
(define default-backmatter-sections
  '(context references backlinks related))

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

(define (check-reference-taxon-id id)
  (unless (non-empty-string? id)
    (error "Forester reference taxon ID must be a non-empty string" id))
  id)
