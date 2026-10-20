;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester backmatter)
  #:use-module (grove i18n)
  #:use-module (grove string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26) ; cut(e)
  #:use-module (ice-9 match)
  #:export (default-backmatter-sections
            default-reference-taxon-id
            validate-backmatter-sections
            normalize-backmatter-labels
            localized-backmatter-labels
            check-reference-taxon-id))

(define default-reference-taxon-id "reference")
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

(define* (normalize-backmatter-labels labels
                                      #:key (defaults default-backmatter-labels))
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
       defaults))

(define (localized-backmatter-labels catalog locale)
  "Default backmatter headings from CATALOG for LOCALE."
  (map (lambda (section)
         (cons section
               (message catalog
                        (string->symbol
                          (string-append "backmatter-" (symbol->string section)))
                        locale)))
       default-backmatter-sections))

(define (check-reference-taxon-id id)
  (unless (non-empty-string? id)
    (error "Forester reference taxon ID must be a non-empty string" id))
  id)
