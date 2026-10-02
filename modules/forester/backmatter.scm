;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester backmatter)
  #:use-module (srfi srfi-1)
  #:export (default-backmatter-sections
            check-backmatter-sections))

(define default-backmatter-sections '(context backlinks))

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
