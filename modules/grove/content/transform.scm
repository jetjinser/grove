;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove content transform)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:export (compose-content-transforms
            transform-document))

;; A content transform is an ordinary endomorphism on content documents.
;; Procedures are already composable Scheme values; this module supplies the
;; domain-specific order and checks the content-document boundary.

(define (compose-content-transforms . transforms)
  (unless (every procedure? transforms)
    (error "content transforms must be procedures"
           transforms))
  (lambda (document)
    (fold (lambda (transform document)
            (transform document))
          document
          transforms)))

(define (transform-document transform document)
  (unless (procedure? transform)
    (error "content transform must be a procedure" transform))
  (unless (content-document? document)
    (error "content transform requires a content document" document))
  (let ((result (transform document)))
    (unless (content-document? result)
      (error "content transform must return a content document" result))
    result))
