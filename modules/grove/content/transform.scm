;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove content transform)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:export (compose-content-transforms))

;; A content transform is an ordinary endomorphism on content documents.
;; Procedures are already composable Scheme values; this module supplies the
;; domain-specific order and checks the content-document boundary.

(define (check-content-document who document)
  (unless (content-document? document)
    (error who "content transform requires a content document" document))
  document)

(define (apply-content-transform transform document)
  (check-content-document "content transform requires a content document"
                          document)
  (let ((result (transform document)))
    (check-content-document "content transform must return a content document"
                            result)))

(define (compose-content-transforms . transforms)
  (unless (every procedure? transforms)
    (error "content transforms must be procedures"
           transforms))
  (lambda (document)
    (check-content-document "content transform requires a content document"
                            document)
    (fold apply-content-transform document transforms)))
