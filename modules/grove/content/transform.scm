;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove content transform)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (content-transform?
            make-content-transform
            identity-content-transform
            compose-content-transforms
            transform-document))

;; A content transform is an endomorphism on content documents.  It is a
;; separate layer from rendering: transforms rewrite the content algebra,
;; renderer rules interpret it.  Composition is ordinary function composition
;; in application order, with the identity transform as the unit.

(define-record-type <content-transform>
  (make-content-transform procedure)
  content-transform?
  (procedure content-transform-procedure))

(define identity-content-transform
  (make-content-transform identity))

(define (compose-content-transforms . transforms)
  (unless (every content-transform? transforms)
    (error "content transforms can only be composed from transforms"
           transforms))
  (make-content-transform
    (lambda (document)
      (let loop ((remaining transforms) (document document))
        (if (null? remaining)
            document
            (loop (cdr remaining)
                  ((content-transform-procedure (car remaining)) document)))))))

(define (transform-document transform document)
  (unless (content-transform? transform)
    (error "not a content transform" transform))
  (unless (content-document? document)
    (error "content transform requires a content document" document))
  (let ((result ((content-transform-procedure transform) document)))
    (unless (content-document? result)
      (error "content transform must return a content document" result))
    result))
