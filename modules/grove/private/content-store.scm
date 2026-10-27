;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private content-store)
  #:use-module (grove private artifact-store)
  #:use-module (grove private fingerprint)
  #:use-module (grove output)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (read-content-index
            write-content-index
            restore-content-bytes
            publish-content-bytes))

(define (read-content-index identity)
  ;;     A corrupt index is a miss; each referenced blob is verified
  ;;     independently before its bytes are reused.
  (catch #t
    (lambda ()
      (let ((path (artifact-blob-path identity)))
        (and (artifact-blob-exists? identity)
             (call-with-input-file path
               (lambda (port)
                 (let ((index (read port)))
                   (and (list? index)
                        (every (lambda (entry)
                                 (and (pair? entry)
                                      (artifact-path? (car entry))
                                      (fingerprint? (cdr entry))))
                               index)
                        index)))))))
    (lambda args #f)))

(define (write-content-index identity entries)
  (publish-artifact-blob! identity
    (lambda (port)
      (write (map (lambda (entry) (cons (car entry) (cdr entry))) entries)
             port))))

(define (restore-content-bytes fingerprint)
  (catch #t
    (lambda ()
      (let ((path (artifact-blob-path fingerprint)))
        (and (artifact-blob-exists? fingerprint)
             (let ((bytes
                     (call-with-input-file path get-bytevector-all
                       #:binary #t)))
               (and (string=? (fingerprint-bytes bytes) fingerprint)
                    bytes)))))
    (lambda args #f)))

(define (publish-content-bytes bytes)
  (let ((fingerprint (content-fingerprint bytes)))
    (publish-artifact-blob! fingerprint
      (lambda (port) (put-bytevector port bytes)))
    fingerprint))
