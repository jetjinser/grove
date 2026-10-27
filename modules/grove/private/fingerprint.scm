;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private fingerprint)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (fingerprint-bytes
            fingerprint?
            content-fingerprint
            file-fingerprint))

(define (fingerprint? value)
  (and (string? value)
       (= (string-length value) 64)
       (every (lambda (character)
                (or (and (char>=? character #\0) (char<=? character #\9))
                    (and (char>=? character #\a) (char<=? character #\f))))
              (string->list value))))

(define (fingerprint-bytes bytes)
  (let ((digest (sha256 bytes))
        (digits "0123456789abcdef"))
    (list->string
     (append-map
      (lambda (index)
        (let ((byte (bytevector-u8-ref digest index)))
          (list (string-ref digits (quotient byte 16))
                (string-ref digits (modulo byte 16)))))
      (iota (bytevector-length digest))))))

(define (content-fingerprint content)
  (cond
   ((string? content) (fingerprint-bytes (string->utf8 content)))
   ((bytevector? content) (fingerprint-bytes content))
   (else (error "content fingerprint requires text or bytes" content))))

(define (file-fingerprint path)
  (let ((port (open-file path "rb")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (fingerprint-bytes (get-bytevector-all port)))
      (lambda () (close-port port)))))
