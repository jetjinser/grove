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
            fingerprintable-datum?
            content-fingerprint
            scheme-datum-fingerprint
            file-fingerprint))

(define (fingerprintable-datum? value)
  "Return true when VALUE is finite, acyclic Scheme data with a stable write form."
  (let ((active (make-hash-table)))
    (define (walk value)
      (cond
       ((or (boolean? value) (char? value) (number? value)
            (symbol? value) (keyword? value)
            (string? value) (bytevector? value) (null? value))
        #t)
       ((or (pair? value) (vector? value))
        (and (not (hashq-ref active value))
             (begin
               (hashq-set! active value #t)
               (let ((valid?
                       (if (pair? value)
                           (and (walk (car value)) (walk (cdr value)))
                           (let loop ((index 0))
                             (or (= index (vector-length value))
                                 (and (walk (vector-ref value index))
                                      (loop (+ index 1))))))))
                 (hashq-remove! active value)
                 valid?))))
       (else #f)))
    (walk value)))

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

(define (scheme-datum-fingerprint datum)
  "Fingerprint DATUM's Guile write representation as UTF-8.
Only finite, acyclic data with a stable write representation is accepted."
  (unless (fingerprintable-datum? datum)
    (error "Scheme datum fingerprint requires finite, acyclic data" datum))
  (content-fingerprint
    (call-with-output-string (lambda (port) (write datum port)))))

(define (file-fingerprint path)
  (let ((port (open-file path "rb")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (fingerprint-bytes (get-bytevector-all port)))
      (lambda () (close-port port)))))
