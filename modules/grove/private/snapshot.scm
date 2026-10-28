;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private snapshot)
  #:use-module (rnrs bytevectors)
  #:export (snapshot-data))

(define (snapshot-data value)
  "Copy mutable strings, bytevectors, vectors, and pair structure, preserving
opaque values.

Use at public constructor boundaries for declarative data. Procedures and
records remain shared values; their own constructors define their semantics."
  (let ((seen (make-hash-table)))
    (let copy ((value value))
      (cond
       ((string? value)
        (let ((result (string-copy value))
              (properties (source-properties value)))
          (when properties
            (set-source-properties! result (copy properties)))
          result))
       ((bytevector? value)
        (bytevector-copy value))
       ((vector? value)
        (or (hashq-ref seen value)
            (let ((result (make-vector (vector-length value))))
              (hashq-set! seen value result)
              (let loop ((index 0))
                (when (< index (vector-length value))
                  (vector-set! result index (copy (vector-ref value index)))
                  (loop (+ index 1))))
              result)))
       ((pair? value)
        (or (hashq-ref seen value)
            (let ((result (cons #f #f)))
              (hashq-set! seen value result)
              (set-car! result (copy (car value)))
              (set-cdr! result (copy (cdr value)))
              result)))
       (else value)))))
