;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble source)
  #:export (source-range source-text))

(define current-ranges (make-parameter #f))

(define (key properties)
  (map (lambda (field) (assq-ref properties field)) '(filename line column)))

(define (complete-properties properties)
  (and properties
       (if (assq 'end-offset properties) properties
           (and (current-ranges) (hash-ref (current-ranges) (key properties))))))

;; Guile syntax objects keep only filename/line/column. Resolve those against
;; this evaluation's parsed ranges, never against a process-global file cache.
(define (call-with-source-context fragments thunk)
  (let ((ranges (make-hash-table)))
    (define (visit value)
      (when (or (pair? value) (string? value))
        (let ((properties (source-properties value)))
          (when (assq 'end-offset properties)
            (hash-set! ranges (key properties) properties))))
      (cond ((pair? value) (visit (car value)) (visit (cdr value)))
            ((vector? value) (for-each visit (vector->list value)))))
    (for-each visit fragments)
    (parameterize ((current-ranges ranges)) (thunk))))

(define (source-range properties)
  "Return the half-open range from SOURCE-PROPERTIES or SYNTAX-SOURCE.
Lines and columns are one-based; offsets count characters in the reader input.
Return #f when the reader did not supply a complete range."
  (let ((properties (complete-properties properties)))
    (and properties
       (assq 'end-offset properties)
       `((file . ,(assq-ref properties 'filename))
         (line . ,(+ 1 (assq-ref properties 'line)))
         (column . ,(+ 1 (assq-ref properties 'column)))
         (end-line . ,(+ 1 (assq-ref properties 'end-line)))
         (end-column . ,(+ 1 (assq-ref properties 'end-column)))
         (start-offset . ,(assq-ref properties 'start-offset))
         (end-offset . ,(assq-ref properties 'end-offset))))))

(define (source-text properties)
  "Return exact source text for a reader range, or #f.
Internal comments and spacing are retained. Leading/trailing comments outside
an expression are not included. No file is reopened."
  (let ((properties (complete-properties properties)))
    (and properties
       (assq 'source-buffer properties)
       (substring (assq-ref properties 'source-buffer)
                  (assq-ref properties 'start-offset)
                  (assq-ref properties 'end-offset)))))
