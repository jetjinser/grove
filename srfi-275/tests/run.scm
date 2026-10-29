;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(use-modules (srfi srfi-1)
             (srfi srfi-4)
             (srfi srfi-13)
             (srfi srfi-14)
             (srfi srfi-64)
             (rnrs bytevectors)
             ((ice-9 match) #:select (match match-lambda*))
             (srfi :275 iri)
             (srfi :275 uri)
             (srfi :275 normalize)
             (srfi :275 path)
             (srfi :275 utils)
             ((srfi :275 internal resource-identifier)
              #:select (resource-identifier-raw-scheme
                        resource-identifier-raw-user
                        resource-identifier-raw-host
                        resource-identifier-port resource-identifier-path
                        resource-identifier-raw-query
                        resource-identifier-raw-fragment
                        relative-reference-raw-user
                        relative-reference-raw-host
                        relative-reference-port relative-reference-path
                        relative-reference-raw-query
                        relative-reference-raw-fragment))
             ((srfi :275 internal iri)
              #:select (make-raw-relative-iri make-raw-iri))
             ((srfi :275 internal uri)
              #:select (make-raw-relative-uri make-raw-uri)))

(define guile-format format)
(define (format template . args)
  (apply guile-format #f template args))

(define-syntax match-values
  (syntax-rules ()
    ((_ expression clause ...)
     (call-with-values (lambda () expression)
       (lambda values (match values clause ...))))))

(define-syntax match-lambda
  (syntax-rules ()
    ((_ clause ...)
     (lambda arguments (match arguments clause ...)))))

(define (flatten lists0)
  (let loop ((lists lists0) (rest '()))
    (cond
     ((null? lists) rest)
     ((and (pair? (car lists)) (null? (cdar lists)))
      (cons (caar lists) (loop (cdr lists) rest)))
     ((pair? (car lists))
      (cons (caar lists) (loop (cons (cdar lists) (cdr lists)) rest)))
     (else (cons (car lists) (loop (cdr lists) rest))))))

(define (identity x) x)

;; The Chez source tests use R6RS list quantifiers.
(define for-all every)
(define exists any)

(define (compose f . rest)
  (if (null? rest)
      f
      (let ((g (apply compose rest)))
        (lambda args (call-with-values (lambda () (apply g args)) f)))))

(define uri-path-string (compose path->string uri-path))
(define iri-path-string (compose path->string iri-path))
(define uri-path-segments (compose path-segments uri-path))
(define iri-path-segments (compose path-segments iri-path))

(define (shift-percent-encoding k) (+ k 256))
(define (unshift-percent-encoding sk) (- sk 256))

(define (char->u8-list ch)
  (bytevector->u8-list (string->utf8 (string ch))))

(define (hex-digit n) (if (< n 10) (+ 48 n) (+ 55 n)))

(define (percent-encoding->u8-list %enc)
  (let ((char0 (quotient %enc 16))
        (char1 (remainder %enc 16)))
    (list 37 (hex-digit char0) (hex-digit char1))))

(define (percent-encoding->string %enc)
  (utf8->string (u8-list->bytevector (percent-encoding->u8-list %enc))))

(define (u8-list->string octets)
  (utf8->string (u8-list->bytevector octets)))

;; SRFI-64 creates a fresh simple runner for each completed suite.  Preserve
;; failures across all seven source files so a later passing suite cannot hide
;; an earlier failure.
(define total-unexpected-failures 0)
(test-runner-factory
 (lambda ()
   (let* ((runner (test-runner-simple))
          (finish (test-runner-on-final runner)))
     (test-runner-on-final!
      runner
      (lambda (current)
        (set! total-unexpected-failures
              (+ total-unexpected-failures
                 (test-runner-fail-count current)
                 (test-runner-xpass-count current)))
        (finish current)))
     runner)))

(for-each load
          '("tests/upstream/test-misc.scm"
            "tests/guile/test-parse-segment.scm"
            "tests/guile/test-escaped.scm"
            "tests/upstream/test-path.scm"
            "tests/guile/test-relative.scm"
            "tests/upstream/test-setters.scm"
            "tests/guile/test-normalize.scm"))

(unless (zero? total-unexpected-failures)
  (exit 1))
