;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove string-fun)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (non-empty-string?
            grapheme-count
            include-text))

(define-inlinable (non-empty-string? value)
  (and (string? value) (not (string-null? value))))

(define u8-grapheme-breaks
  (pointer->procedure void
                      (dynamic-func "u8_grapheme_breaks" (dynamic-link))
                      (list '* size_t '*)))

(define (grapheme-count text)
  "Return the number of Unicode extended grapheme clusters in TEXT."
  (unless (string? text)
    (error "grapheme-count expects a string" text))
  (let* ((utf8 (string->utf8 text))
         (length (bytevector-length utf8)))
    (if (zero? length)
        0
        (let ((breaks (make-bytevector length 0)))
          (u8-grapheme-breaks (bytevector->pointer utf8)
                              length
                              (bytevector->pointer breaks))
          (let loop ((index 0) (count 0))
            (if (= index length)
                count
                (loop (+ index 1)
                      (+ count (if (= 1 (bytevector-u8-ref breaks index))
                                   1 0)))))))))

(define-syntax include-text
  (lambda (stx)
    (syntax-case stx ()
      [(_ filename)
       (let* ([path   (syntax->datum #'filename)]
              [source (search-path %load-path path)])
         (unless source
           (syntax-violation 'include-text "text file not found in load path"
                             stx #'filename))
         (datum->syntax stx
           `(quote ,(call-with-include-port
                      (datum->syntax #'filename (canonicalize-path source))
                      get-string-all))))])))

