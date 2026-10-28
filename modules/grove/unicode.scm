;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove unicode)
  #:use-module ((scheme char) #:select (string-foldcase))
  #:use-module (ice-9 unicode)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (canonical-caseless-key
            grapheme-count))

(define (canonical-caseless-key text)
  "Return TEXT's canonical caseless key for Unicode identity matching.

Case folding and canonical composition make `STRASSE', `Straße', and a
decomposed `e' plus combining acute compare equal to their canonical
spellings."
  (unless (string? text)
    (error "canonical-caseless-key requires a string" text))
  (string-normalize-nfc (string-foldcase (string-normalize-nfd text))))

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
