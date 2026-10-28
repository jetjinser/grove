;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private string-fun)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 textual-ports)
  #:export (non-empty-string? filesystem-path-string?
            ascii-alpha? ascii-digit? ascii-hex?
            include-text))

(define-inlinable (non-empty-string? value)
  (and (string? value) (not (string-null? value))))

(define-inlinable (filesystem-path-string? value)
  (and (non-empty-string? value)
       (not (string-index value #\nul))))

(define-inlinable (ascii-alpha? character)
  (or (and (char>=? character #\a) (char<=? character #\z))
      (and (char>=? character #\A) (char<=? character #\Z))))

(define-inlinable (ascii-digit? character)
  (and (char>=? character #\0) (char<=? character #\9)))

(define-inlinable (ascii-hex? character)
  (or (ascii-digit? character)
      (and (char>=? character #\A) (char<=? character #\F))
      (and (char>=? character #\a) (char<=? character #\f))))

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

