;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private identifier)
  #:use-module (srfi srfi-1)
  #:export (math-identifier?
            math-identifier-start-char?
            math-identifier-char?
            math-digit-char?))

(define (letter? char)
  (memq (char-general-category char) '(Lu Ll Lt Lm Lo)))

(define (mark? char)
  (memq (char-general-category char) '(Mn Mc)))

(define (math-digit-char? char)
  (memq (char-general-category char) '(Nd Nl No)))

(define (math-identifier-start-char? char)
  (letter? char))

(define (math-identifier-char? char)
  (or (letter? char) (mark? char) (math-digit-char? char)))

(define (math-identifier? value)
  (and (string? value)
       (let ((characters (string->list value)))
         (and (pair? characters)
              (math-identifier-start-char? (car characters))
              (every math-identifier-char? (cdr characters))))))
