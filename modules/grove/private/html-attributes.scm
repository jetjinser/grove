;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private html-attributes)
  #:use-module (grove private alist)
  #:use-module (srfi srfi-1)
  #:export (valid-html-attribute-list?))

(define (forbidden-html-attribute-name-character? character)
  (let ((code (char->integer character)))
    (or (< code #x20)
        (<= #x7f code #x9f)
        (memv character '(#\space #\" #\' #\> #\/ #\=))
        (<= #xfdd0 code #xfdef)
        (memv (logand code #xffff) '(#xfffe #xffff)))))

(define (html-attribute-name? name)
  (and (symbol? name)
       (let ((characters (string->list (symbol->string name))))
         (and (pair? characters)
              (every (lambda (character)
                       (not (forbidden-html-attribute-name-character?
                              character)))
                     characters)))))

(define (valid-html-attribute-list? attributes)
  (and (list? attributes)
       (every (lambda (entry)
                (and (pair? entry)
                     (html-attribute-name? (car entry))
                     (string? (cdr entry))))
              attributes)
       (unique-symbol-key-alist? attributes)))
