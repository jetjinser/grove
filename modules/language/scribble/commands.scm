;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir)
  #:use-module (srfi srfi-13))

(define-public (text-paragraphs value)
  (unless (or (string? value) (content-text? value))
    (error "text-paragraphs expects plain text" value))
  (let* ((text (if (content-text? value)
                   (content-text-value value)
                   value))
         (source (and (content-text? value)
                      (content-text-source value))))
    (define (make-paragraph-text lines)
      (make-content-text (string-join (reverse lines) " ") source))
    (let loop ((lines (string-split text #\newline))
               (current '())
               (paragraphs '()))
      (if (null? lines)
          (reverse
            (if (null? current)
                paragraphs
                (cons (make-paragraph-text current) paragraphs)))
          (let ((line (string-trim-both (car lines))))
            (if (string-null? line)
                (loop (cdr lines) '()
                      (if (null? current)
                          paragraphs
                          (cons (make-paragraph-text current) paragraphs)))
                (loop (cdr lines) (cons line current) paragraphs)))))))

(define-public (bold . body)
  (make-content-element 'bold body))

(define-public (verbatim . body)
  (make-content-element
    'verbatim
    (list (make-content-text (content-verbatim-string body)))
    #:flow-kind 'block))

(define-public (paragraph . body)
  (make-content-element 'paragraph body #:flow-kind 'block))

(define-public (heading . body)
  (make-content-element 'heading body #:flow-kind 'block))

(define-public (blockquote . body)
  (make-content-element 'blockquote body #:flow-kind 'block))

(define-public (itemize . body)
  (make-content-element 'itemize body #:flow-kind 'block))

(define-public (item . body)
  (make-content-element 'item body #:flow-kind 'block))

(define-public (code . body)
  (make-content-element 'code body))

(define-public (code-block . body)
  (make-content-element 'code-block body #:flow-kind 'block))

(define-public (math source-text)
  (make-content-element 'math (list source-text)))

(define-public (display-math source-text)
  (make-content-element 'math (list source-text)
                        #:properties '((display? . #t))
                        #:flow-kind 'block))
