;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:export (math-source->term
            math-derive-curl-curl))

(define (math-source->term source)
  (math-expression->term (parse-math source)))

(define (math-row-for term)
  (let ((layout (math-term->layout term)))
    (make-content-element
      'math-row
      (list (make-content-element
              'math-cell
              (if (eq? (content-element-kind layout) 'math-row)
                  (content-element-children layout)
                  (list layout)))))))

(define (math-derive-curl-curl source)
  (let* ((input (math-source->term source))
         (output (rewrite-curl-curl input)))
    (unless output
      (error "curl-curl rule does not match the parsed math expression"
             input))
    (make-content-element
      'math
      (list (make-content-element
              'math-align
              (list (math-row-for input) (math-row-for output))))
      #:properties '((display? . #t))
      #:flow-kind 'block)))
