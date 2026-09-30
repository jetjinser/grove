;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:use-module (grove relation)
  #:use-module (srfi srfi-1)
  #:export (math-source->term
            math-derive
            math-derive-curl-curl))

(define (math-source->term source)
  (math-expression->term (parse-math source)))

(define (math-cell contents)
  (make-content-element 'math-cell contents))

(define (math-expression-children term)
  (let ((layout (math-term->layout term)))
    (if (eq? (content-element-kind layout) 'math-row)
        (content-element-children layout)
        (list layout))))

(define (math-row-first term)
  (make-content-element 'math-row
    (list (math-cell (math-expression-children term))
          (math-cell '()))))

(define (math-row-next term)
  (make-content-element 'math-row
    (list (math-cell (list (make-content-element
                             'math-token (list "=")
                             #:properties '((token-kind . mo)))))
          (math-cell (math-expression-children term)))))

(define max-derivation-steps 32)

(define (derive-terms input assumptions)
  (let advance ((current input) (history (list input)) (steps 0))
    (let ((answers
            (run 1 (result) (rewriteo assumptions current result))))
      (cond
        ((null? answers) (reverse history))
        ((= steps max-derivation-steps)
         (error "math derivation exceeded its step limit" max-derivation-steps))
        (else
         (let ((next (car answers)))
           (when (member next history)
             (error "math derivation entered a rewrite cycle" next))
           (advance next (cons next history) (+ steps 1))))))))

(define* (math-derive source #:key (assumptions '()))
  (let* ((input (math-source->term source))
         (assumptions (map math-source->term assumptions))
         (terms (derive-terms input assumptions)))
    (unless (> (length terms) 1)
      (error "no math rewrite rule applies to the expression" input))
    (make-content-element
      'math
      (list (make-content-element
              'math-align
              (cons (math-row-first (car terms))
                    (map math-row-next (cdr terms)))))
      #:properties '((display? . #t))
      #:flow-kind 'block)))

(define (math-derive-curl-curl source)
  (math-derive source))
