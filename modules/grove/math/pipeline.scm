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
  #:use-module (srfi srfi-13)
  #:export (math-source->term
            math-derive
            math-derive-curl-curl))

(define (math-source->term source)
  (math-expression->term (parse-math source)))

(define (math-cell contents)
  (make-content-element 'math-cell contents))

(define (math-token kind value)
  (make-content-element 'math-token (list value)
                        #:properties `((token-kind . ,kind))))

(define (math-expression-children term)
  (let ((layout (math-term->layout term)))
    (if (eq? (content-element-kind layout) 'math-row)
        (content-element-children layout)
        (list layout))))

(define (math-row-first term)
  (make-content-element 'math-row
    (list (math-cell (math-expression-children term))
          (math-cell '())
          (math-cell (list (math-token 'mtext ""))))))

(define (rule-label rule)
  (case rule
    ((curl-curl-identity) "curl-curl identity")
    ((divergence-free) "divergence-free assumption")
    ((subtract-zero) "subtract zero")
    (else (symbol->string rule))))

(define (path-label path)
  (if (null? path)
      "root"
      (string-join (map (lambda (index)
                          (format #f "operand ~a" (+ index 1)))
                        path)
                   " → ")))

(define (math-row-next term rule path)
  (make-content-element 'math-row
    (list (math-cell (list (make-content-element
                             'math-token (list "=")
                             #:properties '((token-kind . mo)))))
          (math-cell (math-expression-children term))
          (math-cell
            (list (math-token 'mtext
                    (format #f "~a (at ~a)"
                            (rule-label rule) (path-label path))))))))

(define max-derivation-steps 32)

(define (derive-steps input assumptions)
  (let advance ((current input) (history (list input)) (steps 0))
    (let ((answers
            (run 1 (result rule path)
              (rewrite-stepo assumptions current result rule path))))
      (cond
        ((null? answers) (reverse history))
        ((= steps max-derivation-steps)
         (error "math derivation exceeded its step limit" max-derivation-steps))
        (else
         (let* ((answer (car answers))
                (next (car answer))
                (rule (cadr answer))
                (path (caddr answer)))
           (when (member next history)
             (error "math derivation entered a rewrite cycle" next))
           (advance next
                    (cons (list next rule path) history)
                    (+ steps 1))))))))

(define* (math-derive source #:key (assumptions '()))
  (let* ((input (math-source->term source))
         (assumptions (map math-source->term assumptions))
         (steps (derive-steps input assumptions)))
    (unless (> (length steps) 1)
      (error "no math rewrite rule applies to the expression" input))
    (make-content-element
      'math
      (list (make-content-element
              'math-align
              (cons (math-row-first input)
                    (map (lambda (step)
                           (math-row-next (car step) (cadr step) (caddr step)))
                         (cdr steps)))))
      #:properties '((display? . #t))
      #:flow-kind 'block)))

(define (math-derive-curl-curl source)
  (math-derive source))
