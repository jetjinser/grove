;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math layout)
  #:use-module (grove ir)
  #:use-module (grove math term)
  #:export (math-term->layout))

(define (token kind value)
  (make-content-element 'math-token (list value)
                        #:properties `((token-kind . ,kind))))

(define (row children)
  (make-content-element 'math-row children))

(define (operator value)
  (token 'mo value))

(define* (math-term->layout term #:optional (types '()))
  (let ()
    (define (argument value)
      (make-content-element 'math-parens (list (lower value))))
    (define (operator-argument value)
      (if (and (pair? value) (memq (car value) '(var num zero)))
          (lower value)
          (argument value)))
    (define (lower value)
      (unless (math-term? value)
        (error "invalid math term" value))
      (cond
        ((and (pair? value) (eq? (car value) 'var))
         (let ((identifier (token 'mi (symbol->string (cadr value)))))
           (if (member `(vector ,value) types)
               (make-content-element 'math-vector (list identifier))
               identifier)))
        ((equal? value '(zero vector))
         (make-content-element 'math-vector (list (token 'mn "0"))))
        ((and (pair? value) (eq? (car value) 'num))
         (token 'mn (cadr value)))
        ((and (eq? (car value) 'curl) (= (length value) 2))
         (row (list (operator "∇") (operator "×")
                    (operator-argument (cadr value)))))
        ((and (eq? (car value) 'grad) (= (length value) 2))
         (row (list (operator "∇") (operator-argument (cadr value)))))
        ((and (eq? (car value) 'div) (= (length value) 2))
         (row (list (operator "∇") (operator "·")
                    (operator-argument (cadr value)))))
        ((and (eq? (car value) 'laplacian) (= (length value) 2))
         (row (list (token 'mi "Δ") (lower (cadr value)))))
        ((and (memq (car value) '(+ - * / =)) (= (length value) 3))
         (let ((symbol (case (car value)
                         ((+) "+") ((-) "−") ((*) "×")
                         ((/) "/") ((=) "="))))
           (if (eq? (car value) '/)
               (make-content-element
                 'math-frac
                 (list (row (list (lower (cadr value))))
                       (row (list (lower (caddr value))))))
               (row (list (lower (cadr value))
                          (token 'mo symbol)
                          (lower (caddr value)))))))
        ((and (eq? (car value) '^) (= (length value) 3))
         (make-content-element
           'math-sup
           (list (row (list (lower (cadr value))))
                 (row (list (lower (caddr value)))))))
        ((and (eq? (car value) 'sub) (= (length value) 3))
         (make-content-element
           'math-sub
           (list (row (list (lower (cadr value))))
                 (row (list (lower (caddr value)))))))
        ((and (eq? (car value) '-) (= (length value) 2))
         (row (list (token 'mo "−") (lower (cadr value)))))
        ((symbol? (car value))
         (let loop ((arguments (cdr value)) (first? #t) (children '()))
           (if (null? arguments)
               (row (append (list (token 'mi (symbol->string (car value)))
                                 (operator "⁡")
                                 (operator "("))
                            (reverse children)
                            (list (operator ")"))))
               (loop (cdr arguments)
                     #f
                     (cons (lower (car arguments))
                           (if first?
                               children
                               (cons (operator ",") children)))))))
        (else (error "no math layout lowering for term" value))))
    (lower term)))
