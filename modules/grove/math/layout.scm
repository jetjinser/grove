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

(define (argument value)
  (make-content-element 'math-parens (list (math-term->layout value))))

(define (math-term->layout term)
  (unless (math-term? term)
    (error "invalid math term" term))
  (cond
    ((and (pair? term) (eq? (car term) 'var))
     (token 'mi (symbol->string (cadr term))))
    ((and (pair? term) (eq? (car term) 'num))
     (token 'mn (cadr term)))
    ((and (eq? (car term) 'curl) (= (length term) 2))
     (row (list (token 'mi "curl") (argument (cadr term)))))
    ((and (eq? (car term) 'grad) (= (length term) 2))
     (row (list (token 'mi "grad") (argument (cadr term)))))
    ((and (eq? (car term) 'div) (= (length term) 2))
     (row (list (token 'mi "div") (argument (cadr term)))))
    ((and (eq? (car term) 'laplacian) (= (length term) 2))
     (row (list (token 'mi "Δ") (math-term->layout (cadr term)))))
    ((and (memq (car term) '(+ - * /)) (= (length term) 3))
     (let ((symbol (case (car term)
                     ((+) "+") ((-) "−") ((*) "×") ((/) "/"))))
       (if (eq? (car term) '/)
           (make-content-element
             'math-frac
             (list (row (list (math-term->layout (cadr term))))
                   (row (list (math-term->layout (caddr term))))))
           (row (list (math-term->layout (cadr term))
                      (token 'mo symbol)
                      (math-term->layout (caddr term)))))))
    ((and (eq? (car term) '^) (= (length term) 3))
     (make-content-element
       'math-sup
       (list (row (list (math-term->layout (cadr term))))
             (row (list (math-term->layout (caddr term)))))))
    ((and (eq? (car term) '-) (= (length term) 2))
     (row (list (token 'mo "−") (math-term->layout (cadr term)))))
    ((symbol? (car term))
     (let loop ((arguments (cdr term))
                (first? #t)
                (children (list (token 'mi (symbol->string (car term))))))
       (if (null? arguments)
           (row (reverse children))
           (loop (cdr arguments)
                 #f
                 (cons (argument (car arguments))
                       (if first?
                           children
                           (cons (token 'mo ",") children)))))))
    (else (error "no math layout lowering for term" term))))
