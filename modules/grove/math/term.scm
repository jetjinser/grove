;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math term)
  #:use-module (grove math syntax)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-term?
            math-term-size
            math-term->source
            math-term-at-path
            math-term-paths
            math-path-label
            math-expression->term))

(define (math-term->source term)
  (unless (math-term? term) (error "invalid math term" term))
  (case (car term)
    ((var) (symbol->string (cadr term)))
    ((num) (cadr term))
    ((zero) "zero(vector)")
    ((+ - * / ^ = sub)
     (if (= (length term) 2)
         (format #f "-(~a)" (math-term->source (cadr term)))
         (format #f "(~a ~a ~a)" (math-term->source (cadr term))
                 (if (eq? (car term) 'sub) "_" (symbol->string (car term)))
                 (math-term->source (caddr term)))))
    (else (format #f "~a(~a)" (car term)
                  (string-join (map math-term->source (cdr term)) ", ")))))

(define (math-term-at-path term path)
  (and (math-term? term) (list? path)
       (every (lambda (index) (and (exact-integer? index) (>= index 0))) path)
       (let walk ((term term) (path path))
         (if (null? path) term
             (and (not (memq (car term) '(var num zero)))
                  (< (car path) (length (cdr term)))
                  (walk (list-ref (cdr term) (car path)) (cdr path)))))))

(define (math-term-paths term)
  (if (memq (car term) '(var num zero))
      '(())
      (cons '()
        (append-map
          (lambda (child index)
            (map (lambda (path) (cons index path)) (math-term-paths child)))
          (cdr term) (iota (length (cdr term)))))))

(define (math-path-label path)
  (cond
    ((null? path) "root")
    ((and (list? path)
          (every (lambda (index) (and (exact-integer? index) (>= index 0))) path))
     (string-join (map (lambda (index) (format #f "operand ~a" (+ index 1))) path)
                  " → "))
    (else (format #f "~s" path))))

(define (math-term-size term)
  (unless (math-term? term) (error "invalid math term" term))
  (if (memq (car term) '(var num zero))
      1
      (+ 1 (apply + (map math-term-size (cdr term))))))

(define (math-term? term)
  (cond
    ((and (list? term) (= (length term) 2) (eq? (car term) 'var))
     (symbol? (cadr term)))
    ((and (list? term) (= (length term) 2) (eq? (car term) 'num))
     (string? (cadr term)))
    ((and (list? term) (= (length term) 2) (eq? (car term) 'zero))
     (eq? (cadr term) 'vector))
    ((and (pair? term) (memq (car term) '(var num zero))) #f)
    ((not (list? term)) #f)
    ((not (pair? term)) #f)
    ((memq (car term) '(+ - * / ^ = sub))
     (and (= (length term) (if (and (eq? (car term) '-)
                                    (= (length term) 2))
                               2 3))
          (every math-term? (cdr term))))
    ((symbol? (car term)) (every math-term? (cdr term)))
    (else #f)))

(define (math-expression->term expression)
  (unless (math-expression? expression)
    (error "expected a parsed math expression" expression))
  (case (math-expression-kind expression)
    ((embedded) (math-expression-value expression))
    ((identifier) `(var ,(math-expression-value expression)))
    ((number) `(num ,(math-expression-value expression)))
    ((group) (math-expression->term (car (math-expression-children expression))))
    ((call)
     (let ((arguments (map math-expression->term
                           (math-expression-children expression))))
       (if (and (eq? (math-expression-value expression) 'zero)
                (equal? arguments '((var vector))))
           '(zero vector)
           (cons (math-expression-value expression) arguments))))
    ((negate)
     `(- ,(math-expression->term (car (math-expression-children expression)))))
    ((binary)
     (let ((operator (case (math-expression-value expression)
                       ((plus) '+) ((minus) '-) ((multiply) '*)
                       ((divide) '/) ((power) '^) ((subscript) 'sub)
                       ((equals) '=))))
       (cons operator (map math-expression->term
                           (math-expression-children expression)))))
    (else (error "unsupported math syntax node"
                 (math-expression-kind expression)))))
