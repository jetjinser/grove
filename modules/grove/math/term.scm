;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math term)
  #:use-module (ice-9 match)
  #:use-module (grove math operator)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-term?
            math-term-size
            math-term->source
            math-term-at-path
            math-term-replace-at-path
            math-term-paths
            math-path-label))

(define (math-term->source term)
  (unless (math-term? term) (error "invalid math term" term))
  (match term
    (('var name) (symbol->string name))
    (('num number) number)
    (('zero 'vector) "zero(vector)")
    ((operator operand)
     (if (eq? operator '-)
         (format #f "-(~a)" (math-term->source operand))
         (format #f "~a(~a)" operator (math-term->source operand))))
    ((operator left right)
     (if (memq operator '(+ - * / ^ =))
         (format #f "(~a ~a ~a)" (math-term->source left)
                 operator (math-term->source right))
         (format #f "~a(~a, ~a)" operator
                 (math-term->source left) (math-term->source right))))
    ((operator . operands)
     (format #f "~a(~a)" operator
             (string-join (map math-term->source operands) ", ")))))

(define (math-term-at-path term path)
  (and (math-term? term) (list? path)
       (every (lambda (index) (and (exact-integer? index) (>= index 0))) path)
       (let walk ((term term) (path path))
         (if (null? path) term
             (and (not (memq (car term) '(var num zero)))
                  (< (car path) (length (cdr term)))
                  (walk (list-ref (cdr term) (car path)) (cdr path)))))))

(define (math-term-replace-at-path term path replacement)
  (and (math-term-at-path term path)
       (let replace ((term term) (path path))
         (if (null? path)
             replacement
             (let ((index (car path)))
               (cons (car term)
                 (map (lambda (child position)
                        (if (= position index)
                            (replace child (cdr path))
                            child))
                      (cdr term) (iota (length (cdr term))))))))))

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
  (match term
    (('var (? symbol?)) #t)
    (('num (? string?)) #t)
    (('zero 'vector) #t)
    (((or 'var 'num 'zero) . _) #f)
    (('function _ _ . operands)
     (and (list? operands) (every math-term? operands)))
    ((operator . operands)
     (and (symbol? operator)
          (list? operands)
          (if (math-operator-arities operator)
              (and (memv (length operands) (math-operator-arities operator))
                   (every math-term? operands))
              (every math-term? operands))))
    (_ #f)))
