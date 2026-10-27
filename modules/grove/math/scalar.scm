;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math scalar)
  #:use-module (grove math private number)
  #:use-module (grove math term)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (scalar-normalize scalar-cancel scalar-expand scalar-canonical))


;; A product consists of an exact coefficient and positive natural powers of
;; opaque factors. Scalar typing is checked by the rule, never inferred here.
(define (product term)
  (match term
    (('* a b)
     (let ((a (product a)) (b (product b)))
       (cons (* (car a) (car b)) (append (cdr a) (cdr b)))))
    (('- a) (let ((a (product a))) (cons (- (car a)) (cdr a))))
    (('^ ('^ base ('num m)) ('num n))
     (let ((m (math-natural-value m)) (n (math-natural-value n)))
       (if (and m n) (product `(^ ,base (num ,(number->string (* m n)))))
           (cons 1 (list (cons term 1))))))
    (('^ base ('num exponent))
     (let ((n (math-natural-value exponent)))
       (if n (cons 1 (if (= n 0) '() (list (cons base n))))
           (cons 1 (list (cons term 1))))))
    (_ (let ((n (math-number-value term)))
         (if n (cons n '()) (cons 1 (list (cons term 1))))))))

(define (factor<? a b)
  (define (rank term)
    (case (car term) ((var) 0) ((+ - *) 2) (else 1)))
  (let ((a (car a)) (b (car b)))
    (or (< (rank a) (rank b))
        (and (= (rank a) (rank b))
             (string<? (math-term->source a) (math-term->source b))))))

(define (powers entries)
  (sort
    (map (lambda (base)
           (cons base (fold (lambda (entry n) (if (equal? base (car entry)) (+ n (cdr entry)) n)) 0 entries)))
         (delete-duplicates (map car entries) equal?))
    factor<?))

(define (join operator items identity)
  (if (null? items) identity
      (fold (lambda (item result)
              (if (and (eq? operator '+) (eq? (car item) '-) (= (length item) 2))
                  `(- ,result ,(cadr item)) `(,operator ,result ,item)))
            (car items) (cdr items))))

(define (build-product coefficient entries)
  (let* ((terms (filter-map (lambda (entry)
                            (cond ((= (cdr entry) 0) #f)
                                  ((= (cdr entry) 1) (car entry))
                                  (else `(^ ,(car entry) (num ,(number->string (cdr entry)))))))
                          entries))
         (body (join '* terms '(num "1"))))
    (cond ((= coefficient 0) '(num "0"))
          ((null? terms) (math-number->term coefficient))
          ((= coefficient 1) body)
          ((= coefficient -1) `(- ,body))
          ((< coefficient 0) `(- ,(join '* (cons (math-number->term (- coefficient)) terms) '(num "1"))))
          (else (join '* (cons (math-number->term coefficient) terms) '(num "1"))))))

(define (summands term sign)
  (match term
    (('+ a b) (append (summands a sign) (summands b sign)))
    (('- a b) (append (summands a sign) (summands b (- sign))))
    (('- a) (summands a (- sign)))
    (_ (let* ((p (product term)) (body (build-product 1 (powers (cdr p)))))
         (list (cons body (* sign (car p))))))))

(define (degree term)
  (fold (lambda (factor n)
          (+ n (if (eq? (caar factor) 'var) (cdr factor) 0))) 0 (cdr (product term))))

(define (summand<? a b)
  (cond ((> (degree a) (degree b)) #t)
        ((< (degree a) (degree b)) #f)
        ((equal? a '(num "1")) #f)
        ((equal? b '(num "1")) #t)
        (else (string<? (math-term->source a) (math-term->source b)))))

(define (scalar-normalize input)
  (define result
    (match input
      (((or '+ '-) . operands)
       (let* ((entries (summands input 1))
              (keys (sort (delete-duplicates (map car entries) equal?) summand<?))
              (terms (filter-map
                       (lambda (key)
                         (let ((n (fold (lambda (entry n) (if (equal? key (car entry)) (+ n (cdr entry)) n)) 0 entries)))
                           (and (not (= n 0)) (build-product n (cdr (product key)))))) keys)))
         (join '+ terms '(num "0"))))
      (('* a b) (let ((p (product input))) (build-product (car p) (powers (cdr p)))))
      (('^ ('^ base ('num m)) ('num n))
       (let ((m (math-natural-value m)) (n (math-natural-value n)))
         (if (and m n) `(^ ,base (num ,(number->string (* m n)))) input)))
      (_ input)))
  (and (not (equal? result input)) result))

(define (scalar-cancel input)
  (match input
    (('/ numerator denominator)
     (let* ((n (product numerator)) (d (product denominator))
            (np (powers (cdr n))) (dp (powers (cdr d)))
            (bases (delete-duplicates (append (map car np) (map car dp)) equal?))
            (remaining (map (lambda (base) (cons base (- (or (assoc-ref np base) 0) (or (assoc-ref dp base) 0)))) bases)))
       (and (not (= (car d) 0))
            (let* ((top (build-product (/ (car n) (car d))
                           (sort (filter (lambda (p) (> (cdr p) 0)) remaining) factor<?)))
                   (bottom (build-product 1
                             (sort (map (lambda (p) (cons (car p) (- (cdr p))))
                                        (filter (lambda (p) (< (cdr p) 0)) remaining)) factor<?)))
                   (result (if (or (equal? top '(num "0")) (equal? bottom '(num "1"))) top `(/ ,top ,bottom))))
              (and (not (equal? input result)) result)))))
    (_ #f)))

;; Expansion has a separate rule group and is never part of simplification.
(define (scalar-expand input)
  (match input
    (('* ('+ a b) c) `(+ (* ,a ,c) (* ,b ,c)))
    (('* a ('+ b c)) `(+ (* ,a ,b) (* ,a ,c)))
    (('* ('- a b) c) `(- (* ,a ,c) (* ,b ,c)))
    (('* a ('- b c)) `(- (* ,a ,b) (* ,a ,c)))
    (_ #f)))

;; Only assumption-free algebra is used for comparing scalar domain premises.
;; In particular, cancellation never participates in this equivalence check.
(define (scalar-canonical term)
  (if (memq (car term) '(var num zero)) term
      (let ((term (cons (car term) (map scalar-canonical (cdr term)))))
        (or (scalar-normalize term) term))))
