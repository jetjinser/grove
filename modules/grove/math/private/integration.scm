;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (grove math private integration)
  #:use-module (grove relation)
  #:use-module ((grove relation dsl) #:select (project))
  #:use-module (grove math private polynomial)
  #:use-module (grove math private declaration)
  #:use-module (grove math rule)
  #:use-module (grove math diagnostic)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (math-polynomial-rules polynomial-calculus-diagnostics))

(define (polynomial-result facts input)
  (match input
    (((or 'antiderivative 'poisson) expression ('var coordinate))
     (let ((p (math-term->polynomial expression facts)))
       (and p (member `(coordinate (var ,coordinate)) facts)
            (memq coordinate (polynomial-variables p))
            (let ((basis (find math-space-declaration? facts)))
              (and (or (eq? (car input) 'antiderivative)
                       (and basis (member `(var ,coordinate) (cdr basis))))
                   (let* ((result
                            (if (eq? (car input) 'antiderivative)
                                (polynomial-integral p coordinate)
                                (polynomial-poisson p
                                  (cons coordinate (remove (lambda (axis) (eq? axis coordinate)) (map cadr (cdr basis)))))))
                          (recovered
                            (if (eq? (car input) 'antiderivative)
                                (polynomial-partial result coordinate)
                                (fold polynomial-add (polynomial-scale p 0)
                                  (map (lambda (axis) (polynomial-partial (polynomial-partial result axis) axis))
                                       (map cadr (cdr basis)))))))
                     (and (polynomial-zero? (polynomial-add recovered (polynomial-scale p -1)))
                          (math-polynomial->term result))))))))
    (_ #f)))

(define (safe-result facts input)
  (catch 'polynomial-limit
    (lambda () (polynomial-result facts input))
    (lambda args #f)))

(define (calculus-rule name operator equation)
  (make-math-rewrite-rule name equation
    "Exact polynomials in independent coordinates and constant parameters; bounded sparse expansion" 'polynomial-calculus
    (lambda (facts input output)
      (fresh (expression coordinate)
        (== input `(,operator ,expression ,coordinate))
        (project (facts input)
          (let ((result (safe-result facts input)))
            (if result (== output result) fail)))))
    #:matcher
    (lambda (input)
      (and (pair? input) (eq? (car input) operator)
           `((coordinate ,(caddr input)) (smooth ,(cadr input)))))))

(define math-polynomial-rules
  (list (calculus-rule 'integrate-polynomial 'antiderivative
          "The unique polynomial primitive with zero value at the integration coordinate's origin")
        (calculus-rule 'solve-polynomial-poisson 'poisson
          "A polynomial particular solution of Delta u = f, with zero initial value and first derivative in the selected axis")
        (make-math-rewrite-rule 'expand-polynomial "Exact sparse polynomial expansion"
          "Exact polynomials in explicit coordinates/parameters; bounded expansion" 'polynomial-calculus
          (lambda (facts input output)
            (project (facts input)
              (let ((result (catch 'polynomial-limit
                              (lambda ()
                                (let ((p (math-term->polynomial input facts)))
                                  (and p (math-polynomial->term p))))
                              (lambda args #f))))
                (if (and result (not (equal? result input))) (== output result) fail))))
          #:matcher (lambda (input) `((smooth ,input))))))

(define (polynomial-calculus-diagnostics facts input)
  (catch 'polynomial-limit
    (lambda ()
      (if (polynomial-result facts input) '()
          (list (make-math-diagnostic #f #f '() 'unsupported-expression '()
                  "This pass requires an exact scalar polynomial in explicit coordinates/parameters and nonzero parameter-only denominators; Poisson also requires a Cartesian space containing the selected coordinate"))))
    (lambda (key message)
      (list (make-math-diagnostic #f #f '() 'polynomial-limit '() message)))))
