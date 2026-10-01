;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math assumption)
  #:use-module (grove math term)
  #:use-module (grove math diagnostic)
  #:use-module (grove math declaration)
  #:use-module (grove math number)
  #:use-module (srfi srfi-1)
  #:export (math-assumption? math-assumption-diagnostics))

;; Facts are explicit declarations, regularity assertions or equations.
;; They never arise from notation or presentation preferences.
(define (math-assumption? fact)
  (and (math-term? fact)
       (case (car fact)
         ((scalar vector coordinate parameter) (eq? (car (cadr fact)) 'var))
         ((function) (math-function-declaration? fact))
         ((field) (math-field-declaration? fact))
         ((space) (math-space-declaration? fact))
         ((smooth c2 differentiable nonzero positive) #t)
         ((=) #t)
         (else #f))))

(define (math-assumption-diagnostics facts)
  (cond
    ((not (list? facts))
     (list (make-math-diagnostic #f #f '() 'invalid-assumption '()
             "assumptions must be a list of facts")))
    (else
     (append
       (filter-map
         (lambda (fact)
           (and (math-space-declaration? fact)
                (any (lambda (other) (and (math-space-declaration? other) (not (equal? other fact)))) facts)
                (make-math-diagnostic #f #f '() 'invalid-assumption (list fact)
                  "one scope must use a single ordered Cartesian coordinate space")))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (math-field-declaration? fact)
                (or (any (lambda (other)
                           (and (or (math-field-declaration? other) (math-function-declaration? other))
                                (equal? (cadr other) (cadr fact)) (not (equal? other fact)))) facts)
                    (math-scalar-declared? facts (cadr fact)) (member `(vector ,(cadr fact)) facts)
                    (any (lambda (space) (and (math-space-declaration? space)
                                             (not (equal? (cdr space) (cdddr fact))))) facts))
                (make-math-diagnostic #f #f '() 'invalid-assumption (list fact)
                  "field declaration conflicts with its type, signature or coordinate space")))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (math-space-declaration? fact)
                (any (lambda (coordinate)
                       (not (eq? (math-variable-role facts coordinate) 'coordinate))) (cdr fact))
                (make-math-diagnostic #f #f '() 'invalid-assumption (list fact)
                  "Cartesian space axes must be declared independent coordinates")))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (math-assumption? fact) (memq (car fact) '(nonzero positive))
                (let ((value (math-number-value (cadr fact))))
                  (and value (if (eq? (car fact) 'positive) (<= value 0) (= value 0))))
                (make-math-diagnostic #f #f '() 'invalid-assumption (list fact)
                  (format #f "asserted premise contradicts an exact constant: ~a" (math-term->source fact)))))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (not (math-assumption? fact))
                (make-math-diagnostic #f #f '() 'invalid-assumption (list fact)
                  (format #f "expected a variable declaration, function signature, smoothness fact, or equation; got ~s" fact))))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (math-assumption? fact) (memq (car fact) '(scalar coordinate parameter))
                (member `(vector ,(cadr fact)) facts)
                (make-math-diagnostic #f #f '() 'type-error
                  (list fact `(vector ,(cadr fact)))
                  (format #f "variable ~a has conflicting scalar and vector declarations"
                          (cadadr fact)))))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (math-assumption? fact) (eq? (car fact) 'coordinate)
                (member `(parameter ,(cadr fact)) facts)
                (make-math-diagnostic #f #f '() 'invalid-assumption
                  (list fact `(parameter ,(cadr fact)))
                  "a symbol cannot be both an independent coordinate and a constant parameter")))
         (delete-duplicates facts equal?))
       (filter-map
         (lambda (fact)
           (and (math-function-declaration? fact)
                (or (math-scalar-declared? facts (cadr fact)) (member `(vector ,(cadr fact)) facts)
                    (any (lambda (other) (and (math-function-declaration? other)
                                             (equal? (cadr other) (cadr fact))
                                             (not (equal? other fact)))) facts))
                (make-math-diagnostic #f #f '() 'invalid-assumption (list fact)
                  (format #f "function ~a has conflicting declarations" (cadadr fact)))))
         (delete-duplicates facts equal?))))))
