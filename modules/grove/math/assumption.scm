;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math assumption)
  #:use-module (grove math term)
  #:use-module (grove math diagnostic)
  #:use-module (grove math declaration)
  #:use-module (srfi srfi-1)
  #:export (math-assumption? math-assumption-diagnostics))

;; Facts are explicit declarations, regularity assertions or equations.
;; They never arise from notation or presentation preferences.
(define (math-assumption? fact)
  (and (math-term? fact)
       (case (car fact)
         ((scalar vector coordinate parameter) (eq? (car (cadr fact)) 'var))
         ((function) (math-function-declaration? fact))
         ((smooth) #t)
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
