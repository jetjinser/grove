;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math domain)
  #:use-module (grove math term)
  #:use-module (grove math operator)
  #:use-module (grove math diagnostic)
  #:use-module (grove math declaration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-domain-diagnostics))

;; Structural validity is broader than the executable vocabulary. Unknown
;; function applications remain representable and displayable, not computable.
(define* (math-domain-diagnostics term #:optional (pass 'compute) (assumptions '()))
  (filter-map
    (lambda (path)
      (let* ((local (math-term-at-path term path)) (name (car local)))
        (cond
          ((and (eq? name 'num)
                (or (string-null? (cadr local))
                    (not (string-every (lambda (char) (char<=? #\0 char #\9)) (cadr local)))))
           (make-math-diagnostic #f #f path 'unsupported-expression '()
             (format #f "~a requires decimal integer literals; ~s can still be displayed" pass (cadr local))))
          ((and (eq? name '^)
                (not (and (eq? (car (caddr local)) 'num)
                          (not (string-null? (cadr (caddr local))))
                          (string-every (lambda (c) (char<=? #\0 c #\9)) (cadr (caddr local))))))
           (make-math-diagnostic #f #f (append path '(1)) 'unsupported-expression '()
             "computation currently supports only natural-number exponents"))
          ((and (not (memq name '(var num zero)))
             (not (any (lambda (signature) (eq? name (car signature)))
                       math-operator-signatures))
             (not (math-function-signature assumptions name)))
             (make-math-diagnostic #f #f path 'unsupported-expression '()
               (format #f "~a does not support operator ~a; this expression can still be displayed" pass name)))
          (else #f))))
    (math-term-paths term)))
