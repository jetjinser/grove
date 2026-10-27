;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pass)
  #:use-module (grove math derivation)
  #:use-module (grove math diagnostic)
  #:use-module (grove math report)
  #:use-module (grove math term)
  #:use-module (grove math types)
  #:use-module (grove math private assumption)
  #:use-module (grove math private domain)
  #:use-module (grove math premise)
  #:use-module (grove math rewrite)
  #:use-module (srfi srfi-1)
  #:export (run-math-pass run-math-passes math-execution-diagnostics check-math-bound))

(define (check-math-bound name value minimum)
  (unless (and (exact-integer? value) (>= value minimum))
    (error "invalid math strategy bound" name value minimum)))

(define (execution-diagnostic code message path)
  (make-math-diagnostic #f #f path code '() message))

(define* (math-execution-diagnostics assumptions term #:optional (name 'compute))
  (let ((preconditions (append (math-assumption-diagnostics assumptions)
                               (math-domain-diagnostics term name assumptions))))
    (if (pair? preconditions) preconditions
        (append
          (math-assumption-type-diagnostics assumptions)
          (if (math-infer-type assumptions term) '()
              (list (execution-diagnostic 'type-error (math-type-diagnostic assumptions term) '())))
          (math-definedness-diagnostics assumptions term)))))

;; One deterministic forward pass. Limits never discard a valid prefix or
;; include an oversized/ill-typed edge. Cycle detection is independent of size.
(define* (run-math-pass input assumptions #:key name rules
                        (max-steps 32) (max-term-size 2048)
                        (order 'relational)
                        (validate (lambda (term) '())))
  (check-math-bound 'max-steps max-steps 0)
  (check-math-bound 'max-term-size max-term-size 1)
  (unless (math-term? input) (error "invalid math term" input))
  (unless (memq order '(relational innermost)) (error "unknown rewrite order" order))
  (let ((rules (math-selected-rules rules math-default-rules)))
    (define (finish terms steps reason count diagnostics)
      (make-math-report name
        (list (make-math-derivation (reverse terms) (reverse steps) assumptions))
        reason (if (memq reason '(step-limit term-size-limit)) (list reason) '()) count
        #:diagnostics diagnostics))
    (let advance ((current input) (terms (list input)) (steps '()) (count 0))
      (let* ((domain-diagnostics (validate current))
             (diagnostics (if (pair? domain-diagnostics)
                              (append (math-assumption-diagnostics assumptions) domain-diagnostics)
                              (math-execution-diagnostics assumptions current name))))
        (cond
          ((pair? diagnostics)
           (finish terms steps (math-diagnostic-code (car diagnostics)) count diagnostics))
          ((> (math-term-size current) max-term-size)
           (finish terms steps 'term-size-limit count
             (list (execution-diagnostic 'term-size-limit "term-size limit reached" '()))))
          (else
           (let ((answers (if (eq? order 'innermost)
                              (let ((answer (math-rewrite-innermost assumptions current rules)))
                                (if answer (list answer) '()))
                              (math-rewrite-answers assumptions current rules #f #:max-results 1))))
             (cond
               ((null? answers) (finish terms steps 'normal-form count '()))
               ((= count max-steps)
                (finish terms steps 'step-limit count
                  (list (execution-diagnostic 'step-limit "step limit reached" (caddr (car answers))))))
               (else
                (let* ((answer (car answers)) (next (car answer))
                       (rule (cadr answer)) (path (caddr answer))
                       (diagnostics (math-execution-diagnostics assumptions next name)))
                  (cond
                    ((pair? diagnostics) (finish terms steps 'invalid-result count diagnostics))
                    ((> (math-term-size next) max-term-size)
                     (finish terms steps 'term-size-limit count
                       (list (execution-diagnostic 'term-size-limit "next term exceeds term-size limit" path))))
                    ((member next terms)
                     (finish terms steps 'rewrite-cycle count
                       (list (make-math-diagnostic (+ count 1) rule path 'rewrite-cycle '()
                               "rewrite would repeat an earlier term"))))
                    (else
                     (advance next (cons next terms)
                       (cons (make-math-derivation-step current next rule path) steps)
                       (+ count 1))))))))))))))

;; Compose named passes with the same limits. Each pass has its own step budget;
;; the aggregate certificate joins boundaries without inventing transitions.
(define* (run-math-passes input assumptions stages #:key name
                          (max-steps 32) (max-term-size 2048)
                          (order 'relational)
                          (validate-final (lambda (term) '())))
  (check-math-bound 'max-steps max-steps 0)
  (check-math-bound 'max-term-size max-term-size 1)
  (let run ((stages stages) (current input) (passes '())
            (terms (list input)) (steps '()) (count 0))
    (if (null? stages)
        (let ((diagnostics (validate-final current)))
          (make-math-report name (list (make-math-derivation terms steps assumptions))
            (if (null? diagnostics) 'normal-form (math-diagnostic-code (car diagnostics)))
            '() count #:passes (reverse passes) #:diagnostics diagnostics))
        (let* ((stage (car stages))
               (report (run-math-pass current assumptions #:name (car stage) #:rules (cdr stage)
                         #:order order
                         #:max-steps max-steps #:max-term-size max-term-size))
               (certificate (math-report-derivation report))
               (terms (append terms (cdr (math-derivation-terms certificate))))
               (steps (append steps (math-derivation-steps certificate)))
               (count (+ count (math-report-states report))))
          (if (math-report-complete? report)
              (run (cdr stages) (math-report-final report) (cons report passes) terms steps count)
              (make-math-report name (list (make-math-derivation terms steps assumptions))
                (math-report-reason report) (math-report-limits report) count
                #:diagnostics (math-report-diagnostics report)
                #:passes (reverse (cons report passes))))))))
