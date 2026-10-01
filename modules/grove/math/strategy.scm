;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math strategy)
  #:use-module (grove math derivation)
  #:use-module (grove math term)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove relation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (math-selected-rules math-rewrite-answers
            search-math-terms simplify-math-term calculate-math-term collect-math-term
            math-search-report? math-search-report-candidates
            math-search-report-reason math-search-report-limits
            math-search-report-states math-search-report-complete?))

(define-record-type <math-search-report>
  (make-math-search-report candidates reason limits states)
  math-search-report?
  (candidates math-search-report-candidates)
  (reason math-search-report-reason)
  (limits math-search-report-limits)
  (states math-search-report-states))

(define (math-search-report-complete? report)
  (and (memq (math-search-report-reason report) '(exhausted normal-form)) #t))

(define (math-selected-rules selection defaults)
  (let ((names (if selection selection defaults)))
    (unless (and (list? names) (every math-rewrite-rule-ref names))
      (error "expected a list of registered math rule names" names))
    (delete-duplicates names)))

(define (allowed-ruleo names rule)
  (if (null? names)
      fail
      (conde
        ((== rule (car names)))
        ((allowed-ruleo (cdr names) rule)))))

(define (math-rewrite-answers assumptions term rules backward?)
  (run* (next rule path)
    (allowed-ruleo rules rule)
    (if backward?
        (unrewrite-stepo assumptions next term rule path)
        (rewrite-stepo assumptions term next rule path))))

(define (check-bound name value minimum)
  (unless (and (exact-integer? value) (>= value minimum))
    (error "invalid math strategy bound" name value minimum)))

(define (require-type assumptions term)
  (let ((typed (math-infer-type assumptions term)))
    (unless typed (error (math-type-diagnostic assumptions term)))
    (math-typed-term-type typed)))

(define* (search-math-terms start goal assumptions backward?
                            #:key (rules #f) (max-depth 8)
                                  (max-results 5) (max-states 256)
                                  (max-term-size 128))
  (check-bound 'max-depth max-depth 0)
  (check-bound 'max-results max-results 1)
  (check-bound 'max-states max-states 1)
  (check-bound 'max-term-size max-term-size 1)
  (let* ((type (require-type assumptions start))
         (goal-type (require-type assumptions goal))
         (rules (math-selected-rules rules
                  (if backward? math-backward-rules
                      (map math-rewrite-rule-name math-rewrite-rules)))))
    (unless (eq? type goal-type)
      (error "math search endpoints have different types" type goal-type))
    (define (finish candidates reason limits states)
      (make-math-search-report (reverse candidates) reason
                              (delete-duplicates limits) states))
    (if (or (> (math-term-size start) max-term-size)
            (> (math-term-size goal) max-term-size))
        (finish '() 'term-size-limit '(term-size-limit) 0)
        (let search ((queue (list (list start (list start) '() 0)))
                     (states 0) (candidates '()) (limits '()))
          (cond
            ((null? queue)
             (finish candidates
                     (cond ((memq 'depth-limit limits) 'depth-limit)
                           ((memq 'term-size-limit limits) 'term-size-limit)
                           (else 'exhausted))
                     limits states))
            ((>= (length candidates) max-results)
             (finish candidates 'result-limit (cons 'result-limit limits) states))
            ((>= states max-states)
             (finish candidates 'state-limit (cons 'state-limit limits) states))
            (else
             (let* ((state (car queue))
                    (current (car state))
                    (terms (cadr state))
                    (steps (caddr state))
                    (depth (cadddr state))
                    (rest (cdr queue)))
               (if (equal? current goal)
                   (search rest (+ states 1)
                     (cons (make-math-derivation
                             (if backward? terms (reverse terms))
                             (if backward? steps (reverse steps))
                             assumptions)
                           candidates)
                     limits)
                   (let* ((answers
                            (filter
                              (lambda (answer)
                                (let* ((next (car answer))
                                       (typed (math-infer-type assumptions next)))
                                  (and (not (member next terms)) typed
                                       (eq? (math-typed-term-type typed) type))))
                              (math-rewrite-answers assumptions current rules backward?)))
                          (within-size
                            (filter (lambda (answer)
                                      (<= (math-term-size (car answer)) max-term-size))
                                    answers))
                          (limits
                            (if (= (length answers) (length within-size))
                                limits (cons 'term-size-limit limits))))
                     (if (>= depth max-depth)
                         (search rest (+ states 1) candidates
                           (if (null? within-size) limits (cons 'depth-limit limits)))
                         (search
                           (append rest
                             (map (lambda (answer)
                                    (let ((next (car answer)))
                                      (list next (cons next terms)
                                        (cons (make-math-derivation-step
                                                (if backward? next current)
                                                (if backward? current next)
                                                (cadr answer) (caddr answer))
                                              steps)
                                        (+ depth 1))))
                                  within-size))
                           (+ states 1) candidates limits)))))))))))

(define* (simplify-math-term input assumptions #:key (rules #f) (max-steps 32))
  (check-bound 'max-steps max-steps 0)
  (let ((rules (math-selected-rules rules math-simplification-rules)))
    (unless (every (lambda (rule) (memq rule math-simplification-rules)) rules)
      (error "simplification accepts only arithmetic and collection rules" rules))
    (require-type assumptions input)
    (let normalize ((current input) (terms (list input)) (steps '()) (count 0))
      (let* ((answers (math-rewrite-answers assumptions current rules #f))
             (done? (null? answers)))
        (if (or done? (= count max-steps))
            (make-math-search-report
              (list (make-math-derivation (reverse terms) (reverse steps) assumptions))
              (if done? 'normal-form 'step-limit)
              (if done? '() '(step-limit)) count)
            (let* ((answer (car answers)) (next (car answer)))
              (when (member next terms)
                (error "simplification entered a rewrite cycle" (cadr answer)))
              (normalize next (cons next terms)
                (cons (make-math-derivation-step current next (cadr answer) (caddr answer))
                      steps)
                (+ count 1))))))))

(define* (calculate-math-term input assumptions #:key (max-steps 32))
  (simplify-math-term input assumptions #:rules math-calculation-rules #:max-steps max-steps))

(define* (collect-math-term input assumptions #:key (max-steps 32))
  (simplify-math-term input assumptions #:rules math-collection-rules #:max-steps max-steps))
