;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math strategy)
  #:use-module (grove math derivation)
  #:use-module (grove math term)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove math pass)
  #:use-module (grove math report)
  #:use-module (grove math diagnostic)
  #:use-module (srfi srfi-1)
  #:export (search-math-terms simplify-math-term calculate-math-term collect-math-term)
  #:re-export (math-selected-rules math-rewrite-answers
            math-report? math-report-candidates
            math-report-reason math-report-limits
            math-report-states math-report-complete?))

(define (require-type assumptions term)
  (let ((typed (math-infer-type assumptions term)))
    (unless typed (error (math-type-diagnostic assumptions term)))
    (math-typed-term-type typed)))

(define* (search-typed-math-terms start goal assumptions backward?
                            #:key (rules #f) (max-depth 8)
                                  (max-results 5) (max-states 256)
                                  (max-term-size 128))
  (let* ((type (require-type assumptions start))
         (goal-type (require-type assumptions goal))
         (rules (math-selected-rules rules
                  (if backward? math-backward-rules
                      (map math-rewrite-rule-name math-rewrite-rules)))))
    (unless (eq? type goal-type)
      (error "math search endpoints have different types" type goal-type))
    (define (finish candidates reason limits states)
      (make-math-report (if backward? 'search-backward 'search) (reverse candidates) reason
                              (delete-duplicates limits) states
        #:diagnostics (if (eq? reason 'exhausted) '()
                          (list (make-math-diagnostic #f #f '() reason '()
                                  (format #f "search stopped at ~a" reason))))))
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
                                       (null? (math-execution-diagnostics assumptions next))
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

(define* (search-math-terms start goal assumptions backward?
                            #:key (rules #f) (max-depth 8)
                                  (max-results 5) (max-states 256) (max-term-size 128))
  (check-math-bound 'max-depth max-depth 0)
  (check-math-bound 'max-results max-results 1)
  (check-math-bound 'max-states max-states 1)
  (check-math-bound 'max-term-size max-term-size 1)
  (math-selected-rules rules (if backward? math-backward-rules
                               (map math-rewrite-rule-name math-rewrite-rules)))
  (let* ((diagnostics (append (math-execution-diagnostics assumptions start)
                              (math-execution-diagnostics assumptions goal)))
         (diagnostics
           (if (or (pair? diagnostics)
                   (eq? (math-typed-term-type (math-infer-type assumptions start))
                        (math-typed-term-type (math-infer-type assumptions goal))))
               diagnostics
               (list (make-math-diagnostic #f #f '() 'type-change '()
                       "math search endpoints have different types")))))
    (if (pair? diagnostics)
        (make-math-report (if backward? 'search-backward 'search) '()
          (math-diagnostic-code (car diagnostics)) '() 0 #:diagnostics diagnostics)
        (search-typed-math-terms start goal assumptions backward?
          #:rules rules #:max-depth max-depth #:max-results max-results
          #:max-states max-states #:max-term-size max-term-size))))

(define* (simplify-math-term input assumptions #:key (rules #f) (max-steps 32)
                           (max-term-size 2048))
  (let ((rules (math-selected-rules rules math-simplification-rules)))
    (unless (every (lambda (rule) (memq rule math-simplification-rules)) rules)
      (error "simplification accepts only arithmetic and collection rules" rules))
    (run-math-pass input assumptions #:name 'simplify #:rules rules
      #:max-steps max-steps #:max-term-size max-term-size)))

(define* (calculate-math-term input assumptions #:key (max-steps 32) (max-term-size 2048))
  (run-math-pass input assumptions #:name 'calculate #:rules math-calculation-rules
    #:max-steps max-steps #:max-term-size max-term-size))

(define* (collect-math-term input assumptions #:key (max-steps 32) (max-term-size 2048))
  (run-math-pass input assumptions #:name 'collect #:rules math-collection-rules
    #:max-steps max-steps #:max-term-size max-term-size))
