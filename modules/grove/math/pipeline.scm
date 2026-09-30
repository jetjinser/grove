;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove relation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (math-source->term
            math-derive
            math-derive-curl-curl
            math-search
            math-search-backward
            math-derivation-step?
            math-derivation-step-from
            math-derivation-step-to
            math-derivation-step-rule
            math-derivation-step-path
            math-search-result?
            math-search-result-terms
            math-search-result-steps
            math-search-result->content
            math-verify-step
            math-verify-derivation
            math-verification?
            math-verification-valid?
            math-verification-errors
            math-verification-derivation))

(define-record-type <math-derivation-step>
  (make-math-derivation-step from to rule path)
  math-derivation-step?
  (from math-derivation-step-from)
  (to math-derivation-step-to)
  (rule math-derivation-step-rule)
  (path math-derivation-step-path))

(define-record-type <math-search-result>
  (make-math-search-result terms steps assumptions)
  math-search-result?
  (terms math-search-result-terms)
  (steps math-search-result-steps)
  (assumptions math-search-result-assumptions))

(define-record-type <math-verification>
  (make-math-verification derivation errors)
  math-verification?
  (derivation math-verification-derivation)
  (errors math-verification-errors))

(define (math-verification-valid? verification)
  (and (math-verification? verification)
       (null? (math-verification-errors verification))))

(define (math-source->term source)
  (math-expression->term (parse-math source)))

(define (math-source->assumptions sources)
  (append-map
    (lambda (source)
      (map math-source->term
           (filter (lambda (part) (not (string-null? (string-trim-both part))))
                   (string-split source #\;))))
    sources))

(define (curl-curl-field term)
  (and (list? term) (= (length term) 2) (eq? (car term) 'curl)
       (let ((inner (cadr term)))
         (and (list? inner) (= (length inner) 2)
              (eq? (car inner) 'curl)
              (cadr inner)))))

(define (check-derivation-types! input assumptions)
  (unless (math-infer-type assumptions input)
    (error (math-type-diagnostic assumptions input)))
  (let ((field (curl-curl-field input)))
    (when (and field (not (member `(smooth ,field) assumptions)))
      (error "curl-curl-identity requires a smooth(~a) assumption"
             (if (and (pair? field) (eq? (car field) 'var))
                 (cadr field)
                 field)))))

(define (math-cell contents)
  (make-content-element 'math-cell contents))

(define (math-token kind value)
  (make-content-element 'math-token (list value)
                        #:properties `((token-kind . ,kind))))

(define (math-expression-children term assumptions)
  (let ((layout (math-term->layout term assumptions)))
    (if (eq? (content-element-kind layout) 'math-row)
        (content-element-children layout)
        (list layout))))

(define (math-row-first term assumptions)
  (make-content-element 'math-row
    (list (math-cell (math-expression-children term assumptions))
          (math-cell '())
          (math-cell (list (math-token 'mtext ""))))))

(define (path-label path)
  (if (null? path)
      "root"
      (string-join (map (lambda (index)
                          (format #f "operand ~a" (+ index 1)))
                        path)
                   " → ")))

(define (math-row-next term rule path assumptions)
  (make-content-element 'math-row
    (list (math-cell (list (make-content-element
                             'math-token (list "=")
                             #:properties '((token-kind . mo)))))
          (math-cell (math-expression-children term assumptions))
          (math-cell
            (list (math-token 'mtext
                    (format #f "~a (at ~a)"
                            (symbol->string rule) (path-label path))))))))

(define (math-search-result->content result)
  (unless (math-search-result? result)
    (error "expected a math search result" result))
  (let ((terms (math-search-result-terms result))
        (steps (math-search-result-steps result))
        (assumptions (math-search-result-assumptions result)))
    (make-content-element
      'math
      (list (make-content-element
              'math-align
              (cons (math-row-first (car terms) assumptions)
                    (map (lambda (step)
                           (math-row-next
                             (math-derivation-step-to step)
                             (math-derivation-step-rule step)
                             (math-derivation-step-path step)
                             assumptions))
                         steps))))
      #:properties '((display? . #t))
      #:flow-kind 'block)))

(define (math-verify-step assumptions from to rule path)
  (let ((metadata (math-rewrite-rule-ref rule)))
    (and metadata
         (math-rewrite-rule-condition-checked? metadata)
         (pair? (run 1 (verified?)
                 (rewrite-stepo assumptions from to rule path)
                 (== verified? #t))))))

(define (math-verify-derivation result)
  (unless (math-search-result? result)
    (error "expected a math search result" result))
  (let* ((assumptions (math-search-result-assumptions result))
         (terms (math-search-result-terms result))
         (steps (math-search-result-steps result))
         (errors
           (append
             (if (= (length terms) (+ (length steps) 1))
                 '()
                 (list "term and step counts do not form a complete chain"))
             (filter-map
               (lambda (term)
                 (and (not (math-infer-type assumptions term))
                      (format #f "term has no consistent type: ~s" term)))
               terms)
             (let loop ((steps steps) (terms terms) (index 1) (errors '()))
               (if (or (null? steps) (null? terms) (null? (cdr terms)))
                   (reverse errors)
                   (let* ((step (car steps))
                          (from (car terms))
                          (to (cadr terms))
                          (message
                            (cond
                              ((not (math-derivation-step? step))
                               (format #f "step ~a has an invalid record" index))
                              ((or (not (equal? (math-derivation-step-from step)
                                               from))
                                   (not (equal? (math-derivation-step-to step)
                                               to)))
                               (format #f "step ~a endpoints do not match the term chain"
                                       index))
                              ((not (math-verify-step
                                      assumptions
                                      (math-derivation-step-from step)
                                      (math-derivation-step-to step)
                                      (math-derivation-step-rule step)
                                      (math-derivation-step-path step)))
                               (format #f "step ~a does not satisfy its named rule, path, and premises"
                                       index))
                              (else #f))))
                     (loop (cdr steps) (cdr terms) (+ index 1)
                           (if message (cons message errors) errors))))))))
    (make-math-verification result errors)))

(define max-derivation-steps 32)

(define (derive-steps input assumptions)
  (let advance ((current input) (history (list input)) (steps 0))
    (let ((answers
            (run 1 (result rule path)
              (rewrite-stepo assumptions current result rule path))))
      (cond
        ((null? answers) (reverse history))
        ((= steps max-derivation-steps)
         (error "math derivation exceeded its step limit" max-derivation-steps))
        (else
         (let* ((answer (car answers))
                (next (car answer))
                (rule (cadr answer))
                (path (caddr answer)))
           (when (member next history)
             (error "math derivation entered a rewrite cycle" next))
           (advance next
                    (cons (list next rule path) history)
                    (+ steps 1))))))))

(define* (math-derive source #:key (assumptions '()))
  (let* ((input (math-source->term source))
         (assumptions (math-source->assumptions assumptions))
         (steps (derive-steps input assumptions)))
    (check-derivation-types! input assumptions)
    (unless (> (length steps) 1)
      (error "no math rewrite rule applies to the expression" input))
    (make-content-element
      'math
      (list (make-content-element
              'math-align
              (cons (math-row-first input assumptions)
                    (map (lambda (step)
                           (math-row-next (car step) (cadr step) (caddr step)
                                          assumptions))
                         (cdr steps)))))
      #:properties '((display? . #t))
      #:flow-kind 'block)))

(define* (math-derive-curl-curl source #:key (assumptions '()))
  (math-derive source #:assumptions assumptions))

(define (check-search-bound name value minimum)
  (unless (and (integer? value) (>= value minimum))
    (error "invalid math search bound" name value minimum)))

(define* (bounded-math-search start goal assumptions backward?
                              #:key (max-depth 8)
                                    (max-results 5)
                                    (max-states 256))
  (check-search-bound 'max-depth max-depth 0)
  (check-search-bound 'max-results max-results 1)
  (check-search-bound 'max-states max-states 1)
  (unless (math-infer-type assumptions start)
    (error "cannot type the math search starting term"
           (math-type-diagnostic assumptions start)))
  (unless (math-infer-type assumptions goal)
    (error "cannot type the math search goal term"
           (math-type-diagnostic assumptions goal)))
  (let search ((queue (list (list start (list start) '() 0 (list start))))
                 (visited-states 0)
                 (results '()))
      (cond
        ((null? queue) (reverse results))
        ((>= (length results) max-results) (reverse results))
        ((>= visited-states max-states)
         (error "math search exceeded its state limit"
                max-states "raise max-states to continue"))
        (else
         (let* ((state (car queue))
                (current (list-ref state 0))
                (terms-reversed (list-ref state 1))
                (steps-reversed (list-ref state 2))
                (depth (list-ref state 3))
                (history (list-ref state 4))
                (rest (cdr queue)))
           (if (equal? current goal)
               (search rest (+ visited-states 1)
                       (cons (make-math-search-result
                               (if backward?
                                   terms-reversed
                                   (reverse terms-reversed))
                               (if backward?
                                   steps-reversed
                                   (reverse steps-reversed))
                               assumptions)
                             results))
               (if (>= depth max-depth)
                   (search rest (+ visited-states 1) results)
                   (let* ((answers
                            (if backward?
                                (run* (next rule path)
                                  (unrewrite-stepo assumptions next current
                                                   rule path))
                                (run* (next rule path)
                                  (rewrite-stepo assumptions current
                                                 next rule path))))
                          (successors
                            (filter-map
                              (lambda (answer)
                                (let ((next (car answer)))
                                  (and (not (member next history))
                                       (list next
                                         (cons next terms-reversed)
                                         (cons
                                           (make-math-derivation-step
                                             (if backward? next current)
                                             (if backward? current next)
                                             (cadr answer) (caddr answer))
                                           steps-reversed)
                                         (+ depth 1)
                                         (cons next history)))))
                              answers)))
                     (search (append rest successors)
                             (+ visited-states 1)
                             results)))))))))

(define* (math-search source target
                      #:key (assumptions '())
                            (max-depth 8)
                            (max-results 5)
                            (max-states 256))
  (bounded-math-search
    (math-source->term source)
    (math-source->term target)
    (math-source->assumptions assumptions)
    #f
    #:max-depth max-depth
    #:max-results max-results
    #:max-states max-states))

(define* (math-search-backward target source
                               #:key (assumptions '())
                                     (max-depth 8)
                                     (max-results 5)
                                     (max-states 256))
  (bounded-math-search
    (math-source->term target)
    (math-source->term source)
    (math-source->assumptions assumptions)
    #t
    #:max-depth max-depth
    #:max-results max-results
    #:max-states max-states))
