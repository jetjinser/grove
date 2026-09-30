;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove math derivation)
  #:use-module (grove math strategy)
  #:use-module (grove relation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (math-source->term math-derive math-derive-curl-curl
            math-search math-search-backward math-simplify
            math-search-result->content math-verify-step math-verify-derivation
            math-verification? math-verification-valid?
            math-verification-errors math-verification-derivation)
  #:re-export (math-derivation-step? math-derivation-step-from
               math-derivation-step-to math-derivation-step-rule
               math-derivation-step-path math-search-result?
               math-search-result-terms math-search-result-steps
               math-search-result-assumptions
               math-search-report? math-search-report-candidates
               math-search-report-reason math-search-report-limits
               math-search-report-states math-search-report-complete?))

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
  (let ((metadata (math-rewrite-rule-ref rule))
        (from-type (math-infer-type assumptions from))
        (to-type (math-infer-type assumptions to)))
    (and metadata
         from-type to-type
         (eq? (math-typed-term-type from-type)
              (math-typed-term-type to-type))
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

(define (derive-steps input assumptions rules)
  (let advance ((current input) (history (list input)) (steps 0))
    (let ((answers
            (math-rewrite-answers assumptions current rules #f)))
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

(define* (math-derive source #:key (assumptions '()) (rules #f))
  (let* ((input (math-source->term source))
         (assumptions (math-source->assumptions assumptions))
         (rules (math-selected-rules rules
                  (map math-rewrite-rule-name math-rewrite-rules)))
         (steps (derive-steps input assumptions rules)))
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

(define* (math-search source target
                      #:key (assumptions '()) (rules #f)
                            (max-depth 8) (max-results 5)
                            (max-states 256) (max-term-size 128))
  (search-math-terms
    (math-source->term source) (math-source->term target)
    (math-source->assumptions assumptions) #f
    #:rules rules #:max-depth max-depth #:max-results max-results
    #:max-states max-states #:max-term-size max-term-size))

(define* (math-search-backward target source
                               #:key (assumptions '()) (rules #f)
                                     (max-depth 8) (max-results 5)
                                     (max-states 256) (max-term-size 128))
  (search-math-terms
    (math-source->term target) (math-source->term source)
    (math-source->assumptions assumptions) #t
    #:rules rules #:max-depth max-depth #:max-results max-results
    #:max-states max-states #:max-term-size max-term-size))

(define* (math-simplify source #:key (assumptions '()) (rules #f) (max-steps 32))
  (simplify-math-term (math-source->term source)
    (math-source->assumptions assumptions) #:rules rules #:max-steps max-steps))
