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
  #:use-module (grove math verification)
  #:use-module (grove math differentiation)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-source->term math-derive math-derive-curl-curl
            math-search math-search-backward math-simplify
            math-derivation->content math-check-chain math-verification->content
            math-differentiate math-differentiation-result?
            math-differentiation-expansion math-differentiation-simplification
            math-differentiation-final math-differentiation->content
            math-search-report->content)
  #:re-export (math-verify-step math-verify-derivation
               math-verification? math-verification-valid?
               math-verification-errors math-verification-diagnostics
               math-verification-derivation math-diagnostic?
               math-diagnostic-step math-diagnostic-rule math-diagnostic-path
               math-diagnostic-code math-diagnostic-premises math-diagnostic-message
               math-derivation-step? math-derivation-step-from
               math-derivation-step-to math-derivation-step-rule
               math-derivation-step-path math-derivation?
               math-derivation-terms math-derivation-steps
               math-derivation-assumptions
               math-search-report? math-search-report-candidates
               math-search-report-reason math-search-report-limits
               math-search-report-states math-search-report-complete?))

(define (math-source->term source)
  (math-expression->term (parse-math source)))

(define-record-type <math-differentiation-result>
  (make-math-differentiation-result expansion simplification final)
  math-differentiation-result?
  (expansion math-differentiation-expansion)
  (simplification math-differentiation-simplification)
  (final math-differentiation-final))

(define* (math-differentiate source variable #:key (assumptions '())
                              (max-steps 128) (max-term-size 2048))
  (unless (and (exact-integer? max-steps) (>= max-steps 0)
               (exact-integer? max-term-size) (> max-term-size 0))
    (error "invalid differentiation bounds" max-steps max-term-size))
  (let* ((expression (math-source->term source))
         (coordinate (math-source->term variable))
         (facts (math-source->assumptions assumptions))
         (input `(differentiate ,expression ,coordinate)))
    (unless (and (eq? (car coordinate) 'var) (polynomial-term? expression))
      (error "differentiation requires a polynomial and a coordinate variable" source variable))
    (check-derivation-types! input facts)
    (let expand ((current input) (terms (list input)) (steps '()) (count 0))
      (when (> (math-term-size current) max-term-size)
        (error "differentiation exceeded its term-size limit" max-term-size))
      (let ((answers (math-rewrite-answers facts current math-differentiation-rules #f)))
        (if (null? answers)
            (let* ((expansion (make-math-derivation (reverse terms) (reverse steps) facts))
                   (report (simplify-math-term current facts #:max-steps max-steps))
                   (simplification (car (math-search-report-candidates report))))
              (unless (math-search-report-complete? report)
                (error "differentiation simplification exceeded its step limit" max-steps))
              (make-math-differentiation-result expansion simplification
                (last (math-derivation-terms simplification))))
            (begin
              (when (= count max-steps)
                (error "differentiation exceeded its step limit" max-steps))
              (let* ((answer (car answers)) (next (car answer)))
                (expand next (cons next terms)
                  (cons (make-math-derivation-step current next (cadr answer) (caddr answer)) steps)
                  (+ count 1)))))))))

(define (math-differentiation->content result)
  (list (math-derivation->content (math-differentiation-expansion result))
        (math-derivation->content (math-differentiation-simplification result))))

;; Preserve the stopping reason even when a bounded search found a candidate.
(define (math-search-report->content report)
  (unless (math-search-report? report)
    (error "expected a math search report" report))
  (cons (make-content-element 'paragraph
          (list (format #f "Search stopped: ~a; ~a candidate(s); ~a states visited."
                  (math-search-report-reason report)
                  (length (math-search-report-candidates report))
                  (math-search-report-states report)))
          #:flow-kind 'block)
        (map math-derivation->content (math-search-report-candidates report))))

(define (math-source->assumptions sources)
  (append-map
    (lambda (source)
      (map math-source->term
           (filter (lambda (part) (not (string-null? (string-trim-both part))))
                   (string-split source #\;))))
    sources))

(define (check-derivation-types! input assumptions)
  (unless (math-infer-type assumptions input)
    (error (math-type-diagnostic assumptions input))))

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

(define (math-row-next term rule path assumptions)
  (make-content-element 'math-row
    (list (math-cell (list (make-content-element
                             'math-token (list "=")
                             #:properties '((token-kind . mo)))))
          (math-cell (math-expression-children term assumptions))
          (math-cell
            (list (math-token 'mtext
                    (format #f "~a (at ~a)"
                            (symbol->string rule) (math-path-label path))))))))

(define (math-derivation->content result)
  (unless (math-derivation? result)
    (error "expected a math search result" result))
  (let ((terms (math-derivation-terms result))
        (steps (math-derivation-steps result))
        (assumptions (math-derivation-assumptions result)))
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

(define* (math-check-chain sources #:key (assumptions '()) (steps #f) (rules #f))
  (unless (and (list? sources) (every string? sources))
    (error "handwritten math lines must be source strings" sources))
  (check-math-chain (map math-source->term sources)
    (math-source->assumptions assumptions) #:steps steps #:rules rules))

(define (math-verification->content verification)
  (unless (math-verification? verification)
    (error "expected a math verification" verification))
  (unless (math-verification-valid? verification)
    (error (string-join (math-verification-errors verification) "\n")))
  (math-derivation->content (math-verification-derivation verification)))
