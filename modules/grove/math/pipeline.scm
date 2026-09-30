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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-source->term math-derive math-derive-curl-curl
            math-search math-search-backward math-simplify
            math-derivation->content math-check-chain math-verification->content)
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
