;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math context)
  #:use-module (grove math layout)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove math derivation)
  #:use-module (grove math strategy)
  #:use-module (grove math verification)
  #:use-module (grove math differentiation)
  #:use-module (grove math pass)
  #:use-module (grove math report)
  #:use-module (grove math diagnostic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-source->term math-derive math-derive-curl-curl
            math-search math-search-backward math-simplify math-calculate math-collect
            math-derivation->content math-check-chain math-verification->content
            math-differentiate math-rewrite math-report->content math-completed-report->content)
  #:re-export (current-math-assumptions call-with-math-assumptions with-math-assumptions
               math-verify-step math-verify-derivation
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
               math-report? math-report-candidates
               math-report-reason math-report-limits
               math-report-states math-report-complete? math-report-name
               math-report-diagnostics math-report-passes math-report-derivation math-report-final))

(define (math-source->term source)
  (math-expression->term (parse-math source)))

(define* (math-differentiate source variable #:key (assumptions '())
                              (max-steps 128) (max-term-size 2048))
  (let* ((expression (math-source->term source))
         (coordinate (math-source->term variable))
         (facts (math-assumptions assumptions))
         (input `(differentiate ,expression ,coordinate)))
    (if (and (eq? (car coordinate) 'var) (polynomial-term? expression))
        (run-math-passes input facts
          `((differentiate . ,math-differentiation-rules)
            (simplify . ,math-simplification-rules))
          #:name 'differentiate #:max-steps max-steps #:max-term-size max-term-size)
        (run-math-pass input facts #:name 'differentiate #:rules '()
          #:max-steps max-steps #:max-term-size max-term-size
          #:validate (lambda (term)
                       (list (make-math-diagnostic #f #f '() 'unsupported-expression '()
                               "differentiation requires a polynomial and a coordinate variable")))))))

(define (math-completed-report->content report)
  (unless (math-report-complete? report)
    (error (string-join (map math-diagnostic-message (math-report-diagnostics report)) "; ")
           (math-report-reason report)))
  (map math-derivation->content
    (if (null? (math-report-passes report))
        (math-report-candidates report)
        (map math-report-derivation (math-report-passes report)))))

;; Preserve the stopping reason even when a bounded search found a candidate.
(define (math-report->content report)
  (unless (math-report? report)
    (error "expected a math search report" report))
  (append (list (make-content-element 'paragraph
          (list (format #f "~a stopped: ~a; ~a candidate(s); ~a states visited."
                  (string-titlecase (symbol->string (math-report-name report)))
                  (math-report-reason report)
                  (length (math-report-candidates report))
                  (math-report-states report)))
          #:flow-kind 'block))
        (map (lambda (diagnostic)
               (make-content-element 'paragraph
                 (list (math-diagnostic-message diagnostic)) #:flow-kind 'block))
             (math-report-diagnostics report))
        (map math-derivation->content (math-report-candidates report))))

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

(define* (math-rewrite source #:key (assumptions '()) (rules #f)
                        (max-steps 32) (max-term-size 2048))
  (run-math-pass (math-source->term source) (math-assumptions assumptions)
    #:name 'rewrite #:rules rules #:max-steps max-steps #:max-term-size max-term-size))

(define* (math-derive source #:key (assumptions '()) (rules #f)
                       (max-steps 32) (max-term-size 2048))
  (let* ((report (math-rewrite source #:assumptions assumptions #:rules rules
                   #:max-steps max-steps #:max-term-size max-term-size))
         (content (math-completed-report->content report)))
    (unless (pair? (math-derivation-steps (math-report-derivation report)))
      (error "no math rewrite rule applies to the expression" source))
    (car content)))

(define* (math-derive-curl-curl source #:key (assumptions '()))
  (math-derive source #:assumptions assumptions))

(define* (math-search source target
                      #:key (assumptions '()) (rules #f)
                            (max-depth 8) (max-results 5)
                            (max-states 256) (max-term-size 128))
  (search-math-terms
    (math-source->term source) (math-source->term target)
    (math-assumptions assumptions) #f
    #:rules rules #:max-depth max-depth #:max-results max-results
    #:max-states max-states #:max-term-size max-term-size))

(define* (math-search-backward target source
                               #:key (assumptions '()) (rules #f)
                                     (max-depth 8) (max-results 5)
                                     (max-states 256) (max-term-size 128))
  (search-math-terms
    (math-source->term target) (math-source->term source)
    (math-assumptions assumptions) #t
    #:rules rules #:max-depth max-depth #:max-results max-results
    #:max-states max-states #:max-term-size max-term-size))

(define* (math-simplify source #:key (assumptions '()) (rules #f) (max-steps 32) (max-term-size 2048))
  (simplify-math-term (math-source->term source)
    (math-assumptions assumptions) #:rules rules #:max-steps max-steps
    #:max-term-size max-term-size))

(define* (math-calculate source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (calculate-math-term (math-source->term source)
    (math-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size))

(define* (math-collect source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (collect-math-term (math-source->term source)
    (math-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size))

(define* (math-check-chain sources #:key (assumptions '()) (steps #f) (rules #f))
  (unless (and (list? sources) (every string? sources))
    (error "handwritten math lines must be source strings" sources))
  (check-math-chain (map math-source->term sources)
    (math-assumptions assumptions) #:steps steps #:rules rules))

(define (math-verification->content verification)
  (unless (math-verification? verification)
    (error "expected a math verification" verification))
  (unless (math-verification-valid? verification)
    (error (string-join (math-verification-errors verification) "\n")))
  (math-derivation->content (math-verification-derivation verification)))
