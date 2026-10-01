;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math pipeline)
  #:use-module (grove math syntax)
  #:use-module (grove math term)
  #:use-module (grove math context)
  #:use-module (grove math rewrite)
  #:use-module (grove math types)
  #:use-module (grove math derivation)
  #:use-module (grove math strategy)
  #:use-module (grove math verification)
  #:use-module (grove math differentiation)
  #:use-module (grove math pass)
  #:use-module (grove math report)
  #:use-module (grove math diagnostic)
  #:use-module (grove math source)
  #:use-module (grove math presentation)
  #:use-module (srfi srfi-1)
  #:export (math-source->term math-derive math-derive-curl-curl
            math-search math-search-backward math-simplify math-calculate math-collect
            math-check-chain
            math-differentiate math-rewrite)
  #:re-export (math-derivation->content math-verification->content
               math-report->content math-completed-report->content current-math-assumptions call-with-math-assumptions with-math-assumptions
               math-verify-step math-verify-derivation
               math-verification? math-verification-valid?
               math-verification-errors math-verification-diagnostics
               math-verification-derivation math-diagnostic?
               math-diagnostic-step math-diagnostic-rule math-diagnostic-path
               math-diagnostic-code math-diagnostic-premises math-diagnostic-message
               math-diagnostic-source math-location? math-location-file math-location-line
               math-location-column math-location-excerpt
               math-derivation-step? math-derivation-step-from
               math-derivation-step-to math-derivation-step-rule
               math-derivation-step-path math-derivation?
               math-derivation-terms math-derivation-steps
               math-derivation-assumptions math-derivation-sources
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
    (math-report-with-source
      (if (and (eq? (car coordinate) 'var) (polynomial-term? expression))
          (run-math-passes input facts
            `((differentiate . ,math-differentiation-rules)
              (simplify . ,math-simplification-rules))
            #:name 'differentiate #:max-steps max-steps #:max-term-size max-term-size)
          (run-math-pass input facts #:name 'differentiate #:rules '()
            #:max-steps max-steps #:max-term-size max-term-size
            #:validate (lambda (term)
                         (list (make-math-diagnostic #f #f '() 'unsupported-expression '()
                                 "differentiation requires a polynomial and a coordinate variable")))))
      source #:prefix '(0))))

(define* (math-rewrite source #:key (assumptions '()) (rules #f)
                        (max-steps 32) (max-term-size 2048))
  (math-report-with-source
    (run-math-pass (math-source->term source) (math-assumptions assumptions)
      #:name 'rewrite #:rules rules #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-derive source #:key (assumptions '()) (rules #f)
                       (max-steps 32) (max-term-size 2048) (view 'compact))
  (let* ((report (math-rewrite source #:assumptions assumptions #:rules rules
                   #:max-steps max-steps #:max-term-size max-term-size))
         (content (math-completed-report->content report #:view view)))
    (unless (pair? (math-derivation-steps (math-report-derivation report)))
      (error "no math rewrite rule applies to the expression" source))
    (car content)))

(define* (math-derive-curl-curl source #:key (assumptions '()))
  (math-derive source #:assumptions assumptions))

(define* (math-search source target
                      #:key (assumptions '()) (rules #f)
                            (max-depth 8) (max-results 5)
                            (max-states 256) (max-term-size 128))
  (math-report-with-source
    (search-math-terms
      (math-source->term source) (math-source->term target)
      (math-assumptions assumptions) #f
      #:rules rules #:max-depth max-depth #:max-results max-results
      #:max-states max-states #:max-term-size max-term-size)
    source))

(define* (math-search-backward target source
                               #:key (assumptions '()) (rules #f)
                                     (max-depth 8) (max-results 5)
                                     (max-states 256) (max-term-size 128))
  (math-report-with-source
    (search-math-terms
      (math-source->term target) (math-source->term source)
      (math-assumptions assumptions) #t
      #:rules rules #:max-depth max-depth #:max-results max-results
      #:max-states max-states #:max-term-size max-term-size)
    source))

(define* (math-simplify source #:key (assumptions '()) (rules #f) (max-steps 32) (max-term-size 2048))
  (math-report-with-source
    (simplify-math-term (math-source->term source)
      (math-assumptions assumptions) #:rules rules #:max-steps max-steps
      #:max-term-size max-term-size)
    source))

(define* (math-calculate source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (math-report-with-source
    (calculate-math-term (math-source->term source)
      (math-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-collect source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
  (math-report-with-source
    (collect-math-term (math-source->term source)
      (math-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
    source))

(define* (math-check-chain sources #:key (assumptions '()) (steps #f) (rules #f))
  (unless (and (list? sources) (every string? sources))
    (error "handwritten math lines must be source strings" sources))
  (check-math-chain (map math-source->term sources)
    (math-assumptions assumptions) #:steps steps #:rules rules #:sources (map math-source-origin sources)))
