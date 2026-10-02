;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math presentation)
  #:use-module (ice-9 format)
  #:use-module (grove ir)
  #:use-module (grove math derivation)
  #:use-module (grove math diagnostic)
  #:use-module (grove math report)
  #:use-module (grove math source)
  #:use-module (grove math layout)
  #:use-module (grove math term)
  #:use-module (grove math rule)
  #:use-module (grove math rewrite)
  #:use-module (grove math verification)
  #:use-module (grove math semantic-check)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (math-derivation->content math-completed-report->content
            math-report->content math-verification->content
            math-semantic-check->content))

(define (check-view view)
  (unless (memq view '(compact full result)) (error "unknown math presentation view" view)))
(define (check-error-mode mode)
  (unless (memq mode '(raise show)) (error "unknown math error presentation" mode)))

(define (cell children) (make-content-element 'math-cell children))
(define* (token kind text #:key (properties '()))
  (make-content-element 'math-token (list text) #:properties (cons (cons 'token-kind kind) properties)))
(define (expression-children term assumptions)
  (let ((layout (math-term->layout term assumptions)))
    (if (eq? (content-element-kind layout) 'math-row) (content-element-children layout) (list layout))))
(define (first-row term assumptions)
  (make-content-element 'math-row
    (list (cell (expression-children term assumptions)) (cell '()) (cell '()))))
(define (next-row term assumptions annotation)
  (make-content-element 'math-row
    (list (cell (list (token 'mo "=")))
          (cell (expression-children term assumptions)) (cell (if annotation (list annotation) '())))))
(define (step-annotation step origin)
  (let* ((name (math-derivation-step-rule step)) (rule (math-rewrite-rule-ref name))
         (location (math-origin-location origin (math-derivation-step-path step))))
    (token 'mtext (if rule (math-rewrite-rule-label rule) (symbol->string name))
      #:properties (append `((rule . ,(symbol->string name)) (path . ,(math-path-label (math-derivation-step-path step))))
                           (if location `((location . ,(math-location-label location))) '())))))
(define (chain-content certificate endpoints?)
  (let* ((terms (math-derivation-terms certificate))
         (assumptions (math-derivation-assumptions certificate))
         (steps (math-derivation-steps certificate))
         (sources (math-derivation-sources certificate)))
    (make-content-element 'math
      (list (make-content-element 'math-align
        (cons (first-row (car terms) assumptions)
          (if endpoints?
              (if (equal? (car terms) (last terms)) '() (list (next-row (last terms) assumptions #f)))
              (map (lambda (step index)
                     (next-row (math-derivation-step-to step) assumptions
                       (step-annotation step (and (< index (length sources)) (list-ref sources index)))))
                   steps (iota (length steps)))))))
      #:properties '((display? . #t)) #:flow-kind 'block)))

(define (display-certificate certificate view full-content)
  (check-view view)
  (case view
    ((result) (chain-content certificate #t))
    ((full) (make-content-element 'math-derivation full-content #:flow-kind 'block))
    (else
      (if (null? (math-derivation-steps certificate)) (chain-content certificate #t)
          (make-content-element 'math-derivation
            (list (chain-content certificate #t)
                  (make-content-disclosure
                    (format #f "Derivation (~a step~:p)"
                            (length (math-derivation-steps certificate)))
                    full-content #:open? #f #:addressable? #t))
            #:flow-kind 'block)))))

(define* (math-derivation->content certificate #:key (view 'compact))
  (unless (math-derivation? certificate) (error "expected a math derivation" certificate))
  (display-certificate certificate view (list (chain-content certificate #f))))

(define (diagnostic-content diagnostic)
  (let* ((location (math-diagnostic-source diagnostic))
         (origin (math-diagnostic-origin diagnostic))
         (rule (and (math-origin-generated? origin) (math-rewrite-rule-ref (math-origin-rule origin)))))
    (make-content-element 'math-diagnostic
      (append (list (make-content-element 'paragraph (list (math-diagnostic-message diagnostic)) #:flow-kind 'block))
        (if location
            (list (make-content-element 'details
                    (append (list (make-content-element 'summary
                            (list (format #f "~a:~a:~a" (basename (math-location-file location))
                                    (math-location-line location) (math-location-column location))))
                          (make-content-element 'code-block (list (math-location-excerpt location)) #:flow-kind 'block))
                          (if rule
                                (list (make-content-element 'paragraph
                                        (list (format #f "Generated by ~a from this source."
                                                (math-rewrite-rule-label rule))) #:flow-kind 'block)) '()))
                    #:flow-kind 'block)) '()))
      #:flow-kind 'block)))

(define (report-errors report)
  (if (pair? (math-report-diagnostics report))
      (map diagnostic-content (math-report-diagnostics report))
      (list (make-content-element 'paragraph
              (list (format #f "Computation stopped: ~a." (math-report-reason report))) #:flow-kind 'block))))

(define* (math-completed-report->content report #:key (view 'compact) (on-error 'raise))
  (check-view view) (check-error-mode on-error)
  (if (not (math-report-complete? report))
      (if (eq? on-error 'show) (report-errors report)
          (error (string-join (map math-diagnostic-description (math-report-diagnostics report)) "; ")
                 (math-report-reason report)))
      (let* ((certificate (math-report-derivation report))
             (passes (math-report-passes report))
             (full-content
               (begin
                 (unless (math-derivation? certificate)
                   (error "completed presentation requires a derivation" report))
                 (if (null? passes) (list (chain-content certificate #f))
                   (append-map
                     (lambda (pass)
                       (list (make-content-element 'paragraph
                               (list (string-capitalize (symbol->string (math-report-name pass)))) #:flow-kind 'block)
                             (chain-content (math-report-derivation pass) #f))) passes)))))
        (append (list (display-certificate certificate view full-content))
          (if (eq? (math-report-status report) 'symbolic)
              (list (make-content-element 'paragraph
                      '("Symbolic result: unevaluated derivatives remain.") #:flow-kind 'block))
              '())))))

(define (reason-label reason)
  (case reason ((exhausted) "finished") ((normal-form) "complete")
    ((depth-limit) "stopped at the depth limit") ((state-limit) "stopped at the state limit")
    ((result-limit) "stopped at the result limit") ((term-size-limit) "stopped at the term-size limit")
    ((step-limit) "stopped at the step limit") (else "failed")))

;; Reports retain their bounds and candidates even when the article uses a
;; compact view. A cutoff is never presented as exhaustive failure.
(define* (math-report->content report #:key (view 'compact))
  (check-view view)
  (unless (math-report? report) (error "expected a math report" report))
  (append
    (list (make-content-element 'paragraph
            (list (format #f "~a ~a: ~a derivation~:p found~a."
                    (string-capitalize (symbol->string (math-report-name report)))
                    (reason-label (math-report-reason report))
                    (length (math-report-candidates report))
                    (if (math-report-complete? report) " with these rules and assumptions" " within the search bounds")))
            #:flow-kind 'block))
    (map diagnostic-content (math-report-diagnostics report))
    (map (lambda (candidate) (math-derivation->content candidate #:view view)) (math-report-candidates report))))

(define* (math-verification->content verification #:key (view 'compact) (on-error 'raise))
  (check-view view) (check-error-mode on-error)
  (unless (math-verification? verification) (error "expected a math verification" verification))
  (if (math-verification-valid? verification)
      (math-derivation->content (math-verification-derivation verification) #:view view)
      (if (eq? on-error 'raise) (error (string-join (math-verification-errors verification) "\n"))
          (make-content-element 'math-diagnostics
            (map diagnostic-content (math-verification-diagnostics verification)) #:flow-kind 'block))))

(define (semantic-check-label check)
  (case (math-semantic-check-status check)
    ((semantically-checked) "Semantically checked in the exact rational-polynomial model.")
    ((rejected) "Semantic check rejected the supplied claim.")
    ((unsupported) "Semantic check is unsupported for this expression or rule.")))

(define (math-semantic-check->content check)
  "Present the scope and result of an independent semantic check."
  (unless (math-semantic-check? check)
    (error "expected a semantic check" check))
  (make-content-element 'math-semantic-check
    (append
      (list (make-content-element 'paragraph
              (list (semantic-check-label check)) #:flow-kind 'block))
      (if (symbol? (math-semantic-check-subject check))
          (list (make-content-element 'paragraph
                  (list (format #f "Rule: ~a" (math-semantic-check-subject check)))
                  #:flow-kind 'block))
          '())
      (if (math-derivation? (math-semantic-check-subject check))
          (list (math-derivation->content (math-semantic-check-subject check)))
          '())
      (map (lambda (message)
             (make-content-element 'paragraph (list message) #:flow-kind 'block))
           (math-semantic-check-diagnostics check)))
    #:flow-kind 'block))
