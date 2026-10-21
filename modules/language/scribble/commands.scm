;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir)
  #:use-module (ice-9 optargs)
  #:use-module (language scribble command)
  #:use-module (grove math pipeline)
  #:use-module (grove math presentation)
  #:use-module (grove math context)
  #:use-module (grove math source)
  #:use-module ((grove math semantic) #:prefix semantic:)
  #:use-module ((grove math formula) #:prefix φ:)
  #:use-module (grove math expression)
  #:use-module (grove math layout)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13))

(define-public (text-paragraphs value)
  (unless (or (string? value) (content-text? value))
    (error "text-paragraphs expects plain text" value))
  (let* ((text (if (content-text? value)
                   (content-text-value value)
                   value))
         (source (and (content-text? value)
                      (content-text-source value))))
    (define (make-paragraph-text lines)
      (make-content-text (string-join (reverse lines) " ") source))
    (let loop ((lines (string-split text #\newline))
               (current '())
               (paragraphs '()))
      (if (null? lines)
          (reverse
            (if (null? current)
                paragraphs
                (cons (make-paragraph-text current) paragraphs)))
          (let ((line (string-trim-both (car lines))))
            (if (string-null? line)
                (loop (cdr lines) '()
                      (if (null? current)
                          paragraphs
                          (cons (make-paragraph-text current) paragraphs)))
                (loop (cdr lines) (cons line current) paragraphs)))))))

(define*-public (bold #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element 'bold body #:style style))

(define*-public (verbatim #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element
    'verbatim
    (list (make-content-text (content-verbatim-string body)))
    #:style style #:flow-kind 'block))

(define*-public (paragraph #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element 'paragraph body #:style style #:flow-kind 'block))

(define*-public (blockquote #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element 'blockquote body #:style style #:flow-kind 'block))

(define*-public (itemize #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element 'itemize body #:style style #:flow-kind 'block))

(define*-public (item #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element 'item body #:style style #:flow-kind 'block))

(define*-public (code #:key (style #f) . body)
  (set! body (command-body body))
  (make-content-element 'code body #:style style))

(define*-public (code-block #:key (style #f) (language #f) . body)
  (set! body (command-body body))
  (let ((language (cond ((not language) #f)
                        ((symbol? language) language)
                        ((string? language) (string->symbol language))
                        (else
                          (error "code-block language must be a symbol, string, or #f"
                                 language)))))
    (make-content-element 'code-block
      (list (make-content-text (content-verbatim-string body)))
      #:properties (if language `((language . ,language)) '())
      #:style style #:flow-kind 'block)))

(define*-public (math #:key (style #f) . source)
  (set! source (command-body source))
  (make-content-element 'math
    (list (math-value->layout (apply φ:read source) (math-assumptions '()))) #:style style))

(define*-public (display-math #:key (style #f) . source)
  (set! source (command-body source))
  (make-content-element 'math
                        (list (math-value->layout (apply φ:read source) (math-assumptions '())))
                        #:style style
                        #:properties '((display? . #t))
                        #:flow-kind 'block))

;; A function would evaluate the body before installing its assumptions.
(define-syntax-rule (with-assumptions assumptions body ...)
  (with-math-assumptions (φ:assumptions assumptions) (list body ...)))
(export with-assumptions)

;; Guile's lambda* rest argument retains keyword/value pairs. Strip them once
;; for all math commands; lambda* owns option defaults and validation.
(define (command-assumptions assumptions)
  (if assumptions (φ:assumptions assumptions) '()))

(define-public derive
  (lambda* (#:key (assumptions #f) (view 'compact) (on-error 'raise)
                 (rules #f) (max-steps 32) (max-term-size 2048) #:rest source)
    (math-completed-report->content
      (math-derive (apply φ:read (command-body source))
        #:assumptions (command-assumptions assumptions)
        #:rules rules #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public check-derive
  (lambda* (#:key (assumptions #f) (view 'compact) (on-error 'raise)
                 (rules #f) (steps #f) #:rest source)
    (let ((lines (apply φ:read-lines (command-body source))))
      (math-verification->content
        (math-check-chain lines #:assumptions (command-assumptions assumptions) #:rules rules #:steps steps)
        #:view view #:on-error on-error))))

(define-public differentiate
  (lambda* (variable #:key (assumptions #f) (view 'compact) (on-error 'raise)
                          (max-steps 128) (max-term-size 2048) #:rest source)
    (math-completed-report->content
      (math-differentiate (apply φ:read (command-body source)) (φ:read variable)
        #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public partial
  (lambda* (variable #:key (assumptions #f) (view 'compact) (on-error 'raise)
                          (max-steps 128) (max-term-size 2048) #:rest source)
    "Compute and display a partial derivative in an explicit coordinate scope."
    (math-completed-report->content
      (math-partial (apply φ:read (command-body source)) (φ:read variable)
        #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public search-derive
  (lambda* (target #:key (assumptions #f) (view 'compact)
                       (rules #f) (backward? #f) (max-results 1)
                       (max-depth 8) (max-states 256) (max-term-size 128) #:rest source)
    (unless (boolean? backward?) (error "search direction must be boolean" backward?))
    (math-report->content
      (let ((input (apply φ:read (command-body source))))
        ((if backward? math-search-backward math-search)
          (if backward? (φ:read target) input) (if backward? input (φ:read target))
          #:assumptions (command-assumptions assumptions) #:rules rules #:max-results max-results
          #:max-depth max-depth #:max-states max-states #:max-term-size max-term-size))
      #:view view)))

(define (math-properties tex? display? id)
  (append (if tex? '((tex? . #t)) '())
          (if display? '((display? . #t)) '())
          (if id `((id . ,id)) '())))

(define*-public (tex-math #:key (style #f) (id #f) . source)
  (set! source (command-body source))
  (make-content-element 'math
                        (map make-content-text
                             (map (lambda (value)
                                    (if (string? value)
                                        value
                                        (error "TeX math accepts plain source text"
                                               value)))
                                  source))
                        #:style style
                        #:properties (math-properties #t #f (checked-anchor-id id))))

(define*-public (tex-display-math #:key (style #f) (id #f) . source)
  (set! source (command-body source))
  (make-content-element 'math
                        (map make-content-text
                             (map (lambda (value)
                                    (if (string? value)
                                        value
                                        (error "TeX math accepts plain source text"
                                               value)))
                                  source))
                        #:style style
                        #:properties (math-properties #t #t (checked-anchor-id id))
                        #:flow-kind 'block))

(define-public compute
  (lambda* (#:key (assumptions #f) (view 'compact) (on-error 'raise)
                 (max-steps 256) (max-term-size 8192) #:rest source)
    "Compute Cartesian components with explicit, replayable passes."
    (math-completed-report->content
      (math-compute (apply φ:read (command-body source))
        #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public antiderivative
  (lambda* (coordinate #:key (assumptions #f) (view 'compact) (on-error 'raise)
                            (max-steps 128) (max-term-size 8192) #:rest source)
    (math-completed-report->content
      (math-antiderivative (apply φ:read (command-body source)) (φ:read coordinate)
        #:assumptions (command-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public solve-poisson
  (lambda* (#:key (coordinate #f) (assumptions #f) (view 'compact) (on-error 'raise)
                 (max-steps 128) (max-term-size 8192) #:rest source)
    (math-completed-report->content
      (math-solve-poisson (apply φ:read (command-body source))
        #:coordinate coordinate #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public check-equation
  (lambda* (target #:key (assumptions #f) (view 'compact) (on-error 'raise)
                        (max-steps 256) (max-term-size 8192) #:rest source)
    (math-completed-report->content
      (math-check-equation (apply φ:read (command-body source)) (φ:read target)
        #:assumptions (command-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))
