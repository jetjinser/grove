;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir)
  #:use-module (grove math pipeline)
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

(define-public (bold . body)
  (make-content-element 'bold body))

(define-public (verbatim . body)
  (make-content-element
    'verbatim
    (list (make-content-text (content-verbatim-string body)))
    #:flow-kind 'block))

(define-public (paragraph . body)
  (make-content-element 'paragraph body #:flow-kind 'block))

(define-public (heading . body)
  (make-content-element 'heading body #:flow-kind 'block))

(define-public (blockquote . body)
  (make-content-element 'blockquote body #:flow-kind 'block))

(define-public (itemize . body)
  (make-content-element 'itemize body #:flow-kind 'block))

(define-public (item . body)
  (make-content-element 'item body #:flow-kind 'block))

(define-public (code . body)
  (make-content-element 'code body))

(define-public (code-block . body)
  (make-content-element 'code-block body #:flow-kind 'block))

(define-public (math . source)
  (make-content-element 'math
    (list (math-value->layout (apply φ:read source) (math-assumptions '())))))

(define-public (display-math . source)
  (make-content-element 'math
                        (list (math-value->layout (apply φ:read source) (math-assumptions '())))
                        #:properties '((display? . #t))
                        #:flow-kind 'block))

;; A function would evaluate the body before installing its assumptions.
(define-syntax-rule (with-assumptions assumptions body ...)
  (with-math-assumptions (list assumptions) (list body ...)))
(export with-assumptions)

;; Guile's lambda* rest argument retains keyword/value pairs. Strip them once
;; for all math commands; lambda* owns option defaults and validation.
(define (math-command-fragments fragments)
  (let body ((remaining fragments) (text '()))
    (cond ((null? remaining) (reverse text))
          ((keyword? (car remaining)) (body (cddr remaining) text))
          (else (body (cdr remaining) (cons (car remaining) text))))))

(define (command-assumptions assumptions)
  (if assumptions (list assumptions) '()))

(define-public derive
  (lambda* (#:key (assumptions #f) (view 'compact) (on-error 'raise)
                 (rules #f) (max-steps 32) (max-term-size 2048) #:rest source)
    (math-derive (apply φ:read (math-command-fragments source))
      #:assumptions (command-assumptions assumptions) #:view view #:on-error on-error
      #:rules rules #:max-steps max-steps #:max-term-size max-term-size)))

(define-public check-derive
  (lambda* (#:key (assumptions #f) (view 'compact) (on-error 'raise)
                 (rules #f) (steps #f) #:rest source)
    (let ((lines (apply φ:read-lines (math-command-fragments source))))
      (math-verification->content
        (math-check-chain lines #:assumptions (command-assumptions assumptions) #:rules rules #:steps steps)
        #:view view #:on-error on-error))))

(define-public differentiate
  (lambda* (variable #:key (assumptions #f) (view 'compact) (on-error 'raise)
                          (max-steps 128) (max-term-size 2048) #:rest source)
    (math-completed-report->content
      (math-differentiate (apply φ:read (math-command-fragments source)) variable
        #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public partial
  (lambda* (variable #:key (assumptions #f) (view 'compact) (on-error 'raise)
                          (max-steps 128) (max-term-size 2048) #:rest source)
    "Compute and display a partial derivative in an explicit coordinate scope."
    (math-completed-report->content
      (math-partial (apply φ:read (math-command-fragments source)) variable
        #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public search-derive
  (lambda* (target #:key (assumptions #f) (view 'compact)
                       (rules #f) (backward? #f) (max-results 1)
                       (max-depth 8) (max-states 256) (max-term-size 128) #:rest source)
    (unless (boolean? backward?) (error "search direction must be boolean" backward?))
    (math-report->content
      (let ((input (apply φ:read (math-command-fragments source))))
        ((if backward? math-search-backward math-search)
          (if backward? target input) (if backward? input target)
          #:assumptions (command-assumptions assumptions) #:rules rules #:max-results max-results
          #:max-depth max-depth #:max-states max-states #:max-term-size max-term-size))
      #:view view)))

(define-public (tex-math . source)
  (make-content-element 'math
                        (map make-content-text
                             (map (lambda (value)
                                    (if (string? value)
                                        value
                                        (error "TeX math accepts plain source text"
                                               value)))
                                  source))
                        #:properties '((tex? . #t))))

(define-public (tex-display-math . source)
  (make-content-element 'math
                        (map make-content-text
                             (map (lambda (value)
                                    (if (string? value)
                                        value
                                        (error "TeX math accepts plain source text"
                                               value)))
                                  source))
                        #:properties '((tex? . #t) (display? . #t))
                        #:flow-kind 'block))

(define-public compute
  (lambda* (#:key (assumptions #f) (view 'compact) (on-error 'raise)
                 (max-steps 256) (max-term-size 8192) #:rest source)
    "Compute Cartesian components with explicit, replayable passes."
    (math-completed-report->content
      (math-compute (apply φ:read (math-command-fragments source))
        #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public antiderivative
  (lambda* (coordinate #:key (assumptions #f) (view 'compact) (on-error 'raise)
                            (max-steps 128) (max-term-size 8192) #:rest source)
    (math-completed-report->content
      (math-antiderivative (apply φ:read (math-command-fragments source)) coordinate
        #:assumptions (command-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public solve-poisson
  (lambda* (#:key (coordinate #f) (assumptions #f) (view 'compact) (on-error 'raise)
                 (max-steps 128) (max-term-size 8192) #:rest source)
    (math-completed-report->content
      (math-solve-poisson (apply φ:read (math-command-fragments source))
        #:coordinate coordinate #:assumptions (command-assumptions assumptions)
        #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))

(define-public check-equation
  (lambda* (target #:key (assumptions #f) (view 'compact) (on-error 'raise)
                        (max-steps 256) (max-term-size 8192) #:rest source)
    (math-completed-report->content
      (math-check-equation (apply φ:read (math-command-fragments source)) target
        #:assumptions (command-assumptions assumptions) #:max-steps max-steps #:max-term-size max-term-size)
      #:view view #:on-error on-error)))
