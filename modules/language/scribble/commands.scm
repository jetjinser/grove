;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove private anchor)
  #:use-module (grove ir)
  #:use-module (ice-9 optargs)
  #:use-module (language scribble command)
  #:use-module (grove math calculation)
  #:use-module (grove math presentation)
  #:use-module (grove math context)
  #:use-module (grove math private source)
  #:use-module ((grove math semantic) #:prefix semantic:)
  #:use-module ((grove math formula) #:prefix φ:)
  #:use-module (grove math expression)
  #:use-module (grove math layout)
  #:use-module (srfi srfi-1)
  #:export (use-commands))

(define (command-module-name? value)
  (and (list? value) (pair? value) (every symbol? value)))

(define (use-command-module module-name)
  "Import the command modules declared by MODULE-NAME's command-modules export."
  (let* ((module (resolve-module module-name))
         (variable (and module (module-local-variable module 'command-modules))))
    (unless module
      (error "no such command module" module-name))
    (unless variable
      (error "command module does not define command-modules" module-name))
    ;; Import the manifest itself too, so the build records its source as an
    ;; input even when the manifest is a plain .scm module.
    (let ((interface (resolve-interface module-name)))
      (unless (memq interface (module-uses (current-module)))
        (module-use! (current-module) interface)))
    (let ((dependencies (variable-ref variable)))
      (unless (and (list? dependencies)
                   (every command-module-name? dependencies))
        (error "command-modules must be a list of non-empty module names"
               module-name dependencies))
      (for-each
        (lambda (dependency)
          (module-use! (current-module) (resolve-interface dependency)))
        dependencies))))

(define-syntax use-commands
  (lambda (form)
    (syntax-case form ()
      ((_ name)
       (let* ((datum (syntax->datum #'name))
              (base (cond ((string? datum) (string->symbol datum))
                          ((symbol? datum) datum)
                          ((list? datum) datum)
                          (else
                            (syntax-violation
                              'use-commands
                              "expected a command module name"
                              form
                              #'name))))
              (module (if (list? base) base (list base))))
         (with-syntax ((quoted (datum->syntax #'name (list 'quote module))))
           #'(use-command-module quoted)))))))

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

(define (make-math-element children style display? properties)
  (make-content-element 'math children
    #:style style
    #:properties properties
    #:flow-kind (if display? 'block 'inline)))

(define*-public (math #:key (style #f) . source)
  (set! source (command-body source))
  (make-math-element
    (list (math-value->layout (apply φ:read source) (math-assumptions '())))
    style #f '()))

(define*-public (display-math #:key (style #f) . source)
  (set! source (command-body source))
  (make-math-element
    (list (math-value->layout (apply φ:read source) (math-assumptions '())))
    style #t '((display? . #t))))

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

(define (make-tex-math-element style id display? source)
  (set! source (command-body source))
  (make-math-element
    (map make-content-text
      (map (lambda (value)
             (if (string? value)
                 value
                 (error "TeX math accepts plain source text" value)))
           source))
    style display? (math-properties #t display? (checked-anchor-id id))))

(define*-public (tex-math #:key (style #f) (id #f) . source)
  (make-tex-math-element style id #f source))

(define*-public (tex-display-math #:key (style #f) (id #f) . source)
  (make-tex-math-element style id #t source))

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
