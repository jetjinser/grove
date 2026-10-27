;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math formula)
  #:use-module (grove ir)
  #:use-module ((grove math formula reader) #:prefix reader:)
  #:use-module ((grove math semantic) #:prefix semantic:)
  #:use-module ((grove math pipeline) #:prefix core:)
  #:use-module (grove math presentation)
  #:use-module (grove math expression)
  #:use-module (grove math assumption)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (read read-lines assumptions term
            add subtract negate multiply divide power dot cross
            sin cos exp log grad divergence curl laplacian group styled
            derivative partial-expression antiderivative-expression poisson-particular
            call components
            derive rewrite search search-backward simplify calculate normalize expand collect
            check-chain check-polynomial-rule check-polynomial-derivation
            check-polynomial-step check-polynomial-derivative check-polynomial-antiderivative
            polynomial-check->content antiderivative
            compute check-equation solve-poisson differentiate partial))

;; Public Formula helpers are adapters: parse fragments once, then call the
;; Semantic API. The parser itself lives in `formula reader` so Semantic
;; constructors can use it without depending on this facade.
(define-public (read . fragments)
  (apply reader:read fragments))

(define-public (read-lines . fragments)
  (apply reader:read-lines fragments))

(define (parse-assumptions sources)
  "Read assumption syntax into Semantic fact terms for the computation API."
  (define (decode source)
    (cond
      ((string? source)
       (map (lambda (part) (math-value-term (read part)))
            (filter (lambda (part) (not (string-null? part)))
              (map string-trim-both (string-split source #\;)))))
      ((math-value? source)
       (let ((declarations (math-value-declarations source)))
         (if (pair? declarations) declarations
             (list (math-value-term source)))))
      ((math-assumption? source) (list source))
      ((list? source) (append-map decode source))
      (else (error "Formula assumptions require text, Semantic values, or fact terms" source))))
  (delete-duplicates (append-map decode sources) equal?))

(define-public (assumptions . sources)
  (parse-assumptions sources))

(define-public (term input)
  (math-value-term (read input)))

(define-syntax-rule (define-binary name constructor)
  (define-public (name left right)
    (constructor (read left) (read right))))

(define-syntax-rule (define-unary name constructor)
  (define-public (name . fragments)
    (constructor (apply read fragments))))

(define-binary add semantic:add)
(define-binary subtract semantic:subtract)
(define-unary negate semantic:negate)
(define-binary multiply semantic:multiply)
(define-binary divide semantic:divide)
(define-binary power semantic:power)
(define-binary dot semantic:dot)
(define-binary cross semantic:cross)
(define-unary sin semantic:sin)
(define-unary cos semantic:cos)
(define-unary exp semantic:exp)
(define-unary log semantic:log)
(define-unary grad semantic:grad)
(define-unary divergence semantic:divergence)
(define-unary curl semantic:curl)
(define-unary laplacian semantic:laplacian)
(define-unary group semantic:group)

(define-public styled
  (lambda* (input #:key (variant 'normal))
    (semantic:styled (read input) #:variant variant)))

(define-public (derivative input coordinate)
  (semantic:derivative (read input) (read coordinate)))
(define-public (partial-expression input coordinate)
  (semantic:partial (read input) (read coordinate)))
(define-public (antiderivative-expression input coordinate)
  (semantic:antiderivative (read input) (read coordinate)))
(define-public (poisson-particular input coordinate)
  (semantic:poisson-particular (read input) (read coordinate)))

(define-public (call function . operands)
  (apply semantic:call function (map read operands)))

(define-public components
  (lambda* (x y z #:key space)
    (semantic:components (read x) (read y) (read z) #:space space)))

(define-public derive
  (lambda* (source #:key (assumptions '()) (rules #f) (max-steps 32)
                   (max-term-size 2048) (view 'compact) (on-error 'raise))
    (let ((content
            (math-completed-report->content
              (core:math-derive (read source)
                #:assumptions (parse-assumptions (list assumptions)) #:rules rules
                #:max-steps max-steps #:max-term-size max-term-size)
              #:view view #:on-error on-error)))
      (if (null? (cdr content)) (car content)
          (make-content-element 'math-diagnostics content #:flow-kind 'block)))))

(define-public rewrite
  (lambda* (source #:key (assumptions '()) (rules #f) (max-steps 32) (max-term-size 2048))
    (core:math-rewrite (read source) #:assumptions (parse-assumptions (list assumptions)) #:rules rules
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public search
  (lambda* (source target #:key (assumptions '()) (rules #f) (max-depth 8)
            (max-results 5) (max-states 256) (max-term-size 128))
    (core:math-search (read source) (read target) #:assumptions (parse-assumptions (list assumptions)) #:rules rules
      #:max-depth max-depth #:max-results max-results #:max-states max-states
      #:max-term-size max-term-size)))

(define-public search-backward
  (lambda* (target source #:key (assumptions '()) (rules #f) (max-depth 8)
            (max-results 5) (max-states 256) (max-term-size 128))
    (core:math-search-backward (read target) (read source) #:assumptions (parse-assumptions (list assumptions)) #:rules rules
      #:max-depth max-depth #:max-results max-results #:max-states max-states
      #:max-term-size max-term-size)))

(define-public simplify
  (lambda* (source #:key (assumptions '()) (rules #f) (max-steps 32) (max-term-size 2048))
    (core:math-simplify (read source) #:assumptions (parse-assumptions (list assumptions)) #:rules rules
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public calculate
  (lambda* (source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
    (core:math-calculate (read source) #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public normalize
  (lambda* (source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
    (core:math-normalize (read source) #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public expand
  (lambda* (source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
    (core:math-expand (read source) #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public collect
  (lambda* (source #:key (assumptions '()) (max-steps 32) (max-term-size 2048))
    (core:math-collect (read source) #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public check-chain
  (lambda* (sources #:key (assumptions '()) (steps #f) (rules #f))
    (core:math-check-chain (map read sources)
      #:assumptions (parse-assumptions (list assumptions)) #:steps steps #:rules rules)))

(define-public (check-polynomial-rule name)
  (core:math-polynomial-check-rule name))

(define-public (check-polynomial-derivation derivation)
  (core:math-polynomial-check-derivation derivation))

(define-public (polynomial-check->content check)
  (core:math-semantic-check->content check))

(define-public check-polynomial-step
  (lambda* (from to rule path #:key (assumptions '()))
    (core:math-polynomial-check-step
      (parse-assumptions (list assumptions))
      (math-value-term (read from)) (math-value-term (read to)) rule path)))

(define-public (check-polynomial-derivative expression coordinate result)
  (let* ((coordinate-term (and (not (symbol? coordinate))
                               (math-value-term (read coordinate))))
         (variable (if (symbol? coordinate) coordinate
                       (and (pair? coordinate-term)
                            (eq? (car coordinate-term) 'var)
                            (cadr coordinate-term)))))
    (core:math-polynomial-check-derivative
      (math-value-term (read expression)) variable
      (math-value-term (read result)))))

(define-public (check-polynomial-antiderivative expression coordinate result)
  (let* ((coordinate-term (and (not (symbol? coordinate))
                               (math-value-term (read coordinate))))
         (variable (if (symbol? coordinate) coordinate
                       (and (pair? coordinate-term)
                            (eq? (car coordinate-term) 'var)
                            (cadr coordinate-term)))))
    (core:math-polynomial-check-antiderivative
      (math-value-term (read expression)) variable
      (math-value-term (read result)))))

(define-public compute
  (lambda* (source #:key (assumptions '()) (max-steps 256) (max-term-size 8192))
    (core:math-compute (read source) #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public differentiate
  (lambda* (source coordinate #:key (assumptions '()) (max-steps 128) (max-term-size 2048))
    (core:math-differentiate (read source) (read coordinate)
      #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public partial
  (lambda* (source coordinate #:key (assumptions '()) (max-steps 128) (max-term-size 2048))
    (core:math-partial (read source) (read coordinate)
      #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public antiderivative
  (lambda* (source coordinate #:key (assumptions '()) (max-steps 128) (max-term-size 8192))
    (core:math-antiderivative (read source) (read coordinate)
      #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))

(define-public solve-poisson
  (lambda* (source #:key (coordinate #f) (assumptions '()) (max-steps 128) (max-term-size 8192))
    (core:math-solve-poisson (read source) #:coordinate (and coordinate (read coordinate))
      #:assumptions (parse-assumptions (list assumptions)) #:max-steps max-steps #:max-term-size max-term-size)))

(define-public check-equation
  (lambda* (left right #:key (assumptions '()) (max-steps 256) (max-term-size 8192))
    (core:math-check-equation (read left) (read right)
      #:assumptions (parse-assumptions (list assumptions))
      #:max-steps max-steps #:max-term-size max-term-size)))
