;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math formula)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math expression)
  #:use-module (grove math source)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (read read-lines))

;; Formula DSL is an optional textual reader for Semantic values. Its AST is
;; private to this boundary; all later computation uses ordinary math terms.
(define (fragments values)
  (append-map (lambda (value) (if (list? value) (fragments value) (list value))) values))

(define (syntax-preferences syntax embedded)
  (case (math-expression-kind syntax)
    ((embedded) (math-value-properties (assv-ref embedded (math-expression-start syntax))))
    ((group) (acons 'parenthesized? #t (syntax-preferences (car (math-expression-children syntax)) embedded)))
    (else
     (let ((children (map (lambda (child) (syntax-preferences child embedded)) (math-expression-children syntax))))
       (if (every null? children) '() (list (cons 'children children)))))))

(define (check-semantic-syntax syntax)
  (when (and (eq? (math-expression-kind syntax) 'binary)
             (eq? (math-expression-value syntax) 'subscript))
    (error "a subscript needs an explicit mathematical meaning; use Notation DSL for a layout-only script"))
  (for-each check-semantic-syntax (math-expression-children syntax)))

(define (formula-expression->term expression)
  (unless (math-expression? expression)
    (error "expected a Formula DSL expression" expression))
  (case (math-expression-kind expression)
    ((embedded) (math-expression-value expression))
    ((identifier) `(var ,(math-expression-value expression)))
    ((number) `(num ,(math-expression-value expression)))
    ((group) (formula-expression->term (car (math-expression-children expression))))
    ((call)
     (let ((arguments (map formula-expression->term (math-expression-children expression))))
       (if (and (eq? (math-expression-value expression) 'zero)
                (equal? arguments '((var vector))))
           '(zero vector)
           (cons (math-expression-value expression) arguments))))
    ((negate) `(- ,(formula-expression->term (car (math-expression-children expression)))))
    ((binary)
     (let ((operator (case (math-expression-value expression)
                       ((plus) '+) ((minus) '-) ((multiply) '*)
                       ((divide) '/) ((power) '^) ((subscript) 'sub)
                       ((equals) '=))))
       (cons operator (map formula-expression->term (math-expression-children expression)))))
    (else (error "unsupported Formula DSL syntax node" (math-expression-kind expression)))))

(define-public (read . values)
  "Read Formula DSL fragments into a Semantic value."
  (if (and (= (length values) 1) (math-value? (car values))) (car values)
      (let collect ((remaining (fragments values)) (offset 0)
                    (tokens '()) (text '()) (embedded '()))
        (cond
          ((null? remaining)
           (let* ((source (math-source-text (reverse text)))
                  (syntax (parse-math-tokens
                            (append (reverse tokens) (list (make-math-token 'eof "" offset offset)))))
                  (term (begin (check-semantic-syntax syntax)
                               (formula-expression->term syntax))))
             (make-math-value term
               #:origin (math-source-origin source #:syntax syntax
                          #:embedded (map (lambda (entry) (cons (car entry) (math-value-origin (cdr entry)))) embedded))
               #:properties (syntax-preferences syntax embedded)
               #:requirements (append-map (lambda (entry) (math-value-requirements (cdr entry))) embedded)
               #:declarations (delete-duplicates
                                (append-map (lambda (entry) (math-value-declarations (cdr entry))) embedded) equal?))))
          ((math-value? (car remaining))
           (let ((value (car remaining)))
             (collect (cdr remaining) (+ offset 1)
               (cons (make-math-token 'embedded (math-value-term value) offset (+ offset 1)) tokens)
               (cons (string-copy " ") text) (acons offset value embedded))))
          ((or (string? (car remaining)) (content-text? (car remaining)) (content-layout? (car remaining)))
           (let* ((run (take-while (lambda (value)
                                     (or (string? value) (content-text? value) (content-layout? value))) remaining))
                  (source (math-source-text run))
                  (scanned (drop-right (tokenize-math source) 1)))
             (collect (drop remaining (length run)) (+ offset (string-length source))
               (append (reverse (map (lambda (token)
                                       (make-math-token (math-token-kind token) (math-token-value token)
                                         (+ offset (math-token-start token)) (+ offset (math-token-end token)))) scanned)) tokens)
               (append (reverse run) text) embedded)))
          (else (error "Formula DSL accepts text fragments and Semantic values" (car remaining)))))))

(define-public (read-lines . values)
  "Read a handwritten sequence of Formula DSL expressions separated by newlines."
  (define (nonblank? parts)
    (any (lambda (part) (or (math-value? part) (not (string-every char-whitespace? part)))) parts))
  (let lines ((remaining (fragments values)) (pending '()) (output '()))
    (cond
      ((null? remaining)
       (reverse (if (nonblank? pending) (cons (apply read (reverse pending)) output) output)))
      ((math-value? (car remaining)) (lines (cdr remaining) (cons (car remaining) pending) output))
      ((or (string? (car remaining)) (content-text? (car remaining)) (content-layout? (car remaining)))
       (let ((text (math-source-text (list (car remaining)))))
         (let split ((start 0) (pending pending) (output output))
           (let ((end (string-index text #\newline start)))
             (if end
                 (let ((pending (cons (math-source-slice text start end) pending)))
                   (split (+ end 1) '()
                     (if (nonblank? pending) (cons (apply read (reverse pending)) output) output)))
                 (lines (cdr remaining) (cons (math-source-slice text start (string-length text)) pending) output))))))
      (else (error "Formula DSL lines require text fragments or Semantic values" (car remaining))))))
