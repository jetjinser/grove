;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private syntax)
  #:use-module ((grove math private identifier)
                #:select (math-identifier-start-char?
                          math-identifier-char?
                          math-digit-char?))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (math-token?
            make-math-token
            math-token-kind
            math-token-value
            math-token-start
            math-token-end
            math-expression?
            make-math-expression
            math-expression-kind
            math-expression-value
            math-expression-children
            math-expression-start
            math-expression-end
            tokenize-math
            parse-math-tokens
            parse-math))

(define-record-type <math-token>
  (make-math-token kind value start end)
  math-token?
  (kind math-token-kind)
  (value math-token-value)
  (start math-token-start)
  (end math-token-end))

(define-record-type <math-expression>
  (make-math-expression kind value children start end)
  math-expression?
  (kind math-expression-kind)
  (value math-expression-value)
  (children math-expression-children)
  (start math-expression-start)
  (end math-expression-end))

(define (tokenize-math source)
  (unless (string? source)
    (error "math source must be a string" source))
  (let ((length (string-length source)))
    (define (at offset) (string-ref source offset))
    (let loop ((position 0) (tokens '()))
      (if (= position length)
          (reverse (cons (make-math-token 'eof "" length length) tokens))
          (let ((char (at position)))
            (cond
              ((char-whitespace? char) (loop (+ position 1) tokens))
              ((math-identifier-start-char? char)
               (let collect ((end (+ position 1)))
                 (if (and (< end length) (math-identifier-char? (at end)))
                     (collect (+ end 1))
                     (loop end
                       (cons (make-math-token 'identifier
                               (substring source position end)
                               position end)
                             tokens)))))
              ((math-digit-char? char)
               (let collect ((end (+ position 1)))
                 (if (and (< end length) (math-digit-char? (at end)))
                     (collect (+ end 1))
                     (loop end
                       (cons (make-math-token 'number
                               (substring source position end)
                               position end)
                             tokens)))))
              (else
               (let ((kind (case char
                             ((#\+) 'plus)
                             ((#\- #\−) 'minus)
                             ((#\* #\×) 'multiply)
                             ((#\/ #\÷) 'divide)
                             ((#\^) 'power)
                             ((#\_) 'subscript)
                             ((#\=) 'equals)
                             ((#\() 'left-paren)
                             ((#\)) 'right-paren)
                             ((#\,) 'comma)
                             (else #f))))
                 (if kind
                     (loop (+ position 1)
                       (cons (make-math-token kind (string char)
                                              position (+ position 1))
                             tokens))
                     (error "unsupported character in math source"
                            char position))))))))))

(define (parse-math source)
  (parse-math-tokens (tokenize-math source)))

(define (numeric-coefficient-tail? expression)
  (case (math-expression-kind expression)
    ((number) #t)
    ((negate) (numeric-coefficient-tail? (car (math-expression-children expression))))
    ((binary)
     (and (eq? (math-expression-value expression) 'multiply)
          (numeric-coefficient-tail? (cadr (math-expression-children expression)))))
    (else #f)))

(define (parse-math-tokens tokens)
  (let ((position 0))
    (letrec ((current (lambda () (list-ref tokens position)))
             (take! (lambda ()
                      (let ((token (current)))
                        (set! position (+ position 1))
                        token)))
             (expect! (lambda (kind)
                        (if (eq? (math-token-kind (current)) kind)
                            (take!)
                            (error "unexpected token in math expression"
                                   (math-token-value (current))
                                   (math-token-start (current))))))
             (precedence (lambda (kind)
                           (case kind
                             ((equals) 5)
                             ((plus minus) 10)
                             ((multiply divide) 20)
                             ((power) 30)
                             ((subscript) 30)
                             (else -1))))
             (primary
               (lambda ()
                 (let ((token (take!)))
                   (case (math-token-kind token)
                     ((embedded)
                      (make-math-expression 'embedded (math-token-value token) '()
                        (math-token-start token) (math-token-end token)))
                     ((identifier)
                      (let ((name (string->symbol (math-token-value token))))
                        (if (eq? (math-token-kind (current)) 'left-paren)
                            (begin
                              (take!)
                              (let arguments ((args '()))
                                (let ((arg (expression 0)))
                                  (if (eq? (math-token-kind (current)) 'comma)
                                      (begin (take!)
                                             (arguments (cons arg args)))
                                      (let ((close (expect! 'right-paren)))
                                        (make-math-expression 'call name
                                          (reverse (cons arg args))
                                          (math-token-start token)
                                          (math-token-end close)))))))
                            (make-math-expression 'identifier name '()
                              (math-token-start token) (math-token-end token)))))
                     ((number)
                      (make-math-expression 'number (math-token-value token) '()
                        (math-token-start token) (math-token-end token)))
                     ((left-paren)
                      (let ((inside (expression 0)))
                        (let ((close (expect! 'right-paren)))
                          (make-math-expression 'group #f (list inside)
                            (math-token-start token) (math-token-end close)))))
                     ((minus)
                      (let ((operand (expression 30)))
                        (make-math-expression 'negate #f (list operand)
                          (math-token-start token)
                          (math-expression-end operand))))
                     (else
                     (error "expected a math expression"
                             (math-token-value token)
                             (math-token-start token)))))))
             (expression
               (lambda (minimum)
                 (let loop ((left (primary)))
                   (let* ((operator (current))
                          ;; Only numeric coefficients license juxtaposition;
                          ;; identifiers remain whole names and calls.
                          (implicit?
                            (and (memq (math-token-kind operator) '(identifier left-paren embedded))
                                 (numeric-coefficient-tail? left)))
                          (priority (if implicit? 20 (precedence (math-token-kind operator)))))
                     (if (< priority minimum)
                         left
                         (begin
                           (unless implicit? (take!))
                           (let* ((right
                                    (expression
                                      (if (eq? (math-token-kind operator) 'power)
                                          priority (+ priority 1))))
                                  (kind (if implicit? 'multiply (math-token-kind operator)))
                                  (right-children
                                    (math-expression-children right))
                                  (combined
                                    (if (and (eq? kind 'power)
                                             (eq? (math-expression-kind right)
                                                  'binary)
                                             (eq? (math-expression-value right)
                                                  'subscript)
                                             (not (eq? (math-expression-kind
                                                         (car right-children))
                                                       'group)))
                                        (let* ((base
                                                 (make-math-expression
                                                   'binary 'power
                                                   (list left
                                                         (car right-children))
                                                   (math-expression-start left)
                                                   (math-expression-end
                                                     (car right-children))))
                                               (index (cadr right-children)))
                                          (make-math-expression
                                            'binary 'subscript
                                            (list base index)
                                            (math-expression-start left)
                                            (math-expression-end index)))
                                        (make-math-expression
                                          'binary kind
                                          (list left right)
                                          (math-expression-start left)
                                          (math-expression-end right)))))
                             (loop combined)))))))))
      (let ((result (expression 0)))
        (unless (eq? (math-token-kind (current)) 'eof)
          (error "trailing input in math expression"
                 (math-token-value (current))
                 (math-token-start (current))))
        result))))
