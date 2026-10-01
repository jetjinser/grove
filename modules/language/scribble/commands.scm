;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble commands)
  #:use-module (grove ir)
  #:use-module (grove math pipeline)
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
  (make-content-element 'math (decode-math-fragments source)))

(define-public (display-math . source)
  (make-content-element 'math (decode-math-fragments source)
                        #:properties '((display? . #t))
                        #:flow-kind 'block))

;; A function would evaluate the body before installing its assumptions.
(define-syntax-rule (with-assumptions assumptions body ...)
  (with-math-assumptions (list assumptions) (list body ...)))
(export with-assumptions)

;; Guile's lambda* rest argument retains keyword/value pairs. Strip them once
;; for all math commands; lambda* owns option defaults and validation.
(define (math-command-text fragments)
  (let body ((remaining fragments) (text '()))
    (cond ((null? remaining) (content-verbatim-string (reverse text)))
          ((keyword? (car remaining)) (body (cddr remaining) text))
          (else (body (cdr remaining) (cons (car remaining) text))))))

(define (command-assumptions assumptions)
  (if assumptions (list assumptions) '()))

(define-public derive
  (lambda* (#:key (assumptions #f) #:rest source)
    (math-derive (math-command-text source)
      #:assumptions (command-assumptions assumptions))))

(define-public check-derive
  (lambda* (#:key (assumptions #f) #:rest source)
    (let ((lines
            (filter (lambda (line) (not (string-null? line)))
                    (map string-trim-both
                         (string-split (math-command-text source) #\newline)))))
      (math-verification->content
        (math-check-chain lines #:assumptions (command-assumptions assumptions))))))

(define-public differentiate
  (lambda* (variable #:key (assumptions #f) #:rest source)
    (math-completed-report->content
      (math-differentiate (math-command-text source) variable
        #:assumptions (command-assumptions assumptions)))))

(define-public search-derive
  (lambda* (target #:key (assumptions #f) #:rest source)
    (math-report->content
      (math-search (math-command-text source) target
        #:assumptions (command-assumptions assumptions) #:max-results 1))))

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

(define (math-token kind value)
  (make-content-element 'math-token (list value)
                        #:properties `((token-kind . ,kind))))

(define math-letters '(Lu Ll Lt Lm Lo Mn Mc))
(define math-numbers '(Nd Nl No))

(define (scan-math-text text)
  (let ((characters (string->list text)))
    (let loop ((characters characters) (tokens '()))
      (if (null? characters)
          (reverse tokens)
          (let* ((character (car characters))
                 (category (char-general-category character)))
            (cond
              ((char-whitespace? character)
               (loop (cdr characters) tokens))
              ((memq category math-numbers)
               (let collect ((rest (cdr characters))
                             (number (list character)))
                 (if (and (pair? rest)
                          (memq (char-general-category (car rest)) math-numbers))
                     (collect (cdr rest) (cons (car rest) number))
                     (loop rest
                           (cons (math-token 'mn
                                             (list->string (reverse number)))
                                 tokens)))))
              ((memq category math-letters)
               (loop (cdr characters)
                     (cons (math-token 'mi (string character)) tokens)))
              (else
               (let ((operator
                       (case character
                         ((#\-) #\−)
                         ((#\*) #\∗)
                         (else character))))
                 (loop (cdr characters)
                       (cons (math-token 'mo (string operator)) tokens))))))))))

(define (math-fragment? value)
  (and (content-element? value)
       (memq (content-element-kind value)
             '(math-token math-frac math-vector math-sup math-sub math-parens
               math-row math-cell math-align))))

(define (decode-math-fragment value)
  (cond
    ((string? value) (scan-math-text value))
    ((content-text? value) (scan-math-text (content-text-value value)))
    ((list? value) (decode-math-fragments value))
    ((and (content-element? value)
          (eq? (content-element-kind value) 'math-token))
     (list value))
    ((math-fragment? value)
     (list (make-content-element
             (content-element-kind value)
             (decode-math-fragments (content-element-children value))
             (content-element-source value)
             #:properties (content-element-properties value))))
    (else (error "unsupported value in math expression" value))))

(define (decode-math-fragments values)
  (append-map decode-math-fragment values))

(define (math-argument-row value)
  (make-content-element
    'math-row
    (if (list? value) value (list value))))

(define-public (frac numerator denominator)
  (make-content-element 'math-frac
                        (list (math-argument-row numerator)
                              (math-argument-row denominator))))

(define-public (vec . body)
  (make-content-element 'math-vector body))

(define-public (sup base exponent)
  (make-content-element 'math-sup
                        (list (math-argument-row base)
                              (math-argument-row exponent))))

(define-public (sub base index)
  (make-content-element 'math-sub
                        (list (math-argument-row base)
                              (math-argument-row index))))

(define-public (parens . body)
  (make-content-element 'math-parens body))

(define (align-fragment-events fragment)
  (cond
    ((content-layout? fragment)
     (if (eq? (content-layout-kind fragment) 'newline)
         '((line))
         (list (list 'value fragment))))
    ((or (string? fragment) (content-text? fragment))
      (let loop ((characters
                   (string->list (if (content-text? fragment)
                                     (content-text-value fragment)
                                     fragment)))
                 (text '()) (events '()))
        (cond
          ((null? characters)
           (reverse (if (null? text)
                        events
                        (cons (list 'text (list->string (reverse text)))
                              events))))
          ((char=? (car characters) #\newline)
           (loop (cdr characters) '()
                 (cons '(line)
                       (if (null? text)
                           events
                           (cons (list 'text (list->string (reverse text)))
                                 events)))))
          ((char=? (car characters) #\&)
           (loop (cdr characters) '()
                 (cons '(cell)
                       (if (null? text)
                           events
                           (cons (list 'text (list->string (reverse text)))
                                 events)))))
          (else
           (loop (cdr characters) (cons (car characters) text) events)))))
    (else
     (list (list 'value fragment)))))

(define (make-align-row cell cells)
  (let ((all-cells (reverse (cons (reverse cell) cells))))
    (and (not (every null? all-cells))
         (make-content-element
           'math-row
           (map (lambda (contents)
                  (make-content-element
                    'math-cell (decode-math-fragments contents)))
                all-cells)))))

(define (align-lines fragments)
  (let loop ((events (append-map align-fragment-events fragments))
             (cell '()) (cells '()) (rows '()))
    (cond
      ((null? events)
       (reverse (let ((row (make-align-row cell cells)))
                  (if row (cons row rows) rows))))
      ((eq? (caar events) 'line)
       (let ((row (make-align-row cell cells)))
         (loop (cdr events) '() '() (if row (cons row rows) rows))))
      ((eq? (caar events) 'cell)
       (loop (cdr events) '() (cons (reverse cell) cells) rows))
      ((eq? (caar events) 'text)
       (loop (cdr events) (cons (cadar events) cell) cells rows))
      (else
       (loop (cdr events) (cons (cadar events) cell) cells rows)))))

(define-public (align . fragments)
  (make-content-element 'math-align (align-lines fragments)
                        #:flow-kind 'block))
