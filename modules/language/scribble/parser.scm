;;; SPDX-FileCopyrightText: 2011 Alex Shinn
;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later OR BSD-3-Clause

;;; Commentary:

;; A library for parsing "scribble" format, introduced by Racket.
;; https://docs.racket-lang.org/scribble/.

;;; Code:

(define-module (language scribble parser)
  #:use-module (scheme char)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-14) ; char-set
  #:use-module (srfi srfi-26) ; cut(e)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (scribble-read
            scribble-parse
            scribble-reader))

(define default-escape-char #\@)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general character utils

(define +mirrors+
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)))
(define +delimiters+
  (char-set #\( #\) #\[ #\] #\{ #\} #\" #\|))
(define +scribble-delims+
  (char-set->string
    (char-set-union char-set:whitespace +delimiters+)))
(define +punctuations+
  (char-set #\- #\+ #\! #\< #\> #\[ #\] #\|))

(define-inlinable (char-mirror ch)
  (or (assq-ref +mirrors+ ch) ch))

(define-inlinable (char-delimiter? ch)
  (or (eof-object? ch) (char-whitespace? ch)
      (char-set-contains? +delimiters+ ch)))

(define-inlinable (char-punctuation? ch)
  (char-set-contains? +punctuations+ ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utils

(define (list-prefix? prefix ls)
  (match (cons prefix ls)
    [(() . _) #t]
    [(_ . ()) #f]
    [((x . xs) . (y . ys))
     (and (equal? x y) (list-prefix? xs ys))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scribble reader (standalone, don't use the native reader)

(define scribble-dot   (list "."))
(define scribble-close (list ")"))

(define (if-peek-char ch in pass fail)
  (cond [(eqv? ch (peek-char in))
         (read-char in)
         pass]
        [else fail]))

(define (read-number in)
  (let ([token (read-delimited +scribble-delims+ in 'peek)])
    (or (string->number token)
        (error "invalid numeric syntax:" token))))

(define (read-escaped in terminal)
  (let loop ([ls '()])
    (let ([ch (read-char in)])
      (cond
        [(or (eof-object? ch) (eqv? ch terminal))
         (list->string (reverse ls))]
        [(eqv? ch #\\)
         (loop (cons (read-char in) ls))]
        [else (loop (cons ch ls))]))))

(define (read-symbol in ch)
  (unread-char ch in)
  (string->symbol (read-delimited +scribble-delims+ in 'peek)))

(define* (scrib-read in #:optional (ec default-escape-char))
  (define ch (read-char in))
  (cond
    [(eof-object? ch) ch]
    [(char-whitespace? ch) (scrib-read in)]
    [(eqv? ch ec)
     (scribble-parse-escape in ec)]
    [else
      (case ch
        [(#\( #\[ #\{)
         (let loop ([res '()])
           (let ([x (scrib-read in)])
             (cond [(eof-object? x) (error "unterminated list" x)]
                   [(eq? x scribble-close) (reverse res)]
                   [(eq? x scribble-dot)
                    (let ([y (scrib-read in)])
                      (if (or (eof-object? y) (eq? y scribble-close))
                        (error "unterminated dotted list")
                        (let ([z (scrib-read in)])
                          (if (not (eq? z scribble-close))
                              (error "dot in non-terminal position in list" y z)
                              (append (reverse res) y)))))]
                   [else (loop (cons x res))])))]
        [(#\} #\] #\)) scribble-close]
        [(#\.) (cond
                 [(char-delimiter? (peek-char in)) scribble-dot]
                 [else (unread-char #\. in) (read-number in)])]
        [(#\') (list 'quote (scrib-read in))]
        [(#\`) (list 'quasiquote (scrib-read in))]
        [(#\,) (list (if-peek-char #\@ in 'unquote-splicing 'unquote)
                     (scrib-read in))]
        [(#\;) (read-line in) (scrib-read in)]
        [(#\|) (string->symbol (read-escaped in #\|))]
        [(#\") (read-escaped in #\")]
        [(#\+ #\-)
         (cond [(char-numeric? (peek-char in))
                (unread-char ch in)
                (read-number in)]
               [else (read-symbol in ch)])]
        [(#\#)
         (case (peek-char in)
           [(#\t #\f) (eqv? (read-char in) #\t)]
           [(#\() (list->vector (scrib-read in))]
           [(#\\)
            (read-char in)
            (if (char-alphabetic? (peek-char in))
              (let ([name (scrib-read in)])
                (case name
                  [(space) #\space]
                  [(newline) #\newline]
                  [else (string-ref (symbol->string name) 0)]))
              (read-char in))]
           [(#\:)
            (read-char in)
            (let ([sym (scrib-read in)])
              (if (symbol? sym)
                (symbol->keyword sym)
                (error "invalid keyword syntax")))]
           [else (error "unknown # syntax")])]
        [else
         (if (char-numeric? ch)
             (begin (unread-char ch in)
                    (read-number in))
             (read-symbol in ch))])]))

(define* (scribble-read in #:optional (ec default-escape-char))
  (let ([res (scrib-read in ec)])
    (cond [(eq? res scribble-dot)
           (error "invalid . in source")]
          [(eq? res scribble-close)
           (error "too many )'s")]
          [else res])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scribble parser

(define (read-punctuation in)
  (if (not (eqv? #\| (peek-char in)))
      '()
      (let loop ([ls '()])
        (let ([c (peek-char in)])
          (cond [(or (eof-object? c) (not(char-punctuation? c))) ls]
                [else (loop (cons (char-mirror (read-char in)) ls))])))))

(define (read-prefix-wrapper in)
  (let loop ([wrap identity])
    (case (peek-char in)
      [(#\') (read-char in) (loop (cut wrap (list 'quote <>)))]
      [(#\`) (read-char in) (loop (cut wrap (list 'quasiquote <>)))]
      [(#\,)
       (read-char in)
       (cond [(eqv? #\@ (peek-char in))
              (read-char in)
              (loop (cut wrap (list 'unquote-splicing <>)))]
             [else (loop (cut wrap (list 'unquote <>)))])]
      [else wrap])))

(define (scribble-parse-escape in ec)
  (cond
    [(eqv? #\" (peek-char in))
     (scribble-read in)]
    [else
     (let* ([wrap  (read-prefix-wrapper in)]
            [cmd   (if (memv (peek-char in) '(#\[ #\{)) '() (list (scribble-read in ec)))]
            [data? (eqv? (peek-char in) #\[)]
            [data  (if data? (scribble-read in ec) '())]
            [punc  (read-punctuation in)]
            [body? (eqv? (peek-char in) #\{)]
            [body  (if body?
                     (begin (read-char in)
                            (scribble-parse in punc ec #:need-close? #t))
                     '())])
       (wrap (if (or data? body?)
               (append cmd data body)
               (car cmd))))]))

(define* (scribble-parse in #:optional (init-punc '())
                                       (escape-char default-escape-char)
                            #:key (need-close? #f))
  (define comment-char #\;)
  (define bracket-char #\[)
  (define brace-char   #\{)
  (define close-bracket-char (char-mirror bracket-char))
  (define close-brace-char   (char-mirror brace-char))

  (define-inlinable (collect str res)
    (if (pair? str) (cons (list->string (reverse str)) res) res))
  (define-inlinable (skip-space in)
    (let ([ch (peek-char in)])
      (cond [(char-whitespace? ch) (read-char in) (skip-space in)]
            [(eqv? ch #\;) (read-line in) (skip-space in)])))

  (define (tok str res punc depth)
    (let ([c (read-char in)])
      (cond
        [(eof-object? c)
         (if (and (zero? depth) (not need-close?))
           (reverse (collect str res))
           (error "unterminated expression" punc))]
        [(and (eqv? c escape-char) (list-prefix? punc str))
         (let ([c (peek-char in)])
           (cond
             [(eof-object? c)
              (error "dangling escape character at end of input")]
             [(char-whitespace? c)
              (tok (cons c str) res punc depth)]
             [(eqv? c comment-char)
              (read-char in)
              (cond [(eqv? brace-char (peek-char in))
                     (scribble-parse-escape in escape-char)]
                    [else (read-line in)])
              (tok str res punc depth)]
             [(eqv? c #\|)
              (read-char in)
              (let loop ([ls (collect str res)])
                (skip-space in)
                (cond [(eqv? #\| (peek-char in)) (read-char in) (tok '() ls punc depth)]
                      [else (loop (cons (scribble-read in) ls))]))]
             [else
              (let ([str (drop str (length punc))]
                    [x (scribble-parse-escape in escape-char)])
                (if (string? x)
                  (tok (append (reverse (string->list x)) str) res punc depth)
                  (tok '() (cons x (collect str res)) punc depth)))]))]
        [(eqv? c brace-char)
         (tok (cons c str) res punc (+ depth 1))]
        [(eqv? c close-brace-char)
         (cond
          [(zero? depth)
           (let loop ([p punc] [ls '()])
             (cond [(and need-close? (null? p))
                    (reverse (collect str res))]
                   [(and (null? p) (null? ls))
                    (error "unexpected closing brace")]
                   [(not (eqv? (car p) (peek-char in)))
                    (tok (append ls (cons c str)) res punc (- depth 1))]
                   [else
                    (loop (cdr p) (cons (read-char in) ls))]))]
          [else (tok (cons c str) res punc (- depth 1))])]
        [(eqv? c #\newline)
         (let* ([res (collect str res)]
                [res (if (and (null? res) (null? str))
                       res
                       (cons "\n" res))])
           (tok '() res punc depth))]
        [else
         (tok (cons c str) res punc depth)])))
  ;; begin
  (tok '() '() init-punc 0))


(define (scribble-reader port env)
  (scribble-parse port))
