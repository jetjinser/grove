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
  #:use-module (srfi srfi-13) ; string utilities
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (scribble-read
            scribble-parse))

;; All ranges share one immutable input buffer. Slices are made only when a
;; consumer asks for source-text, rather than copying every nested expression.
(define current-source (make-parameter #f))
(define reading-expression? (make-parameter #f))

(define (call-with-source-input in procedure)
  (if (or (current-source) (reading-expression?))
      (procedure in)
      (let* ((line (port-line in)) (column (port-column in))
             (filename (port-filename in))
             (text (get-string-all in))
             (offsets (make-hash-table))
             (port (open-input-string text)))
        (set-port-encoding! port "UTF-8")
        ;; String ports report UTF-8 byte offsets, whereas columns expand tabs.
        ;; Map byte boundaries once to character offsets; neither is a slice index.
        (let loop ((character 0) (byte 0))
          (hashv-set! offsets byte character)
          (when (< character (string-length text))
            (let* ((code (char->integer (string-ref text character)))
                   (width (cond ((< code #x80) 1) ((< code #x800) 2)
                                ((< code #x10000) 3) (else 4))))
              (loop (+ character 1) (+ byte width)))))
        (set-port-filename! port filename)
        (set-port-line! port line)
        (set-port-column! port column)
        (dynamic-wind
          (lambda () #t)
          (lambda ()
            (parameterize ((current-source (cons text offsets)))
              (procedure port)))
          (lambda ()
            ;; Preserve the caller's error position even though input is buffered.
            (set-port-line! in (port-line port))
            (set-port-column! in (port-column port))
            (close-port port))))))

(define (position in)
  `((filename . ,(port-filename in))
    (line . ,(port-line in)) (column . ,(port-column in))
    ,@(if (current-source)
          `((offset . ,(hashv-ref (cdr (current-source)) (ftell in))))
          '())))

(define (locate-at! value start end)
  (when (or (pair? value) (string? value))
    (let ((range (if (current-source)
                      `((end-line . ,(assq-ref end 'line))
                        (end-column . ,(assq-ref end 'column))
                        (start-offset . ,(assq-ref start 'offset))
                        (end-offset . ,(assq-ref end 'offset))
                        (source-buffer . ,(car (current-source))))
                      '())))
      (set-source-properties! value
        (append start range (source-properties value)))))
  value)

(define (locate! value start in)
  (locate-at! value start (position in)))

(define default-escape-char #\@)

(define-syntax define-constant
  (syntax-rules ()
    ((_ name value)
     (define-syntax name (identifier-syntax value)))))

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

(define (horizontal-space? ch)
  (memv ch '(#\space #\tab)))

(define (source-newline? value)
  (and (string? value) (string=? value "\n")
       (assq-ref (source-properties value) 'scribble-newline)))

(define (split-body-lines fragments)
  (let loop ((fragments fragments) (line '()) (lines '()))
    (cond
      ((null? fragments)
       (reverse (cons (list (reverse line) #f) lines)))
      ((source-newline? (car fragments))
       (loop (cdr fragments) '()
             (cons (list (reverse line) (car fragments)) lines)))
      (else (loop (cdr fragments) (cons (car fragments) line) lines)))))

(define (line-content? fragments)
  (any (lambda (fragment)
         (or (not (string? fragment))
             (string-any (lambda (ch) (not (horizontal-space? ch))) fragment)))
       fragments))

(define (line-indent fragments)
  (let loop ((fragments fragments) (indent ""))
    (if (or (null? fragments) (not (string? (car fragments))))
        indent
        (let* ((text (car fragments))
               (count (let scan ((i 0))
                        (if (and (< i (string-length text))
                                 (horizontal-space? (string-ref text i)))
                            (scan (+ i 1)) i)))
               (indent (string-append indent (substring text 0 count))))
          (if (< count (string-length text)) indent
              (loop (cdr fragments) indent))))))

(define (source-slice text start end)
  (let* ((properties (source-properties text))
         (slice (substring text start end))
         (column (assq-ref properties 'column))
         (offset (assq-ref properties 'offset))
         (start-offset (assq-ref properties 'start-offset))
         (column-at
          (lambda (text column)
            (string-fold
              (lambda (ch column)
                (if (char=? ch #\tab)
                    (+ column (- 8 (modulo column 8)))
                    (+ column 1)))
              column text))))
    (when properties
      (set-source-properties!
        slice
        (map (lambda (property)
               (case (car property)
                 ((column) (cons 'column
                                 (column-at (substring text 0 start) column)))
                 ((offset) (cons 'offset (+ offset start)))
                 ((start-offset) (cons 'start-offset (+ start-offset start)))
                 ((end-offset) (cons 'end-offset
                                     (+ start-offset end)))
                 ((end-column)
                  (cons 'end-column
                        (column-at (substring text 0 end) column)))
                 (else property)))
             properties)))
    slice))

(define (line-strip-prefix fragments count)
  (let loop ((fragments fragments) (remaining count))
    (cond
      ((or (zero? remaining) (null? fragments)) fragments)
      ((not (string? (car fragments))) fragments)
      (else
       (let* ((text (car fragments))
              (spaces (let scan ((i 0))
                        (if (and (< i (string-length text))
                                 (horizontal-space? (string-ref text i)))
                            (scan (+ i 1)) i)))
              (take (min remaining spaces)))
         (if (= take (string-length text))
             (loop (cdr fragments) (- remaining take))
             (cons (source-slice text take (string-length text))
                   (cdr fragments))))))))

(define (line-take-prefix fragments count)
  (let loop ((fragments fragments) (remaining count) (prefix '()))
    (cond
      ((or (zero? remaining) (null? fragments))
       (values (reverse prefix) fragments))
      ((not (string? (car fragments)))
       (values (reverse prefix) fragments))
      (else
       (let* ((text (car fragments))
              (spaces (let scan ((i 0))
                        (if (and (< i (string-length text))
                                 (horizontal-space? (string-ref text i)))
                            (scan (+ i 1)) i)))
              (take (min remaining spaces)))
         (if (= take (string-length text))
             (loop (cdr fragments) (- remaining take) (cons text prefix))
             (values
               (reverse (cons (source-slice text 0 take) prefix))
               (cons (source-slice text take (string-length text))
                     (cdr fragments)))))))))

(define (line-strip-trailing-space fragments)
  (let loop ((fragments (reverse fragments)) (suffix '()))
    (cond
      ((null? fragments)
       (values '() (list->string suffix)))
      ((not (string? (car fragments)))
       (values (reverse fragments) (list->string suffix)))
      (else
       (let* ((text (car fragments))
              (start (let scan ((i (- (string-length text) 1)))
                       (if (and (>= i 0)
                                (horizontal-space? (string-ref text i)))
                           (scan (- i 1)) (+ i 1)))))
         (cond
           ((= start (string-length text))
            (values (reverse fragments) (list->string suffix)))
           ((zero? start)
            (loop (cdr fragments)
                  (append (string->list text) suffix)))
           (else
            (values (append (reverse (cdr fragments))
                            (if (zero? start) '()
                                (list (source-slice text 0 start))))
                    (list->string
                      (append (string->list (substring text start)) suffix))))))))))

(define (mark-indentation value)
  (set-source-property! value 'scribble 'indentation)
  value)

(define (decorate-newline newline following-indent)
  (when newline
    (set-source-property!
      newline 'scribble
      (list 'newline (string-append "\n" following-indent))))
  newline)

(define (normalize-body fragments)
  "Remove Scribble's body margin while retaining original indentation metadata."
  (let* ((lines (split-body-lines fragments))
         (has-content? (any (lambda (entry) (line-content? (car entry))) lines))
         (has-newline? (any cadr lines)))
    (if (or (not has-content?) (not has-newline?))
        fragments
        (let* ((remove-opening?
                 (and (pair? lines) (null? (caar lines)) (cadar lines)))
               (remove-closing?
                 (and (> (length lines) 1)
                      (not (line-content? (car (last lines))))
                      (cadr (list-ref lines (- (length lines) 2)))))
               (start (if remove-opening? 1 0))
               (end (if remove-closing? (- (length lines) 1) (length lines)))
               (selected (take (drop lines start) (- end start)))
               (inline-first?
                 (and (pair? selected) (line-content? (caar selected))
                      (not remove-opening?)))
               (indents
                 (filter-map
                   (lambda (entry index)
                     (and (line-content? (car entry))
                          (not (and inline-first? (= index 0)))
                          (line-indent (car entry))))
                   selected (iota (length selected))))
               (margin (if (null? indents) ""
                           (let ((shortest
                                   (apply min (map string-length indents))))
                             (if (zero? shortest) ""
                                 (substring (car indents) 0 shortest)))))
               (original-lines (drop lines start)))
          (let loop ((lines selected) (index 0) (result '()))
            (if (null? lines)
                (append-map identity (reverse result))
                (let* ((entry (car lines))
                       (line (car entry))
                       (newline (cadr entry))
                       (content? (line-content? line))
                       (preserve-leading-space?
                         (and (= index 0) (not content?) (pair? line)))
                       (remove-count
                         (cond ((and inline-first? (= index 0)) 0)
                               (preserve-leading-space? 0)
                               ((not content?) (string-length (line-indent line)))
                               ((string-prefix? margin (line-indent line))
                                (string-length margin))
                               (else 0)))
                       (rest (line-strip-prefix line remove-count))
                       (deindent-line?
                         (and content?
                              (not (and inline-first? (= index 0)))))
                       (relative-source rest)
                       (relative-indent
                         (if deindent-line? (line-indent relative-source) ""))
                       (indent-values
                         (call-with-values
                           (lambda ()
                             (line-take-prefix relative-source
                               (string-length relative-indent)))
                           list))
                       (indent-pieces (car indent-values))
                       (rest (if deindent-line?
                                 (cadr indent-values)
                                 relative-source))
                       (closing-newline?
                         (and remove-closing?
                              (= index (- (length selected) 1))))
                       (values (if (and newline (not closing-newline?)
                                        (not preserve-leading-space?))
                                   (call-with-values
                                     (lambda () (line-strip-trailing-space rest))
                                     list)
                                   (list rest "")))
                       (rest (car values))
                       (trailing (cadr values))
                       (next-line
                         (and (< (+ index 1) (length original-lines))
                              (list-ref original-lines (+ index 1))))
                       (newline (decorate-newline
                                  (and (not closing-newline?) newline)
                                  (if next-line
                                      (line-indent (car next-line)) "")))
                       (pieces
                         (append
                           (map mark-indentation indent-pieces)
                           rest
                           (if (and newline (not (string-null? trailing)))
                               (begin
                                 (set-source-property! newline
                                   'scribble-trailing-space trailing)
                                 '())
                               '())
                           (if newline (list newline) '()))))
                  (loop (cdr lines) (+ index 1) (cons pieces result)))))))))

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

(define (read-number in)
  (let ([token (read-delimited +scribble-delims+ in 'peek)])
    (or (string->number token)
        (and (let ((separator (string-index token #\-)))
               (and separator
                    (> separator 0)
                    (< separator (- (string-length token) 1))
                    (string-every char-numeric? (substring token 0 separator))
                    (string-every char-numeric?
                                  (substring token (+ separator 1)))))
             (list 'quote (string->symbol token)))
        (error "invalid numeric syntax:" token))))

(define (read-escaped in terminal)
  (let loop ([ls '()])
    (let ([ch (read-char in)])
      (cond
        [(eof-object? ch)
         (error "unterminated escaped sequence")]
        [(eqv? ch terminal)
         (list->string (reverse ls))]
        [(eqv? ch #\\)
         (let ((escaped (read-char in)))
           (when (eof-object? escaped)
             (error "dangling escape at end of escaped sequence"))
           (loop (cons escaped ls)))]
        [else (loop (cons ch ls))]))))

(define (read-symbol in ch)
  (unread-char ch in)
  (string->symbol (read-delimited +scribble-delims+ in 'peek)))

(define (read-list in)
  (let loop ([res '()])
    (let ([x (scrib-read in)])
      (cond [(eof-object? x) (error "unterminated list" x)]
            [(eq? x scribble-close) (reverse res)]
            [(eq? x scribble-dot)
             (let ([y (scrib-read in)])
               (when (or (eof-object? y) (eq? y scribble-close))
                 (error "unterminated dotted list"))
               (let ([z (scrib-read in)])
                 (unless (eq? z scribble-close)
                   (error "dot in non-terminal position in list" y z))
                 (append (reverse res) y)))]
            [else (loop (cons x res))]))))

(define* (scrib-read in #:optional [ec default-escape-char])
  (let loop ()
    (when (and (char? (peek-char in)) (char-whitespace? (peek-char in)))
      (read-char in) (loop)))
  (let* ((start (position in)) (value (%scrib-read in ec)))
    (if (or (eq? value scribble-dot) (eq? value scribble-close)) value
        (locate! value start in))))

(define* (%scrib-read in #:optional [ec default-escape-char])
  (define ch (read-char in))
  (cond
    [(eof-object? ch) ch]
    [(char-whitespace? ch) (scrib-read in)]
    [(eqv? ch ec)
     (scribble-parse-escape in ec)]
    [else
      (case ch
        [(#\( #\[ #\{) (read-list in)]
        [(#\} #\] #\)) scribble-close]
        [(#\.) (cond
                 [(char-delimiter? (peek-char in)) scribble-dot]
                 [(char-numeric? (peek-char in))
                  (unread-char #\. in) (read-number in)]
                 [else (read-symbol in #\.)])]
        [(#\') (list 'quote (scrib-read in))]
        [(#\`) (list 'quasiquote (scrib-read in))]
        [(#\,) (cond [(eqv? #\@ (peek-char in))
                      (read-char in)
                      (list 'unquote-splicing (scrib-read in))]
                     [else
                      (list 'unquote (scrib-read in))])]
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
           [(#\') (read-char in) (list 'syntax (scrib-read in))]
           [(#\`) (read-char in) (list 'quasisyntax (scrib-read in))]
           [(#\,)
            (read-char in)
            (if (eqv? #\@ (peek-char in))
                (begin (read-char in) (list 'unsyntax-splicing (scrib-read in)))
                (list 'unsyntax (scrib-read in)))]
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
        [else (cond [(char-numeric? ch) (unread-char ch in)
                                        (read-number in)]
                    [else (read-symbol in ch)])])]))

(define* (scribble-read in #:optional [ec default-escape-char])
  (let ([res (parameterize ((reading-expression? #t)) (scrib-read in ec))])
    (cond [(eq? res scribble-dot)
           (error "invalid . in source")]
          [(eq? res scribble-close)
           (error "too many )'s")]
          [else res])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scribble parser

(define (read-punctuation in)
  (if (eqv? #\| (peek-char in))
    (let loop ([ls '()])
      (let ([c (peek-char in)])
        (cond [(or (eof-object? c) (not (char-punctuation? c))) ls]
              [else (loop (cons (char-mirror (read-char in)) ls))])))
    '()))

(define (read-prefix-wrapper in)
  (let loop ([wrap identity])
    (case (peek-char in)
      [(#\') (read-char in) (loop (lambda (value) (wrap (list 'quote value))))]
      [(#\`) (read-char in) (loop (lambda (value) (wrap (list 'quasiquote value))))]
      [(#\,)
       (read-char in)
       (cond [(eqv? #\@ (peek-char in))
              (read-char in)
              (loop (lambda (value) (wrap (list 'unquote-splicing value))))]
             [else (loop (lambda (value) (wrap (list 'unquote value))))])]
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
            [body  (cond [body? (read-char in)
                                (normalize-body
                                  (scribble-parse in punc ec #:need-close? #t))]
                         [else '()])]
            [extra-bodies
              (if (and body? (pair? cmd))
                  (let loop ([arguments '()])
                    (if (eqv? (peek-char in) #\{)
                        (begin
                          (read-char in)
                          (let ((fragments
                                  (normalize-body
                                    (scribble-parse in punc ec
                                                    #:need-close? #t))))
                            (loop
                              (cons (if (= (length fragments) 1)
                                        (car fragments)
                                        (cons 'list fragments))
                                    arguments))))
                        (reverse arguments)))
                  '())]
            [body-arguments
              (if (pair? extra-bodies)
                  (list (if (= (length body) 1)
                            (car body)
                            (cons 'list body)))
                  body)])
       (let ((expression (wrap (if (or data? body?)
                                 (append cmd data body-arguments extra-bodies)
                                 (car cmd)))))
         ;; Only an at-command produces a content-boundary annotation. Ordinary
         ;; Scheme forms, including macro input, retain their original syntax.
         (when (and (pair? expression) (pair? cmd) (symbol? (car cmd)))
           (set-source-property! expression 'scribble-command #t))
         expression))]))

(define* (scribble-parse in #:optional (init-punc '())
                                       (escape-char default-escape-char)
                            #:key (need-close? #f))
  (call-with-source-input in
    (lambda (in) (%scribble-parse in init-punc escape-char #:need-close? need-close?))))

(define* (%scribble-parse in #:optional [init-punc '()]
                                       [escape-char default-escape-char]
                            #:key [need-close? #f])
  (define-constant comment-char     #\;)
  (define-constant brace-char       #\{)
  (define-constant close-brace-char (char-mirror brace-char))

  (define text-start #f)

  (define-inlinable (collect str res end)
    (if (pair? str)
        (let ((text (list->string (reverse str))))
          (locate-at! text text-start end)
          (cons text res)) res))
  (define-inlinable (skip-space in)
    (let ([ch (peek-char in)])
      (cond [(char-whitespace? ch)  (read-char in) (skip-space in)]
            [(eqv? ch comment-char) (read-line in) (skip-space in)])))

  (define (tok str res punc depth)
    (let* ([start (position in)] [c (read-char in)])
      (when (null? str) (set! text-start start))
      (cond
        [(eof-object? c)
         (if (and (zero? depth) (not need-close?))
           (reverse (collect str res start))
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
              (let loop ([ls (collect str res start)])
                (skip-space in)
                (cond [(eqv? #\| (peek-char in)) (read-char in) (tok '() ls punc depth)]
                      [else (loop (cons (scribble-read in) ls))]))]
             [else
              (let* ([str (drop str (length punc))]
                     [saved-start text-start]
                     [x (scribble-parse-escape in escape-char)])
                (set! text-start saved-start)
                (when (pair? x) (locate! x start in))
                (if (string? x)
                  (tok (append (reverse (string->list x)) str) res punc depth)
                  (tok '() (cons x (collect str res start)) punc depth)))]))]
        [(eqv? c brace-char)
         (tok (cons c str) res punc (+ depth 1))]
        [(eqv? c close-brace-char)
         (cond
          [(zero? depth)
           (let loop ([p punc] [ls '()])
             (cond [(and need-close? (null? p))
                    (reverse (collect str res start))]
                   [(and (null? p) (null? ls))
                    (error "unexpected closing brace")]
                   [(not (eqv? (car p) (peek-char in)))
                    (tok (append ls (cons c str)) res punc (- depth 1))]
                   [else
                    (loop (cdr p) (cons (read-char in) ls))]))]
          [else (tok (cons c str) res punc (- depth 1))])]
        [(eqv? c #\newline)
         (let* ([res (collect str res start)]
                [res (if (and (null? res) (null? str) (not need-close?))
                       res
                       (let ((text (string #\newline)))
                         (set-source-property! text 'scribble-newline #t)
                         (locate! text start in)
                         (cons text res)))])
           (tok '() res punc depth))]
        [else
         (tok (cons c str) res punc depth)])))
  ;; begin
  (tok '() '() init-punc 0))
