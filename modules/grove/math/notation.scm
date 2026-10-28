;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math notation)
  #:use-module (grove private anchor)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove ir)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (language scribble command)
  #:use-module ((grove math private notation) #:prefix notation:)
  #:use-module (grove math expression)
  #:use-module (grove math layout)
  #:use-module (grove math context)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13))

;; Notation nodes carry layout, never mathematical assertions.
(define (with-math-id element id)
  "Attach ID as the container anchor when given, leaving ELEMENT otherwise."
  (if id
      (content-with-property element 'id (checked-anchor-id id))
      element))

(define*-public (math #:key (style #f) (id #f) . source)
  (set! source (command-body source))
  (with-math-id
    (content-with-style (notation:math (decode-math-fragments source)) style)
    id))

(define*-public (display-math #:key (style #f) (id #f) . source)
  (set! source (command-body source))
  (with-math-id
    (content-with-style (notation:display-math (decode-math-fragments source)) style)
    id))

(define math-token notation:token)

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
               math-row math-cell math-align math-column math-style))))

(define (decode-math-fragment value)
  (cond
    ((math-value? value) (list (math-value->layout value (math-assumptions '()))))
    ((string? value) (scan-math-text value))
    ((content-text? value) (scan-math-text (ir:content-text-value/raw value)))
    ((content-layout? value) (scan-math-text (ir:content-layout-raw/raw value)))
    ((list? value) (decode-math-fragments value))
    ((and (content-element? value)
          (eq? (content-element-kind value) 'math-token))
     (list value))
    ((math-fragment? value)
     (list (content-element-with-children
             value
             (decode-math-fragments (ir:content-element-children/raw value)))))
    (else (error "unsupported value in math expression" value))))

(define (decode-math-fragments values)
  (append-map decode-math-fragment values))

(define (math-argument value)
  (decode-math-fragments (if (list? value) value (list value))))

(define*-public (frac #:key (style #f) . arguments)
  (match (command-body arguments)
    ((numerator denominator) (content-with-style (notation:frac (math-argument numerator) (math-argument denominator)) style))
    (_ (error "frac requires exactly two arguments" arguments))))

(define*-public (vec #:key (style #f) . body)
  (set! body (command-body body))
  (content-with-style (notation:vec (decode-math-fragments body)) style))

(define*-public (sup #:key (style #f) . arguments)
  (match (command-body arguments)
    ((base exponent) (content-with-style (notation:sup (math-argument base) (math-argument exponent)) style))
    (_ (error "sup requires exactly two arguments" arguments))))

(define*-public (sub #:key (style #f) . arguments)
  (match (command-body arguments)
    ((base index) (content-with-style (notation:sub (math-argument base) (math-argument index)) style))
    (_ (error "sub requires exactly two arguments" arguments))))

(define*-public (parens #:key (style #f) . body)
  (set! body (command-body body))
  (content-with-style (notation:parens (decode-math-fragments body)) style))

(define (align-fragment-events fragment)
  (cond
    ((content-layout? fragment)
     (if (eq? (content-layout-kind fragment) 'newline)
         '((line))
         (list (list 'value fragment))))
    ((or (string? fragment) (content-text? fragment))
      (let loop ((characters
                   (string->list (if (content-text? fragment)
                                     (ir:content-text-value/raw fragment)
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
         (notation:row
           (map (lambda (contents)
                  (notation:cell (decode-math-fragments contents)))
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

(define*-public (align #:key (style #f) . fragments)
  (set! fragments (command-body fragments))
  (content-with-style (notation:align (align-lines fragments)) style))

(define*-public (column #:key (style #f) . entries)
  "Lay out entries as a column; this notation does not assert vector semantics."
  (set! entries (command-body entries))
  (content-with-style (notation:column (map (lambda (entry) (notation:row (decode-math-fragments (list entry)))) entries)) style))
