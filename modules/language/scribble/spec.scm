;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble spec)
  #:use-module (system base language)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scribble parser)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (scribble
            scribble-evaluate-document))

(define* (fragment->expression fragment #:optional (top-level? #t))
  (cond
    ((string? fragment)
     (cond
       ((string=? fragment "\n")
        `(make-content-layout 'newline ,fragment))
       (top-level? `(make-content-text ,fragment))
       (else fragment)))
    ((pair? fragment)
     (let ((expression (cons (car fragment)
           (map (lambda (child)
                  (fragment->expression child #f))
                (cdr fragment)))))
       (set-source-properties! expression (source-properties fragment))
       expression))
    (else fragment)))

(define (scribble-language-reader port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      `(make-content-document
         (list ,@(map fragment->expression
                      (parse-with-context port (port-filename port)))))))

(define (source-name source)
  (or source "<input>"))

(define (parse-with-context port source)
  (catch #t
    (lambda () (scribble-parse port))
    (lambda (key . args)
      (error
        (format #f "Scribble syntax error in ~a:~a:~a (~a): ~s"
                (source-name source)
                (+ 1 (or (port-line port) 0))
                (+ 1 (or (port-column port) 0))
                key
                args)))))

(define (evaluate-with-context expression source form-number)
  (catch #t
    (lambda () (primitive-eval expression))
    (lambda (key . args)
      (error
        (format #f "Scribble evaluation failed in ~a at top-level form ~a (~a): ~s"
                (source-name source)
                form-number
                key
                args)))))

(define (content-value? value)
  (or (string? value)
      (content-text? value)
      (content-layout? value)
      (content-element? value)
      (content-document? value)))

(define (content-sequence? value)
  (and (list? value)
       (every (lambda (item)
                (or (content-value? item)
                    (content-sequence? item)))
              value)))

(define (flatten-content-sequence values)
  (append-map (lambda (value)
                (if (content-sequence? value)
                    (flatten-content-sequence value)
                    (list value)))
              values))

(define flow-newline (list 'flow-newline))

(define (text-flow-events text source)
  (let loop ((lines (string-split text #\newline))
             (events '()))
    (if (null? lines)
        (reverse events)
        (let* ((line (car lines))
               (events (if (string-null? line)
                           events
                           (cons (make-content-text line source) events))))
          (loop (cdr lines)
                (if (null? (cdr lines))
                    events
                    (cons flow-newline events)))))))

(define (flow-events value)
  (cond
    ((string? value) (text-flow-events value #f))
    ((content-text? value)
     (text-flow-events (content-text-value value)
                       (content-text-source value)))
    ((content-layout? value)
     (if (eq? (content-layout-kind value) 'newline)
         (list flow-newline)
         (list value)))
    ((content-document? value)
     (append-map flow-events (content-document-children value)))
    ((content-sequence? value)
     (append-map flow-events (flatten-content-sequence value)))
    (else (list value))))

(define (flow-block? value)
  (and (content-element? value)
       (content-element-block? value)))

(define (flow-whitespace? value)
  (cond
    ((string? value) (string-every char-whitespace? value))
    ((content-text? value)
     (string-every char-whitespace? (content-text-value value)))
    ((content-layout? value)
     (string-every char-whitespace? (content-layout-raw value)))
    (else #f)))

(define (decode-flow values)
  (define (flush output pending)
    (if (or (null? pending) (every flow-whitespace? pending))
        output
        (let* ((contents (reverse pending))
               (contents (drop-while flow-whitespace? contents))
               (contents
                 (reverse (drop-while flow-whitespace? (reverse contents)))))
          (if (null? contents)
              output
              (cons (make-content-element 'paragraph contents
                                          #:flow-kind 'block)
                    output)))))
  (let loop ((events (append-map flow-events values))
             (pending '())
             (newlines 0)
             (output '()))
    (cond
      ((null? events)
       (reverse (flush output pending)))
      ((eq? (car events) flow-newline)
       (if (= newlines 0)
           (loop (cdr events) pending 1 output)
           (loop (cdr events) '() 0 (flush output pending))))
      ((flow-block? (car events))
       (loop (cdr events) '() 0
             (cons (car events) (flush output pending))))
      ((and (= newlines 1) (flow-whitespace? (car events)))
       (loop (cdr events) pending newlines output))
      (else
       (loop (cdr events)
             (if (and (= newlines 1) (pair? pending))
                 (cons (car events)
                       (cons (make-content-layout 'newline "\n") pending))
                 (cons (car events) pending))
             0
             output)))))

(define (use-interface-if-needed! module name)
  (let ((interface (resolve-interface name)))
    (unless (memq interface (module-uses module))
      (module-use! module interface))))

(define (use-document-interfaces! module)
  (use-interface-if-needed! module '(grove ir))
  (use-interface-if-needed! module '(language scribble commands)))

(define* (scribble-evaluate-document port environment #:optional (source #f))
  (when source (set-port-filename! port source))
  (save-module-excursion
    (lambda ()
      (set-current-module environment)
      (let loop ((fragments (parse-with-context port source))
                 (children '())
                 (active-module environment)
                 (form-number 1))
        (if (null? fragments)
            (make-content-document (decode-flow (reverse children)) source)
            (let* ((value (evaluate-with-context
                            (fragment->expression (car fragments))
                            source form-number))
                   (module (current-module)))
              ;; define-module changes Guile's current module while evaluating
              ;; the form. Preserve the language's default bindings there too.
              (unless (eq? module active-module)
                (use-document-interfaces! module))
              (if (or (content-value? value) (content-sequence? value))
                  (let ((produced
                          (if (content-sequence? value)
                              (flatten-content-sequence value)
                              (list value))))
                    (loop (cdr fragments)
                          (append (reverse produced) children)
                          module
                          (+ form-number 1)))
                  ;; Definitions, imports, and other Scheme forms may return
                  ;; values that are not document content.
                  (loop (cdr fragments) children module
                        (+ form-number 1)))))))))

(define-language scribble
  #:title      "Scribble"
  #:reader     scribble-language-reader
  #:compilers  `((tree-il . ,compile-tree-il))
  #:evaluator  (lambda (exp env) (primitive-eval exp))
  #:printer    write
  #:make-default-environment
    (lambda ()
      (let ((environment (make-fresh-user-module)))
        (module-use! environment (resolve-interface '(grove ir)))
        (module-use! environment
                     (resolve-interface '(language scribble commands)))
        environment)))
