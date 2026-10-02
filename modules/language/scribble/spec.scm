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

(define (fragment-source-location fragment)
  (let* ((properties (source-properties fragment))
         (filename (assq-ref properties 'filename))
         (line (assq-ref properties 'line))
         (column (assq-ref properties 'column)))
    (and (string? filename) (integer? line) (integer? column)
         `((file . ,filename)
           (line . ,(+ line 1))
           (column . ,(+ column 1))))))

(define (locate-expression expression fragment)
  (let ((location (fragment-source-location fragment)))
    (if location
        `(content-with-source-location ,expression ',location)
        expression)))

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
       (if top-level? expression (locate-expression expression fragment))))
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

(define (location-on-next-line location offset)
  (and location
       (let ((line (assq-ref location 'line)))
         (map (lambda (entry)
                (case (car entry)
                  ((line) (cons 'line (+ line offset)))
                  ((column) (cons 'column 1))
                  (else entry)))
              location))))

(define (text-flow-events text source location)
  (let loop ((lines (string-split text #\newline))
             (line-offset 0)
             (events '()))
    (if (null? lines)
        (reverse events)
        (let* ((line (car lines))
               (events (if (string-null? line)
                           events
                           (cons (make-content-text
                                   line source
                                   #:location
                                   (and location
                                        (location-on-next-line
                                          location line-offset)))
                                 events))))
          (loop (cdr lines) (1+ line-offset)
                (if (null? (cdr lines))
                    events
                    (cons flow-newline events)))))))

(define (flow-events value)
  (cond
    ((string? value) (text-flow-events value #f #f))
    ((content-text? value)
     (text-flow-events (content-text-value value)
                       (content-text-source value)
                       (content-text-location value)))
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
              (let* ((paragraph
                       (make-content-element 'paragraph contents
                                             #:flow-kind 'block))
                     (first (car contents))
                     (location
                       (cond
                         ((content-text? first)
                          (content-text-location first))
                         ((content-element? first)
                          (content-element-property first 'source-location))
                         (else #f))))
                (cons (if location
                          (content-with-source-location paragraph location)
                          paragraph)
                      output))))))
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
            (let* ((fragment (car fragments))
                   (value (evaluate-with-context
                            (fragment->expression fragment)
                            source form-number))
                   (location (fragment-source-location fragment))
                   (value (if location
                              (content-with-source-location value location)
                              value))
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
