;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble spec)
  #:use-module (system base language)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scribble parser)
  #:use-module (language scribble flow)
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

(define (use-interface-if-needed! module name)
  (let ((interface (resolve-interface name)))
    (unless (memq interface (module-uses module))
      (module-use! module interface))))

(define (use-document-interfaces! module)
  (use-interface-if-needed! module '(grove ir))
  (use-interface-if-needed! module '(language scribble commands)))

(define* (scribble-evaluate-document port environment #:optional (source #f)
                                     #:key (timings #f))
  "Evaluate a document; TIMINGS optionally receives each reading phase and seconds."
  (define elapsed (make-hash-table))
  (define (measure phase thunk)
    (if timings
        (let* ((start (get-internal-real-time)) (value (thunk)))
          (hashq-set! elapsed phase
            (+ (hashq-ref elapsed phase 0)
               (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
          value)
        (thunk)))
  (when source (set-port-filename! port source))
  (save-module-excursion
    (lambda ()
      (set-current-module environment)
      (let loop ((fragments (measure 'parse (lambda () (parse-with-context port source))))
                 (children '())
                 (active-module environment)
                 (form-number 1))
        (if (null? fragments)
            (let ((document (measure 'decode
                              (lambda ()
                                (make-content-document (decode-flow (reverse children)) source)))))
              (when timings
                (for-each (lambda (phase) (timings phase (hashq-ref elapsed phase 0)))
                          '(parse translate evaluate decode)))
              document)
            (let* ((fragment (car fragments))
                   (expression (measure 'translate (lambda () (fragment->expression fragment))))
                   (value (measure 'evaluate
                            (lambda () (evaluate-with-context expression source form-number))))
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
