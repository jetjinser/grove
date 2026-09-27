;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble spec)
  #:use-module (system base language)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scribble parser)
  #:use-module (grove ir)
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
     (cons (car fragment)
           (map (lambda (child)
                  (fragment->expression child #f))
                (cdr fragment))))
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

(define (use-interface-if-needed! module name)
  (let ((interface (resolve-interface name)))
    (unless (memq interface (module-uses module))
      (module-use! module interface))))

(define (use-document-interfaces! module)
  (use-interface-if-needed! module '(grove ir))
  (use-interface-if-needed! module '(language scribble commands)))

(define* (scribble-evaluate-document port environment #:optional (source #f))
  (save-module-excursion
    (lambda ()
      (set-current-module environment)
      (let loop ((fragments (parse-with-context port source))
                 (children '())
                 (active-module environment)
                 (form-number 1))
        (if (null? fragments)
            (make-content-document (reverse children) source)
            (let* ((value (evaluate-with-context
                            (fragment->expression (car fragments))
                            source form-number))
                   (module (current-module)))
              ;; define-module changes Guile's current module while evaluating
              ;; the form. Preserve the language's default bindings there too.
              (unless (eq? module active-module)
                (use-document-interfaces! module))
              (if (content-value? value)
                  (loop (cdr fragments) (cons value children) module
                        (+ form-number 1))
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
