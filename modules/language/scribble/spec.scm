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
         (list ,@(map fragment->expression (scribble-parse port))))))

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
      (let loop ((fragments (scribble-parse port))
                 (children '())
                 (active-module environment))
        (if (null? fragments)
            (make-content-document (reverse children) source)
            (let* ((value (primitive-eval
                            (fragment->expression (car fragments))))
                   (module (current-module)))
              ;; define-module changes Guile's current module while evaluating
              ;; the form. Preserve the language's default bindings there too.
              (unless (eq? module active-module)
                (use-document-interfaces! module))
              (if (content-value? value)
                  (loop (cdr fragments) (cons value children) module)
                  ;; Definitions, imports, and other Scheme forms may return
                  ;; values that are not document content.
                  (loop (cdr fragments) children module))))))))

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
