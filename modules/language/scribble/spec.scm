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

(define* (scribble-evaluate-document port environment #:optional (source #f))
  (let loop ((fragments (scribble-parse port))
             (children '()))
    (if (null? fragments)
        (make-content-document (reverse children) source)
        (let ((value (eval (fragment->expression (car fragments))
                           environment)))
          (cond
            ((content-value? value)
             (loop (cdr fragments) (cons value children)))
            ;; Definitions, imports, and other Scheme forms may return values
            ;; that are not document content.
            (else
             (loop (cdr fragments) children)))))))

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
