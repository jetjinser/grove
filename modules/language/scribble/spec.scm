;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble spec)
  #:use-module (system base language)
  #:use-module (language scribble parser)
  #:use-module (language scribble content)
  #:use-module (language scribble html)
  #:use-module (sxml simple)
  #:export (scribble))

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
  `(make-content-document
     (list ,@(map fragment->expression (scribble-parse port)))))

(define (compile-to-value exp env opts)
  (let* ((content (eval exp env))
         (xml (call-with-output-string
                (lambda (port)
                  (sxml->xml (content->sxml content) port)))))
    (values `(quote ,xml) env env)))

(define-language scribble
  #:title      "Scribble"
  #:reader     scribble-language-reader
  #:compilers  `((scheme . ,compile-to-value))
  #:printer    write
  #:make-default-environment
    (lambda () (resolve-module '(language scribble commands))))
