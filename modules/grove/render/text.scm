;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render text)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (content->text))

(define* (content->text content #:key (handlers '()) (lazy-handlers '()))
  (define (render content)
    (cond
      ((string? content) content)
      ((content-text? content) (content-text-value content))
      ((content-layout? content) (content-layout-raw content))
      ((content-element? content) (render-element content))
      ((content-document? content)
       (string-concatenate
         (map render (content-document-children content))))
      (else (error "not a content node" content))))

  (define (render-element element)
    (let* ((kind (content-element-kind element))
           (lazy-handler (assq-ref lazy-handlers kind)))
      (if lazy-handler
          (lazy-handler element render)
          (let ((body (string-concatenate
                        (map render (content-element-children element))))
                (handler (assq-ref handlers kind)))
            (if handler
                (handler element body)
                (case kind
            ((paragraph) (string-append body "\n"))
            ((summary) (string-append body "\n"))
            ((math-derivation)
             (let ((children (content-element-children element)))
               (if (and (= (length children) 2)
                        (content-element? (cadr children))
                        (eq? 'details (content-element-kind (cadr children))))
                   (render (cadr children)) body)))
            ((heading)
             (string-append body "\n"
                            (make-string (string-length body) #\=) "\n"))
            ((blockquote)
             (string-append
               (string-join
                 (map (lambda (line) (string-append "> " line))
                      (string-split body #\newline))
                 "\n")
               "\n"))
            ((item) (string-append "- " body "\n"))
            ((code) (string-append "`" body "`"))
            ((code-block)
             (string-append "```\n" body
                            (if (string-suffix? "\n" body) "" "\n")
                            "```\n"))
            ((math)
             (let ((source-text body))
               (if (content-element-property element 'tex?)
                   (if (content-element-property element 'display?)
                       (string-append "$$\n" source-text "\n$$\n")
                       (string-append "$" source-text "$"))
                   source-text)))
            ((math-token) body)
            ((math-frac)
             (let ((children (content-element-children element)))
               (string-append (render (car children))
                              "/"
                              (render (cadr children)))))
            ((math-sup)
             (let ((children (content-element-children element)))
               (string-append (render (car children))
                              "^"
                              (render (cadr children)))))
            ((math-sub)
             (let ((children (content-element-children element)))
               (string-append (render (car children))
                              "_"
                              (render (cadr children)))))
            ((math-vector) (string-append body "⃗"))
            ((math-parens) (string-append "(" body ")"))
            ((math-align)
             (string-concatenate
               (map (lambda (row) (string-append (render row) "\n"))
                    (content-element-children element))))
            ((math-row)
             (let ((children (content-element-children element)))
               (if (every (lambda (child)
                            (and (content-element? child)
                                 (eq? (content-element-kind child) 'math-cell)))
                          children)
                   (string-join (filter (lambda (text) (not (string-null? text)))
                                        (map render children)) " ")
                   body)))
            ((math-cell) body)
                  (else body)))))))

  (render content))
