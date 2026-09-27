;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html)
  #:use-module (grove ir)
  #:use-module (sxml simple)
  #:export (content->sxml
            sxml->html))

(define* (content->sxml content #:key (handlers '()))
  (define (render content)
    (cond
      ((string? content) content)
      ((content-text? content) (content-text-value content))
      ((content-layout? content) (content-layout-raw content))
      ((content-element? content) (render-element content))
      ((content-document? content)
       (cons 'article (map render (content-document-children content))))
      (else (error "not a content node" content))))

  (define (render-element element)
    (let* ((kind (content-element-kind element))
           (children (map render (content-element-children element)))
           (handler (assq-ref handlers kind)))
      (if handler
          (handler element children)
          (case kind
            ((bold) (cons 'strong children))
            ((paragraph) (cons 'p children))
            ((heading) (cons 'h2 children))
            ((blockquote) (cons 'blockquote children))
            ((itemize) (cons 'ul children))
            ((item) (cons 'li children))
            ((code) (cons 'code children))
            ((code-block) `(pre (code ,@children)))
            ((verbatim) (cons 'pre children))
            ((math)
             (let ((display? (content-element-property element 'display?))
                   (source-text
                     (content-verbatim-string
                       (content-element-children element))))
               `(,(if display? 'div 'span)
                  (@ (class ,(if display? "math-display" "math-inline")))
                  ,(string-append (if display? "\\[" "\\(")
                                  source-text
                                  (if display? "\\]" "\\)")))))
            (else
             `(span (@ (class ,(symbol->string kind))) ,@children))))))

  (render content))

(define (sxml->html sxml)
  (call-with-output-string (lambda (port) (sxml->xml sxml port))))
