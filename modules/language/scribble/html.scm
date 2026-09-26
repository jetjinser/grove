;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble html)
  #:use-module (language scribble content)
  #:export (content->sxml))

(define (element->sxml element)
  (let* ((kind (content-element-kind element))
         (children (map content->sxml (content-element-children element))))
    (case kind
      ((bold) (cons 'strong children))
      ((paragraph) (cons 'p children))
      ((verbatim) (cons 'pre children))
      ((link)
       `(a (@ (href ,(string-append
                       "/" (content-element-property element 'target) ".html")))
           ,@children))
      ((math)
       (let ((display? (content-element-property element 'display?))
             (source-text
               (content-verbatim-string (content-element-children element))))
         `(,(if display? 'div 'span)
            (@ (class ,(if display? "math-display" "math-inline")))
            ,(string-append (if display? "\\[" "\\(")
                            source-text
                            (if display? "\\]" "\\)")))))
      (else
       `(span (@ (class ,(symbol->string kind))) ,@children)))))

(define (content->sxml content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content) (element->sxml content))
    ((content-document? content)
     (cons 'article
           (map content->sxml (content-document-children content))))
    (else (error "not a content node" content))))
