;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:export (content->sxml
            sxml->html))

(define* (content->sxml content #:key (handlers '()) (lazy-handlers '()))
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
           (lazy-handler (assq-ref lazy-handlers kind)))
      (if lazy-handler
          (lazy-handler element render)
          (if (eq? kind 'math)
              (render-math element)
          (let ((children (map render (content-element-children element)))
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
            ((details) (cons 'details children))
            ((summary) (cons 'summary children))
            ((math-derivation math-diagnostics)
             `(div (@ (class ,(symbol->string kind))) ,@children))
            ((math-diagnostic) `(aside (@ (class "math-diagnostic")) ,@children))
                  (else
                   `(span (@ (class ,(symbol->string kind))) ,@children)))))))))

  (define (render-math-children children)
    (map render-math-child children))

  (define (render-math-row children)
    `(mrow ,@(render-math-children children)))

  (define (render-math-child child)
    (if (not (content-element? child))
        (error "math children must be tokenized" child)
        (let ((kind (content-element-kind child))
              (children (content-element-children child)))
          (case kind
            ((math-token)
             (let ((tag (content-element-property child 'token-kind)))
               `(,tag
                 ,@(let ((rule (content-element-property child 'rule))
                         (path (content-element-property child 'path)))
                     (if rule `((@ (data-rule ,rule) (data-path ,path))) '()))
                 ,(content-verbatim-string children))))
            ((math-frac)
             (unless (= (length children) 2)
               (error "MathML fraction requires numerator and denominator" child))
             `(mfrac ,(render-math-child (list-ref children 0))
                     ,(render-math-child (list-ref children 1))))
            ((math-sup math-sub)
             (unless (= (length children) 2)
               (error "MathML attachment requires a base and script" child))
             `(,(if (eq? kind 'math-sup) 'msup 'msub)
               ,(render-math-child (car children))
               ,(render-math-child (cadr children))))
            ((math-vector)
             `(mover (@ (accent "true"))
                    ,(render-math-row children)
                    (mo "→")))
            ((math-parens)
             `(mrow (mo "(") ,@(render-math-children children) (mo ")")))
            ((math-align)
             `(mtable
                ,@(map
                    (lambda (row)
                      `(mtr
                         ,@(map
                             (lambda (cell index)
                               `(mtd (@ (style
                                         ,(if (zero? index)
                                              (if (= (length
                                                       (content-element-children row))
                                                     1)
                                                  "text-align: left"
                                                  "text-align: right")
                                              "text-align: left")))
                                     ,(render-math-row
                                        (content-element-children cell))))
                             (content-element-children row)
                             (iota (length (content-element-children row))))))
                    children)))
            ((math-row math-cell) (render-math-row children))
            ((math-style)
             `(mstyle (@ (mathvariant ,(symbol->string (content-element-property child 'variant))))
                      ,@(render-math-children children)))
            (else (error "unsupported MathML IR element" kind))))))

  (define (render-math element)
    (let ((display? (content-element-property element 'display?)))
      (if (content-element-property element 'tex?)
          `(,(if display? 'div 'span)
             (@ (class ,(if display? "math-display" "math-inline")))
             ,(string-append (if display? "\\[" "\\(")
                             (content-verbatim-string
                               (content-element-children element))
                             (if display? "\\]" "\\)")))
          (let ((mathml
                  `(math (@ (xmlns "http://www.w3.org/1998/Math/MathML")
                            ,@(if display? '((display "block")) '()))
                       ,(render-math-row
                          (content-element-children element)))))
            (if display?
                `(div (@ (class "math-display")) ,mathml)
                `(span (@ (class "math-inline")) ,mathml))))))

  (render content))

(define (sxml->html sxml)
  (call-with-output-string (lambda (port) (sxml->xml sxml port))))
