;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:export (content->sxml
            sxml->html))

(define* (content->sxml content #:key (handlers '()) (lazy-handlers '()))
  (define math-variant (make-parameter #f))
  (define (render content)
    (cond
      ((string? content) content)
      ((content-text? content) (content-text-value content))
      ((content-layout? content) (content-layout-raw content))
      ((content-element? content) (render-element content))
      ((content-document? content)
       (cons 'article (map render (content-document-children content))))
      (else (error "not a content node" content))))

  (define (style-html-attributes style)
    (if (not style) '()
        (append
          (let ((name (content-style-name style)))
            (if name
                `((class . ,(if (symbol? name) (symbol->string name) name)))
                '()))
          (append-map
            (lambda (property)
              (if (html-attributes? property)
                  (html-attributes-assoc property)
                  '()))
            (content-style-properties style)))))

  (define (style-html-tag style)
    (and style
      (let loop ((properties (content-style-properties style)) (tag #f))
        (if (null? properties) tag
            (loop (cdr properties)
              (if (html-tag? (car properties))
                  (string->symbol (html-tag-name (car properties)))
                  tag))))))

  (define (merge-html-attributes original additions)
    (fold
      (lambda (addition attributes)
        (let* ((name (car addition))
               (value (cdr addition))
               (existing (assq name attributes)))
          (cond
            ((not existing) (append attributes (list addition)))
            ((eq? name 'class)
             (map (lambda (attribute)
                    (if (eq? (car attribute) name)
                        (cons name (string-append (cdr attribute) " " value))
                        attribute)) attributes))
            ((eq? name 'style)
             (map (lambda (attribute)
                    (if (eq? (car attribute) name)
                        (cons name (string-append (cdr attribute) "; " value))
                        attribute)) attributes))
            (else
             (map (lambda (attribute)
                    (if (eq? (car attribute) name) addition attribute))
                  attributes)))))
      original additions))

  (define (apply-element-style element rendered)
    (let* ((style (content-element-style element))
           (attributes (style-html-attributes style))
           (tag (style-html-tag style)))
      (cond
        ((and (null? attributes) (not tag)) rendered)
        ((not (and (pair? rendered) (symbol? (car rendered))))
         (error "HTML style requires a single root tag" element))
        (else
         (let* ((tail (cdr rendered))
                (has-attributes?
                  (and (pair? tail) (pair? (car tail)) (eq? (caar tail) '@)))
                (original-attributes
                  (if has-attributes?
                      (map (lambda (attribute)
                             (cons (car attribute) (cadr attribute)))
                           (cdar tail))
                      '()))
                (children (if has-attributes? (cdr tail) tail))
                (merged (merge-html-attributes original-attributes attributes)))
           `(,(or tag (car rendered))
             ,@(if (null? merged) '()
                   (list (cons '@ (map (lambda (attribute)
                                         (list (car attribute) (cdr attribute)))
                                       merged))))
             ,@children))))))

  (define (render-element element)
    (let* ((kind (content-element-kind element))
           (lazy-handler (assq-ref lazy-handlers kind))
           (rendered
             (cond
               (lazy-handler (lazy-handler element render))
               ((eq? kind 'math) (render-math element))
               (else
                (let ((children (map render (content-element-children element)))
                      (handler (assq-ref handlers kind)))
                  (if handler
                      (handler element children)
                      (case kind
                        ((bold) (cons 'strong children))
                        ((paragraph) (cons 'p children))
                        ((blockquote) (cons 'blockquote children))
                        ((itemize) (cons 'ul children))
                        ((item) (cons 'li children))
                        ((code) (cons 'code children))
                        ((code-block) `(pre (code ,@children)))
                        ((verbatim) (cons 'pre children))
                        ((details) (cons 'details children))
                        ((summary) (cons 'summary children))
                        ((disclosure subtree)
                         `(details
                            ,@(if (if (assq 'open? (content-element-properties element))
                                      (content-element-property element 'open?) #t)
                                  '((@ (open "open"))) '())
                            (summary ,(content-element-property element 'title))
                            ,@children))
                        ((math-derivation math-diagnostics)
                         `(div (@ (class ,(symbol->string kind))) ,@children))
                        ((math-diagnostic)
                         `(aside (@ (class "math-diagnostic")) ,@children))
                        (else `(span (@ (class ,(symbol->string kind))) ,@children)))))))))
      (apply-element-style element rendered)))

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
                     (let ((attributes
                             (append (if rule `((data-rule ,rule) (data-path ,path)) '())
                                     (let ((location (content-element-property child 'location)))
                                       (if location `((data-source ,location) (title ,location)) '()))
                                     (if (and (math-variant) (eq? tag 'mi))
                                         '((mathvariant "normal")) '()))))
                       (if (null? attributes) '() (list (cons '@ attributes)))))
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
            ((math-column)
             `(mrow (mo (@ (stretchy "true")) "(")
                    (mtable ,@(map (lambda (entry) `(mtr (mtd ,(render-math-child entry)))) children))
                    (mo (@ (stretchy "true")) ")")))
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
             (let ((variant (content-element-property child 'variant)))
               (parameterize ((math-variant variant))
                 `(mstyle (@ (style ,(case variant
                                       ((normal) "font-weight: normal; font-style: normal")
                                       ((bold) "font-weight: bold; font-style: normal")
                                       ((italic) "font-weight: normal; font-style: italic")
                                       ((bold-italic) "font-weight: bold; font-style: italic")
                                       (else (error "unsupported math variant" variant)))))
                          ,@(render-math-children children)))))
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
