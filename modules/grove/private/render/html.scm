;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private render html)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove render rules)
  #:use-module (grove highlight)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (content->sxml/internal
            style-attribute-assoc
            merge-html-attributes))

(define (style-attribute-assoc style id-prefix)
  "The style's HTML attributes as an assoc list: the style name's class
first, then each attribute set, with ids and local hrefs scoped by
ID-PREFIX."
  (if (not style) '()
      (append
        (let ((name (content-style-name style)))
          (if name
              `((class . ,(if (symbol? name) (symbol->string name) name)))
              '()))
        (append-map
          (lambda (property)
            (if (html-attributes? property)
                (map (lambda (attribute)
                       (let ((name (car attribute)) (value (cdr attribute)))
                         (cons name
                           (cond
                             ((eq? name 'id) (string-append id-prefix value))
                             ((and (eq? name 'href) (string-prefix? "#" value))
                              (string-append "#" id-prefix (substring value 1)))
                             (else value)))))
                     (html-attributes-assoc property))
                '()))
          (content-style-properties style)))))

(define (merge-html-attributes original additions)
  (fold
    (lambda (addition attributes)
      (let* ((name (car addition))
             (value (cdr addition))
             (existing (assq name attributes)))
        (cond
          ((not existing) (append attributes (list addition)))
          ((eq? name 'id)
           (error "HTML style cannot replace a renderer-owned ID" (cdr existing) value))
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
(define* (content->sxml/internal content #:key (internal-handlers '())
                                 (internal-lazy-handlers '()) (id-prefix "")
                                 (highlighters (default-highlighters))
                                 (rules empty-render-rules) (context #f))
  (unless (and (list? highlighters)
               (every (lambda (entry)
                        (and (pair? entry) (symbol? (car entry))
                             (highlighter? (cdr entry)))) highlighters))
    (error "HTML highlighters must be language/highlighter pairs" highlighters))
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

  (define (style-html-tag style)
    (and style
      (let loop ((properties (content-style-properties style)) (tag #f))
        (if (null? properties) tag
            (loop (cdr properties)
              (if (html-tag? (car properties))
                  (string->symbol (html-tag-name (car properties)))
                  tag))))))

  (define (style-html-void? style)
    (and style
      (any html-void? (content-style-properties style))))

  (define (apply-element-style element rendered)
    (let* ((style (content-element-style element))
           (attributes (style-attribute-assoc style id-prefix))
           (tag (style-html-tag style))
           (void? (style-html-void? style)))
      (cond
        ((and (null? attributes) (not tag) (not void?)) rendered)
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
                (merged (merge-html-attributes original-attributes attributes))
                (tag-sxml `(,(or tag (car rendered))
                            ,@(if (null? merged) '()
                                  (list (cons '@ (map (lambda (attribute)
                                                        (list (car attribute)
                                                              (cdr attribute)))
                                                      merged)))))))
           (if void?
               (begin
                 (unless (null? children)
                   (error "void HTML style takes no content" element))
                 (cons '*void* tag-sxml))
               (append tag-sxml children)))))))

  (define (render-code-block element)
    (let* ((language (content-element-property element 'language))
           (highlighter (and language (assq-ref highlighters language)))
           (source (content-verbatim-string (content-element-children element))))
      (when (and language (not highlighter))
        (format (current-error-port)
          "WARNING: no code highlighter configured for ~a; displaying plain text~%" language))
      `(pre (@ (translate "no"))
            (code
              (@ (translate "no")
                 ,@(if language `((data-language ,(symbol->string language))) '()))
              ,@(if highlighter
                    (map (lambda (piece)
                           (if (car piece)
                               `(span (@ (data-syntax ,(car piece)))
                                      ,(cdr piece))
                               (cdr piece)))
                         (highlight-code highlighter source))
                    (list source))))))

  (define (default-element-sxml element children)
    (case (content-element-kind element)
      ((bold) (cons 'strong children))
      ((paragraph) (cons 'p children))
      ((blockquote) (cons 'blockquote children))
      ((itemize) (cons 'ul children))
      ((item) (cons 'li children))
      ((code) `(code (@ (translate "no")) ,@children))
      ((code-block) (render-code-block element))
      ((verbatim) `(pre (@ (translate "no")) ,@children))
      ((details) (cons 'details children))
      ((summary) (cons 'summary children))
      ((disclosure subtree)
       `(details
          ,@(if (if (assq 'open? (content-element-properties element))
                    (content-element-property element 'open?) #t)
                '((@ (open "open"))) '())
          (summary ,(content-element-property element 'title))
          ,@children))
      ((section)
       `(section (@ (class "forest-section"))
          (h2 ,(content-element-property element 'title))
          ,@children))
      ((math-derivation math-diagnostics)
       `(div (@ (class ,(symbol->string (content-element-kind element))))
             ,@children))
      ((math-diagnostic)
       `(aside (@ (class "math-diagnostic")) ,@children))
      (else
       ;;     A style that replaces the tag owns the
       ;;     element's shape: emit the bare form and
       ;;     no invented class.  Styles that only add
       ;;     attributes keep the kind class.
       (if (and (content-element-style element)
                (style-html-tag
                  (content-element-style element)))
           (cons 'span children)
           `(span (@ (class ,(symbol->string
                               (content-element-kind element))))
                  ,@children)))))

  (define (render-element element)
    (let* ((kind (content-element-kind element))
           (lazy-handler (assq-ref internal-lazy-handlers kind)))
      (cond
        (lazy-handler
         ;;     Lazy handlers own their element's presentation
         ;;     entirely - tree blocks apply the style themselves - so
         ;;     the generic application runs only for the rest.
         (lazy-handler element render))
        ((eq? kind 'math)
         (apply-element-style element (render-math element)))
        (else
         (let* ((children (map render (content-element-children element)))
                (handler (assq-ref internal-handlers kind))
                (rule (and rules (render-rules-ref rules kind)))
                (rendered
                 (if rule
                     (rule context element children
                           (lambda ()
                             (if handler
                                 (handler element children)
                                 (default-element-sxml element children))))
                     (if handler
                         (handler element children)
                         (default-element-sxml element children)))))
           (apply-element-style element rendered))))))

  (define (render-math-children children)
    (map render-math-child children))

  (define (render-math-row children)
    `(mrow ,@(render-math-children children)))

  (define (render-math-child child)
    (apply-element-style child (render-math-child/raw child)))

  (define (render-math-child/raw child)
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

  (define (math-attributes element class)
    `(@ (class ,class)
        ,@(let ((id (content-element-property element 'id)))
            (if id `((id ,id)) '()))))

  (define (render-math element)
    (let* ((display? (content-element-property element 'display?))
           (class (if display? "math-display" "math-inline"))
           (attributes (math-attributes element class))
           (number (and display? (content-element-property element 'number))))
      (if (content-element-property element 'tex?)
          (let ((tex (content-verbatim-string
                       (content-element-children element))))
            `(,(if display? 'div 'span)
               ,attributes
               ,(string-append
                  (if display? "\\[" "\\(")
                  tex
                  (if number (string-append " \\tag{" number "}") "")
                  (if display? "\\]" "\\)"))))
          (let ((mathml
                  `(math (@ (xmlns "http://www.w3.org/1998/Math/MathML")
                            ,@(if display? '((display "block")) '()))
                       ,(render-math-row
                          (content-element-children element)))))
            (if number
                `(div ,attributes
                      (span (@ (class "math-number"))
                            "(" ,number ")")
                      ,mathml)
                `(,(if display? 'div 'span) ,attributes ,mathml))))))

  (render content))
