;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private render html)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove ir)
  #:use-module (grove private html-style)
  #:use-module (grove render html attributes)
  #:use-module (grove render rules)
  #:use-module (grove highlight)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (content->sxml/internal))
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
      ((content-text? content) (ir:content-text-value/raw content))
      ((content-layout? content) (ir:content-layout-raw/raw content))
      ((content-element? content) (render-element content))
      ((content-document? content)
       (cons 'article (map render (ir:content-document-children/raw content))))
      (else (error "not a content node" content))))

  (define (apply-element-style element rendered)
    (let* ((style (content-element-style element))
           (attributes (content-style-html-attributes style id-prefix))
           (tag-name (content-style-html-tag-name style))
           (tag (and tag-name (string->symbol tag-name)))
           (void? (content-style-html-void? style)))
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
    (let* ((language (ir:content-element-property/raw element 'language))
           (highlighter (and language (assq-ref highlighters language)))
           (source (content-verbatim-string (ir:content-element-children/raw element))))
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
          ,@(if (if (assq 'open? (ir:content-element-properties/raw element))
                    (ir:content-element-property/raw element 'open?) #t)
                '((@ (open "open"))) '())
          (summary ,(ir:content-element-property/raw element 'title))
          ,@children))
      ((section)
       `(section (@ (class "forest-section"))
          (h2 ,(ir:content-element-property/raw element 'title))
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
                (content-style-html-tag-name
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
         (let* ((default (render-math element))
                (rule (and rules (render-rules-ref rules kind)))
                (rendered
                 (if rule
                     (rule context element default (lambda () default))
                     default)))
           (apply-element-style element rendered)))
        (else
         (let* ((payload (map render (ir:content-element-children/raw element)))
                (handler (assq-ref internal-handlers kind))
                (rule (and rules (render-rules-ref rules kind)))
                (rendered
                 (if rule
                     (rule context element payload
                           (lambda ()
                             (if handler
                                 (handler element payload)
                                 (default-element-sxml element payload))))
                     (if handler
                         (handler element payload)
                         (default-element-sxml element payload)))))
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
              (children (ir:content-element-children/raw child)))
          (case kind
            ((math-token)
             (let ((tag (ir:content-element-property/raw child 'token-kind)))
               `(,tag
                 ,@(let ((rule (ir:content-element-property/raw child 'rule))
                         (path (ir:content-element-property/raw child 'path)))
                     (let ((attributes
                             (append (if rule `((data-rule ,rule) (data-path ,path)) '())
                                     (let ((location (ir:content-element-property/raw child 'location)))
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
                                                       (ir:content-element-children/raw row))
                                                     1)
                                                  "text-align: left"
                                                  "text-align: right")
                                              "text-align: left")))
                                     ,(render-math-row
                                        (ir:content-element-children/raw cell))))
                             (ir:content-element-children/raw row)
                             (iota (length (ir:content-element-children/raw row))))))
                    children)))
            ((math-row math-cell) (render-math-row children))
            ((math-style)
             (let ((variant (ir:content-element-property/raw child 'variant)))
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
        ,@(let ((id (ir:content-element-property/raw element 'id)))
            (if id `((id ,id)) '()))))

  (define (render-math element)
    (let* ((display? (ir:content-element-property/raw element 'display?))
           (class (if display? "math-display" "math-inline"))
           (attributes (math-attributes element class))
           (number (and display? (ir:content-element-property/raw element 'number))))
      (if (ir:content-element-property/raw element 'tex?)
          (let ((tex (content-verbatim-string
                       (ir:content-element-children/raw element))))
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
                          (ir:content-element-children/raw element)))))
            (if number
                `(div ,attributes
                      (span (@ (class "math-number"))
                            "(" ,number ")")
                      ,mathml)
                `(,(if display? 'div 'span) ,attributes ,mathml))))))

  (render content))
