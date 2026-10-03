;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester commands)
  #:replace (link)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (grove ir)
  #:use-module (grove string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13))

(define-syntax include-text
  (lambda (stx)
    (syntax-case stx ()
      ((_ filename)
       (let* ((path (syntax->datum #'filename))
              (source (search-path %load-path path)))
         (unless source
           (syntax-violation 'include-text "text file not found in load path"
                             stx #'filename))
         (datum->syntax stx
           `(quote ,(call-with-include-port
                      (datum->syntax #'filename source)
                      get-string-all))))))))

(define lipsum-source
  (search-path %load-path "forester/data/lipsum-la.txt"))

(define-public (title . body)
  (make-content-element 'title body #:flow-kind 'block))

(define-public (summary . body)
  (make-content-element 'summary body #:flow-kind 'block))

(define lipsum-paragraphs
  (let ((port (open-input-string
                (include-text "forester/data/lipsum-la.txt"))))
    (let loop ((line (read-line port))
               (current-lines '())
               (paragraphs '()))
      (cond
        ((eof-object? line)
         (reverse
           (if (null? current-lines)
               paragraphs
               (cons (string-join (reverse current-lines) " ")
                     paragraphs))))
        ((string-null? (string-trim-both line))
         (loop (read-line port)
               '()
               (if (null? current-lines)
                   paragraphs
                   (cons (string-join (reverse current-lines) " ")
                         paragraphs))))
        (else
          (loop (read-line port)
                (cons (string-trim-both line) current-lines)
                paragraphs))))))

(define (lipsum-selector->range selector)
  (let* ((text (cond
                 ((string? selector) selector)
                 ((symbol? selector) (symbol->string selector))
                 ((number? selector) (number->string selector))
                 (else #f)))
         (separator (and text (string-index text #\-)))
         (first (and text
                     (string->number
                       (if separator
                           (substring text 0 separator)
                           text))))
         (last (and text
                    (string->number
                      (if separator
                          (substring text (+ separator 1))
                          text)))))
    (unless (and first last
                 (integer? first) (integer? last)
                 (exact? first) (exact? last)
                 (> first 0) (>= last first)
                 (<= last (length lipsum-paragraphs)))
      (error "Invalid Lipsum paragraph range"
             selector
             (length lipsum-paragraphs)))
    (cons first last)))

(define-public (lipsum . options)
  (let* ((range (if (null? options)
                    (cons 1 (length lipsum-paragraphs))
                    (if (= (length options) 1)
                        (lipsum-selector->range (car options))
                        (error "Lipsum accepts one paragraph range" options))))
         (selected
           (take (drop lipsum-paragraphs (- (car range) 1))
                 (+ 1 (- (cdr range) (car range)))))
         (text (string-join selected "\n\n")))
    (make-content-text text lipsum-source)))

(define-public (published date)
  (make-content-element 'published (list date) #:flow-kind 'block))

(define-public (updated date)
  (make-content-element 'updated (list date) #:flow-kind 'block))

(define-public (forest-query query)
  "Record a query to run against the complete Forest after source analysis."
  (unless (procedure? query)
    (error "Forester query must be a procedure" query))
  (make-content-element 'forest-query '()
                        #:properties `((query . ,query))
                        #:flow-kind 'block))

(define-public link
  (lambda* (target . label)
    (make-content-element 'link
                          (if (null? label) (list target) label)
                          #:properties `((target . ,target)))))

(define-public image
  (lambda* (source . alternative-text)
    (unless (non-empty-string? source)
      (error "Forester image source must be a non-empty string" source))
    (make-content-element 'image alternative-text
                          #:properties `((source . ,source)))))

(define (valid-topic-id? id)
  (and (non-empty-string? id)
       (let ((first (string-ref id 0))
             (last (string-ref id (- (string-length id) 1))))
         (and (or (char-alphabetic? first) (char-numeric? first))
              (or (char-alphabetic? last) (char-numeric? last))))
       (every (lambda (character)
                (or (char-alphabetic? character)
                    (char-numeric? character)
                    (char=? character #\-)))
              (string->list id))))

(define* (topic-element kind id label)
  (unless (valid-topic-id? id)
    (error "Forester topic ID must contain only letters, digits, and hyphens, and begin/end with a letter or digit"
           id))
  (make-content-element kind label #:properties `((id . ,id))
                        #:flow-kind 'block))

(define-public tag
  (lambda* (id . label)
    (topic-element 'tag id (if (null? label) (list id) label))))

(define-public taxon
  (lambda* (id . label)
    (topic-element 'taxon id (if (null? label) (list id) label))))

(define subtree-id-unspecified (list 'subtree-id-unspecified))

(define (subtree-body arguments)
  (let loop ((arguments arguments) (body '()))
    (cond
      ((null? arguments) (reverse body))
      ((keyword? (car arguments))
       (unless (pair? (cdr arguments))
         (error "Forester subtree keyword requires a value"
                (car arguments)))
       (loop (cddr arguments) body))
      (else (loop (cdr arguments) (cons (car arguments) body))))))

(define-public subtree
  (lambda* (#:key (id subtree-id-unspecified) (open? #t) . arguments)
    (let* ((named? (not (eq? id subtree-id-unspecified)))
           ;; Guile's rest argument contains keyword arguments as well.
           (body (subtree-body arguments)))
      (when (and named?
                 (not (and (non-empty-string? id)
                           (not (string-index id #\#)))))
        (error "Forester subtree ID must be a non-empty string without #" id))
      (unless (boolean? open?)
        (error "Forester subtree #:open? must be a boolean" open?))
      (let ((titles (filter (lambda (element)
                              (and (content-element? element)
                                   (eq? (content-element-kind element) 'title)))
                            body)))
        (unless (= (length titles) 1)
          (error "Forester subtree requires exactly one direct @title"
                 (length titles)))
        (let* ((title-element (car titles))
               (title-content (content-element-children title-element))
               (title-text (string-trim-both
                             (content-verbatim-string title-content))))
          (when (string-null? title-text)
            (error "Forester subtree title cannot be empty"))
          (make-content-element
            'subtree
            (remove (lambda (element) (eq? element title-element)) body)
            #:properties
            (append `((title . ,title-text) (open? . ,open?))
                    (if named? `((tree-id . ,id)) '()))
            #:flow-kind 'block))))))

(define-public (transclude id)
  (unless (non-empty-string? id)
    (error "Forester transclusion target must be a non-empty tree ID" id))
  (make-content-element 'transclude '() #:properties `((target . ,id))
                        #:flow-kind 'block))
