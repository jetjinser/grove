;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester commands)
  #:replace (link)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26) ; cut(e)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 optargs)
  #:use-module (grove ir)
  #:use-module (language scribble flow)
  #:use-module (grove string-fun))

(define-public (title . body)
  (make-content-element 'title body #:flow-kind 'block))

(define-public (summary . body)
  (make-content-element 'summary body #:flow-kind 'block))


(define lipsum-source
  (search-path %load-path "forester/data/lipsum-la.txt"))

(define +lipsum-paragraphs+
  (let* ([text       (include-text "forester/data/lipsum-la.txt")]
         [normalized (string-replace-substring text "\n\n" "\x1e")]
         [raw-paras  (string-split normalized #\x1e)])
    (map (lambda (p) (string-join (string-tokenize p) " "))
         raw-paras)))

(define (lipsum-selector->range selector)
  (define text
    (cond [(string? selector) selector]
          [(symbol? selector) (symbol->string selector)]
          [(number? selector) (number->string selector)]
          [else ""]))

  (define range-pair
    (match (map string->number (string-split text #\-))
      [((? integer? a))                (cons a a)]
      [((? integer? a) (? integer? b)) (cons a b)]
      [_                               (error "Invalid Lipsum selector format"
                                              selector)]))

  (match-let ([(a . b) range-pair]
              [total   (length +lipsum-paragraphs+)])
    (unless (and (> a 0) (>= b a) (<= b total))
      (error "Lipsum paragraph range out of bounds" selector total)))

  range-pair)

(define-public (lipsum . options)
  (define range
    (match options
      [()  (cons 1 (length +lipsum-paragraphs+))]
      [(s) (lipsum-selector->range s)]
      [_   (error "Lipsum accepts at most one paragraph range selector" options)]))

  (match-let* ([(a . b)  range]
               [count    (+ (- b a) 1)]
               [selected (take (drop +lipsum-paragraphs+ (- a 1)) count)]
               [text     (string-join selected "\n\n")])
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

(define-public (link target . labels)
  (unless (and (non-empty-string? target) (not (string-index target #\#)))
    (error "Forester link requires a tree ID without #" target))
  (make-content-element 'link
                        (if (null? labels) (list target) labels)
                        #:properties `((target . ,target))))

(define-public (image source . alternative-text)
  (unless (non-empty-string? source)
    (error "Forester image source must be a non-empty string" source))
  (make-content-element 'image alternative-text
                        #:properties `((source . ,source))))

(define char-set:topic-body (char-set-adjoin char-set:letter+digit #\-))
(define (valid-topic-id? id)
  (and (non-empty-string? id)
       (char-set-contains? char-set:letter+digit (string-ref id 0))
       (char-set-contains? char-set:letter+digit (string-ref id (- (string-length id) 1)))
       (string-every char-set:topic-body id)))

(define (topic-element kind id label)
  (unless (valid-topic-id? id)
    (error "Forester topic ID must contain only letters, digits, and hyphens, and begin/end with a letter or digit"
           id))
  (make-content-element kind label #:properties `((id . ,id))
                        #:flow-kind 'block))

(define-public (tag id . labels)
  (topic-element 'tag id (if (null? labels) (list id) labels)))

(define-public (taxon id . labels)
  (topic-element 'taxon id (if (null? labels) (list id) labels)))

(define (subtree-body arguments)
  (let loop ([args arguments]
             [body '()])
    (match args
      [() (reverse body)]
      [((? keyword? k) val . rest)
       (loop rest body)]
      [((? keyword? k))
       (error "Forester subtree keyword requires a value" k)]
      [(x . rest)
       (loop rest (cons x body))])))

(define*-public (subtree #:key [id #f] [open? #t] . arguments)
  (unless (boolean? open?)
    (error "Forester subtree #:open? must be a boolean" open?))
  ;;     Guile's rest argument contains keyword arguments as well.
  (let* ([body   (subtree-body arguments)]
         [titles (filter (lambda (element)
                            (and (content-element? element)
                                 (eq? (content-element-kind element) 'title)))
                         body)])
    (when (and id
               (or (not (non-empty-string? id))
                   (string-index id #\#)))
      (error "Forester subtree ID must be a non-empty string without #" id))
    (match titles
      [(title-element)
       (define title-text
         (string-trim-both
           (content-verbatim-string
             (content-element-children title-element))))
       (when (string-null? title-text)
         (error "Forester subtree title cannot be empty"))
       (make-content-element
         'subtree (decode-flow (remove (cut eq? <> title-element) body))
         #:properties
         `((title . ,title-text)
           (open? . ,open?)
           ,@(if id `((tree-id . ,id)) '()))
         #:flow-kind 'block)]
      [_ (error "Forester subtree requires exactly one direct @title"
                (length titles))])))

(define-public (transclude id)
  (unless (non-empty-string? id)
    (error "Forester transclusion target must be a non-empty tree ID" id))
  (make-content-element 'transclude '() #:properties `((target . ,id))
                        #:flow-kind 'block))
