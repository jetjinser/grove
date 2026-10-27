;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester commands)
  #:use-module ((grove private ir) #:prefix ir:)
  #:replace (link)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26) ; cut(e)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 optargs)
  #:use-module (grove ir)
  #:use-module (language scribble command)
  #:use-module (language scribble flow)
  #:use-module (grove private string-fun)
  #:use-module (forester private topic)
  #:use-module (forester i18n))

(define-public (title . body)
  (make-content-element 'title body #:flow-kind 'block))

(define-public (summary . body)
  (make-content-element 'summary body #:flow-kind 'block))

(define-public (og-image . source)
  (make-content-element 'og-image source #:flow-kind 'block))

(define-public (og-image-alt . body)
  (make-content-element 'og-image-alt body #:flow-kind 'block))


(define-public (published date)
  (make-content-element 'published (list date) #:flow-kind 'block))

(define-public (updated date)
  (make-content-element 'updated (list date) #:flow-kind 'block))

(define-public (locale tag)
  "Declare the page's language for the document and its translations."
  (make-content-element 'locale (list tag) #:flow-kind 'block))

(define-public (translation key)
  "Group pages that translate one another under KEY."
  (make-content-element 'translation (list key) #:flow-kind 'block))

(define-public (describes uri)
  "Declare an absolute external URI for the entity this tree describes."
  (make-content-element 'describes (list uri) #:flow-kind 'block))

(define-public (forest-query query)
  "Record a query to run against the complete Forest after source analysis."
  (unless (procedure? query)
    (error "Forester query must be a procedure" query))
  (make-content-element 'forest-query '()
                        #:properties `((query . ,query))
                        #:flow-kind 'block))

(define (split-link-target target)
  "Split TARGET into (values page-id fragment-or-#f) at the first #.

The page is validated to exist later; the fragment is the id of a node on that
page and must be a valid anchor ID."
  (let ((hash (string-index target #\#)))
    (if (not hash)
        (values target #f)
        (let ((page (substring target 0 hash))
              (fragment (substring target (1+ hash))))
          (unless (non-empty-string? page)
            (error "Forester link requires a tree ID before #" target))
          (values page (checked-anchor-id fragment))))))

(define*-public (link target #:key (style #f) . labels)
  (set! labels (command-body labels))
  (unless (non-empty-string? target)
    (error "Forester link requires a tree ID" target))
  (call-with-values (lambda () (split-link-target target))
    (lambda (page fragment)
      (make-content-element 'link
                            (if (null? labels) (list page) labels)
                            #:style style
                            #:properties (if fragment
                                             `((target . ,page)
                                               (fragment . ,fragment))
                                             `((target . ,page)))))))

(define*-public (image source #:key (style #f) . alternative-text)
  (set! alternative-text (command-body alternative-text))
  (unless (non-empty-string? source)
    (error "Forester image source must be a non-empty string" source))
  (make-content-element 'image alternative-text
                        #:style style
                        #:properties `((source . ,source))))

(define (topic-element kind value)
  (unless (valid-topic-id? value)
    (error "Forester topic must contain only letters, digits, and hyphens, and begin/end with a letter or digit"
           value))
  (let ((value (string-downcase value)))
    (make-content-element kind (list value) #:properties `((id . ,value))
                          #:flow-kind 'block)))

(define-public (tag value)
  (topic-element 'tag value))

(define-public (taxon value)
  (topic-element 'taxon value))

(define*-public (subtree #:key [id #f] [anchor #f] [open? #t] [style #f] . arguments)
  (unless (boolean? open?)
    (error "Forester subtree #:open? must be a boolean" open?))
  ;;     Guile's rest argument contains keyword arguments as well.
  (let* ([body   (command-body arguments)]
         [titles (filter (lambda (element)
                            (and (content-element? element)
                                 (eq? (content-element-kind element) 'title)))
                         body)])
    (when (and id
               (or (not (non-empty-string? id))
                   (string-index id #\#)))
      (error "Forester subtree ID must be a non-empty string without #" id))
    (when anchor
      (checked-anchor-id anchor))
    (match titles
      [(title-element)
       (define title-text
         (string-trim-both
           (content-verbatim-string
             (ir:content-element-children/raw title-element))))
       (when (string-null? title-text)
         (error "Forester subtree title cannot be empty"))
       (make-content-element
         'subtree (decode-flow (remove (cut eq? <> title-element) body))
         #:style style
         #:properties
         `((title . ,title-text)
           (open? . ,open?)
           ,@(if id `((tree-id . ,id)) '())
           ,@(cond (anchor `((anchor . ,anchor)))
                   (id `((anchor . ,id)))
                   (else '())))
         #:flow-kind 'block)]
      [_ (error "Forester subtree requires exactly one direct @title"
                (length titles))])))


(define*-public (section #:key (style #f) . arguments)
  "Group content structurally without an addressable tree ID.

A section renders as a nested heading and body but gets no route and no
anchor; use @subtree with #:id when the group must be linked."
  (let* ((body (command-body arguments))
         (titles (filter (lambda (element)
                           (and (content-element? element)
                                (eq? (content-element-kind element) 'title)))
                         body)))
    (match titles
      [(title-element)
       (let ((title-text
               (string-trim-both
                 (content-verbatim-string
                   (ir:content-element-children/raw title-element)))))
         (when (string-null? title-text)
           (error "Forester section title cannot be empty"))
         (make-content-element
           'section (decode-flow (remove (cut eq? <> title-element) body))
           #:style style
           #:properties `((title . ,title-text))
           #:flow-kind 'block))]
      [_ (error "Forester section requires exactly one direct @title"
                (length titles))])))
(define*-public (transclude id #:key (style #f) (anchor #f))
  (unless (non-empty-string? id)
    (error "Forester transclusion target must be a non-empty tree ID" id))
  (when anchor
    (checked-anchor-id anchor))
  (make-content-element 'transclude '() #:style style
                        #:properties `((target . ,id)
                                       ,@(if anchor `((anchor . ,anchor)) '()))
                        #:flow-kind 'block))

(define*-public (bi . body)
  "Folded bilingual block using the project's reader locale."
  (apply bilingual body))
