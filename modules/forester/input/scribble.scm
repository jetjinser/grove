;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester input scribble)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (forester private entity-uri)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove private procedure)
  #:use-module (grove private string-fun)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (make-page-reader
            document->tree-page))

(define metadata-kinds
  '(title summary og-image og-image-alt published updated tag taxon locale
    translation describes))

(define (metadata-element? child)
  (and (content-element? child)
       (memq (content-element-kind child) metadata-kinds)))

(define (elements-of-kind elements kind)
  (filter (lambda (element)
            (eq? (content-element-kind element) kind))
          elements))

(define* (one-metadata-value elements kind path #:optional required?)
  (let ((matches (elements-of-kind elements kind)))
    (when (> (length matches) 1)
      (error (format #f "Forester page ~a has duplicate metadata ~a"
                     path kind)))
    (if (null? matches)
        (if required?
            (error (format #f "Forester page ~a requires metadata ~a"
                           path kind))
            #f)
        (let ((value
                (string-trim-both
                  (content-verbatim-string
                    (ir:content-element-children/raw (car matches))))))
          (when (string-null? value)
            (error (format #f "Forester page metadata ~a cannot be empty in ~a"
                           kind path)))
          value))))

(define (date-metadata elements kind path)
  (let ((value (one-metadata-value elements kind path)))
    (when (and value (not (iso-date? value)))
      (error (format #f "Forester page date ~a in ~a must use YYYY-MM-DD: ~a"
                     kind path value)))
    value))

(define (locale-metadata elements kind path)
  (let ((value (one-metadata-value elements kind path)))
    (and value
         (catch #t
           (lambda () (locale-tag (make-locale value)))
           (lambda (key . args)
             (error "Forester page locale must be a BCP 47 tag" path value))))))

(define (topic-metadata elements kind path)
  (let ((topics
          (map (lambda (element)
                 (let* ((id (ir:content-element-property/raw element 'id))
                        (label (string-trim-both
                                 (content-verbatim-string
                                   (ir:content-element-children/raw element)))))
                   (unless (and (non-empty-string? id)
                                (non-empty-string? label))
                     (error (format #f "Forester ~a requires an ID and label in ~a"
                                    kind path)))
                   (cons id label)))
               (elements-of-kind elements kind))))
    (let ((duplicate (find (lambda (topic)
                             (> (count (lambda (candidate)
                                         (string=? (car topic) (car candidate)))
                                       topics)
                                1))
                           topics)))
      (when duplicate
        (error (format #f "Forester page ~a repeats ~a ID ~a"
                       path kind (car duplicate))))
      topics)))

(define (taxon-metadata elements path)
  (let ((taxa (topic-metadata elements 'taxon path)))
    (when (> (length taxa) 1)
      (error (format #f "Forester page ~a has more than one taxon" path)))
    (and (pair? taxa) (car taxa))))

(define (describes-metadata elements path)
  (let ((uris (map (lambda (element)
                     (let ((value
                             (string-trim-both
                               (content-verbatim-string
                                 (ir:content-element-children/raw element)))))
                       (unless (non-empty-string? value)
                         (error (format #f
                                  "Forester page describes URI cannot be empty in ~a"
                                  path)))
                       (unless (absolute-entity-uri? value)
                         (error (format #f
                                  "Forester page describes value must be an ASCII absolute URI without credentials (HTTP(S) requires a host) in ~a: ~a"
                                  path value)))
                       value))
                   (elements-of-kind elements 'describes))))
    (unless (valid-entity-uri-list? uris)
      (error (format #f "Forester page ~a repeats a describes URI" path)))
    uris))

(define* (document->tree-page id document source #:key title (metadata '()))
  "Extract a tree's direct frontmatter, retaining topic nodes as queryable IR.

TITLE, when supplied, is the resulting page title. When omitted, the document
must contain one @title. Any @title elements in the document are checked for
uniqueness and non-empty content even when TITLE overrides their value.
METADATA adds page metadata; it cannot duplicate keys extracted from the
document."
  (unless (content-document? document)
    (error "Forester tree page requires a content document" document))
  (let* ((children (ir:content-document-children/raw document))
         (metadata-nodes (filter metadata-element? children))
         (declared-title
          (one-metadata-value metadata-nodes 'title source (not title)))
         (tree-title (or title declared-title))
         (summary (one-metadata-value metadata-nodes 'summary source))
         (og-image (one-metadata-value metadata-nodes 'og-image source))
         (og-image-alt (one-metadata-value metadata-nodes 'og-image-alt source))
         (published (date-metadata metadata-nodes 'published source))
         (updated (date-metadata metadata-nodes 'updated source))
         (locale (locale-metadata metadata-nodes 'locale source))
         (translation (one-metadata-value metadata-nodes 'translation source))
         (describes (describes-metadata metadata-nodes source)))
    (when (and published updated (string<? updated published))
      (error (format #f
               "Forester page updated date ~a precedes published date ~a in ~a"
               updated published source)))
    ;; Topics stay in the body, but validation is identical for every tree.
    (topic-metadata metadata-nodes 'tag source)
    (taxon-metadata metadata-nodes source)
    (make-site-page
      id tree-title
      (make-content-document
        (remove (lambda (child)
                  (and (content-element? child)
                       (memq (content-element-kind child)
                             '(title summary og-image og-image-alt published updated
                               locale translation describes))))
                children)
        #:source (ir:content-document-source/raw document))
      #:source source
      #:metadata (append metadata
                         (filter cdr
                           (list (cons 'summary summary)
                                 (cons 'og-image og-image)
                                 (cons 'og-image-alt og-image-alt)
                                 (cons 'published published)
                                 (cons 'updated updated)
                                 (cons 'locale locale)
                                 (cons 'translation translation)
                                 (cons 'describes (and (pair? describes) describes))))))))

(define (page-from-document path document)
  (let ((filename (basename path)))
    (unless (string-suffix? ".plant" filename)
      (error (format #f "Forester page source must end in .plant: ~a" path)))
    ;; Directories organize source files; tree IDs are their basenames.
    (document->tree-page
      (string-drop-right filename (string-length ".plant")) document path)))

(define (make-page-reader read-document)
  "Adapt a one-path document reader to a Forester page reader.

READ-DOCUMENT must accept one path and return a Content IR document. Page IDs
are source basenames without `.plant'."
  (unless (procedure-accepts-arguments? read-document 1)
    (error "Forester document reader must accept one source path"
           read-document))
  (lambda (path)
    (let ((document (read-document path)))
      (unless (content-document? document)
        (error "Forester document reader must return a content document"
               path document))
      (page-from-document path document))))
