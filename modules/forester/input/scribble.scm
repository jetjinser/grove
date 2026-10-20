;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester input scribble)
  #:use-module (ice-9 ftw)
  #:use-module (forester private date)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove string-fun)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-page-reader
            document->tree-page))

(define metadata-kinds
  '(title summary published updated tag taxon locale translation))

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
                    (content-element-children (car matches))))))
          (when (string-null? value)
            (error (format #f "Forester page metadata ~a cannot be empty in ~a"
                           kind path)))
          value))))

(define (date-metadata elements kind path)
  (let ((value (one-metadata-value elements kind path)))
    (when (and value (not (forester-date? value)))
      (error (format #f "Forester page date ~a in ~a must use YYYY-MM-DD: ~a"
                     kind path value)))
    value))

(define (locale-metadata elements kind path)
  (let ((value (one-metadata-value elements kind path)))
    (when value
      (catch #t
        (lambda () (make-locale value))
        (lambda (key . args)
          (error "Forester page locale must be a BCP 47 tag" path value))))
    value))

(define (topic-metadata elements kind path)
  (let ((topics
          (map (lambda (element)
                 (let* ((id (content-element-property element 'id))
                        (label (string-trim-both
                                 (content-verbatim-string
                                   (content-element-children element)))))
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

(define* (document->tree-page id document source #:key title (metadata '()))
  "Extract a tree's direct frontmatter, retaining topic nodes as queryable IR."
  (let* ((children (content-document-children document))
         (metadata-nodes (filter metadata-element? children))
         (tree-title (or title (one-metadata-value metadata-nodes 'title source #t)))
         (summary (one-metadata-value metadata-nodes 'summary source))
         (published (date-metadata metadata-nodes 'published source))
         (updated (date-metadata metadata-nodes 'updated source))
         (locale (locale-metadata metadata-nodes 'locale source))
         (translation (one-metadata-value metadata-nodes 'translation source)))
    ;; Topics stay in the body, but validation is identical for every tree.
    (topic-metadata metadata-nodes 'tag source)
    (taxon-metadata metadata-nodes source)
    (make-site-page
      id tree-title
      (make-content-document
        (remove (lambda (child)
                  (and (content-element? child)
                       (memq (content-element-kind child)
                             '(title summary published updated locale translation))))
                children)
        (content-document-source document))
      source
      #:metadata (append metadata
                         (filter cdr
                           (list (cons 'summary summary)
                                 (cons 'published published)
                                 (cons 'updated updated)
                                 (cons 'locale locale)
                                 (cons 'translation translation)))))))

(define (page-from-document path document)
  (let ((filename (basename path)))
    (unless (string-suffix? ".plant" filename)
      (error (format #f "Forester page source must end in .plant: ~a" path)))
    ;; Directories organize source files; tree IDs are their basenames.
    (document->tree-page
      (string-drop-right filename (string-length ".plant")) document path)))

(define (make-page-reader read-document)
  "Return a reader whose page IDs are source basenames without `.plant'."
  (lambda (path)
    (page-from-document path (read-document path))))
