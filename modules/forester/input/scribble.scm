;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester input scribble)
  #:use-module (ice-9 ftw)
  #:use-module (forester private date)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-page-reader))

(define metadata-kinds '(title summary published updated tag taxon))

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

(define (topic-metadata elements kind path)
  (let ((topics
          (map (lambda (element)
                 (let* ((id (content-element-property element 'id))
                        (label (string-trim-both
                                 (content-verbatim-string
                                   (content-element-children element)))))
                   (unless (and (string? id) (not (string-null? id))
                                (not (string-null? label)))
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

(define (page-from-document path document)
  (let* ((filename (basename path))
         (children (content-document-children document))
         (metadata-nodes (filter metadata-element? children))
         (titles (elements-of-kind metadata-nodes 'title))
         (summary (one-metadata-value metadata-nodes 'summary path))
         (published (date-metadata metadata-nodes 'published path))
         (updated (date-metadata metadata-nodes 'updated path))
         (tags (topic-metadata metadata-nodes 'tag path))
         (taxon (taxon-metadata metadata-nodes path)))
    (unless (string-suffix? ".grv" filename)
      (error (format #f "Forester page source must end in .grv: ~a" path)))
    (unless (= (length titles) 1)
      (error (format #f
                     "Forester page ~a requires exactly one top-level @title (found ~a)"
                     path (length titles))))
    (let ((title (string-trim-both
                   (content-verbatim-string
                     (content-element-children (car titles))))))
      (when (string-null? title)
        (error (format #f "Forester page title is empty in ~a" path)))
      (make-site-page
        ;; Forester addresses trees by filename; directories only organize
        ;; the source tree and must not leak into page IDs or links.
        (string-drop-right filename (string-length ".grv"))
        title
        (make-content-document
          (remove (lambda (child)
                    (and (content-element? child)
                         (memq (content-element-kind child)
                               '(title summary published updated))))
                  children)
          (content-document-source document))
        path
        #:metadata (filter cdr
                     (list (cons 'summary summary)
                           (cons 'published published)
                           (cons 'updated updated)))))))

(define (make-page-reader read-document)
  "Return a reader whose page IDs are source basenames without `.grv'."
  (lambda (path)
    (page-from-document path (read-document path))))
