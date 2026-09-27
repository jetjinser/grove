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
  #:export (make-forester-page-reader))

(define metadata-kinds '(title summary published updated))

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

(define (page-from-document path document)
  (let* ((filename (basename path))
         (children (content-document-children document))
         (metadata-nodes (filter metadata-element? children))
         (titles (elements-of-kind metadata-nodes 'title))
         (summary (one-metadata-value metadata-nodes 'summary path))
         (published (date-metadata metadata-nodes 'published path))
         (updated (date-metadata metadata-nodes 'updated path)))
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
        (string-drop-right filename (string-length ".grv"))
        title
        (make-content-document
          (remove metadata-element? children)
          (content-document-source document))
        path
        #:metadata (filter cdr
                     (list (cons 'summary summary)
                           (cons 'published published)
                           (cons 'updated updated)))))))

(define (make-forester-page-reader read-document)
  (lambda (path)
    (page-from-document path (read-document path))))
