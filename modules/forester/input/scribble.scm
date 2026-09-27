;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester input scribble)
  #:use-module (ice-9 ftw)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (srfi srfi-19)
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
      (error "Forester page has duplicate metadata" path kind))
    (if (null? matches)
        (if required? (error "Forester page requires metadata" path kind) #f)
        (let ((value
                (string-trim-both
                  (content-verbatim-string
                    (content-element-children (car matches))))))
          (when (string-null? value)
            (error "Forester page metadata cannot be empty" path kind))
          value))))

(define (iso-date? value)
  (and (= (string-length value) 10)
       (char=? (string-ref value 4) #\-)
       (char=? (string-ref value 7) #\-)
       (every char-numeric?
              (append (string->list (substring value 0 4))
                      (string->list (substring value 5 7))
                      (string->list (substring value 8 10))))
       (catch #t
         (lambda ()
           (let ((date (string->date value "~Y-~m-~d")))
             (string=? value (date->string date "~Y-~m-~d"))))
         (lambda args #f))))

(define (date-metadata elements kind path)
  (let ((value (one-metadata-value elements kind path)))
    (when (and value (not (iso-date? value)))
      (error "Forester page date must use YYYY-MM-DD" path kind value))
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
      (error "Forester page requires a .grv source" path))
    (unless (= (length titles) 1)
      (error "Forester page requires one top-level @title" path))
    (let ((title (string-trim-both
                   (content-verbatim-string
                     (content-element-children (car titles))))))
      (when (string-null? title)
        (error "Forester page title is empty" path))
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
