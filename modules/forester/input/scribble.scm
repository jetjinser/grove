;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester input scribble)
  #:use-module (ice-9 ftw)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-forester-page-reader))

(define (title-element? child)
  (and (content-element? child)
       (eq? (content-element-kind child) 'title)))

(define (page-from-document path document)
  (let* ((filename (basename path))
         (children (content-document-children document))
         (titles (filter title-element? children)))
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
          (remove title-element? children)
          (content-document-source document))
        path))))

(define (make-forester-page-reader read-document)
  (lambda (path)
    (page-from-document path (read-document path))))
