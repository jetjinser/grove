;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove input scribble)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (system base language)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (language scribble spec)
  #:export (make-scribble-page-reader))

(define (title-element? child)
  (and (content-element? child)
       (eq? (content-element-kind child) 'title)))

(define (page-from-file path environment)
  (let* ((filename (basename path))
         (document
           (call-with-input-file path
             (lambda (port)
               (eval ((language-reader scribble) port environment)
                     environment))))
         (children (content-document-children document))
         (titles (filter title-element? children)))
    (unless (string-suffix? ".scrbl" filename)
      (error "Scribble page requires a .scrbl source" path))
    (unless (= (length titles) 1)
      (error "Scribble page requires one top-level @title" path))
    (let ((title (string-trim-both
                   (content-verbatim-string
                     (content-element-children (car titles))))))
      (when (string-null? title)
        (error "Scribble page title is empty" path))
      (make-site-page
        (string-drop-right filename (string-length ".scrbl"))
        title
        (make-content-document
          (remove title-element? children)
          (content-document-source document))))))

(define* (make-scribble-page-reader
           #:optional (environment (default-environment scribble)))
  (lambda (path)
    (page-from-file path environment)))
