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
  #:export (load-site-pages))

(define (title-element? child)
  (and (content-element? child)
       (eq? (content-element-kind child) 'title)))

(define (page-from-file directory filename environment)
  (let* ((path (string-append directory "/" filename))
         (document
           (call-with-input-file path
             (lambda (port)
               (eval ((language-reader scribble) port environment)
                     environment))))
         (children (content-document-children document))
         (titles (filter title-element? children)))
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

(define (load-site-pages directory)
  (let ((environment (default-environment scribble)))
    (map (lambda (filename)
           (page-from-file directory filename environment))
         (scandir directory
                  (lambda (filename)
                    (string-suffix? ".scrbl" filename))
                  string<?))))
