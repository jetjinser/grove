;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html routes)
  #:use-module (grove string-fun)
  #:use-module (grove output)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (html-routes?
            make-html-routes
            html-page-path
            html-route-table
            html-index-path
            html-home-page
            html-page-url
            html-index-url
            html-asset-url))

(define-record-type <html-routes>
  (%make-html-routes page-path index-path url-prefix home-page)
  html-routes?
  (page-path html-page-path-procedure)
  (index-path html-index-path)
  (url-prefix html-url-prefix)
  (home-page html-home-page))

(define* (make-html-routes
           #:key
           home-page
           (page-path (lambda (id) (string-append id ".html")))
           (index-path "index.html")
           (url-prefix "/"))
  (unless (procedure? page-path)
    (error "HTML page path requires a procedure" page-path))
  (unless (artifact-path? index-path)
    (error "HTML index requires a relative artifact path" index-path))
  (unless (non-empty-string? home-page)
    (error "HTML home page ID must be a non-empty string" home-page))
  (unless (and (string? url-prefix)
               (string-prefix? "/" url-prefix)
               (string-suffix? "/" url-prefix))
    (error "HTML URL prefix must begin and end with /" url-prefix))
  (%make-html-routes page-path index-path url-prefix home-page))

(define (html-page-path routes id)
  (let ((path (if (string=? id (html-home-page routes))
                  (html-index-path routes)
                  ((html-page-path-procedure routes) id))))
    (unless (artifact-path? path)
      (error "HTML page requires a relative artifact path" id path))
    path))

(define (html-route-table routes ids)
  (let ((table (map (lambda (id) (cons id (html-page-path routes id))) ids)))
    (let loop ((remaining table) (seen '()))
      (unless (null? remaining)
        (let* ((entry (car remaining))
               (path (cdr entry))
               (collision (find (lambda (other) (string=? path (cdr other)))
                                seen)))
          (when collision
            (error "HTML page routes must be unique"
                   (car collision) (car entry) path))
          (loop (cdr remaining) (cons entry seen)))))
    table))

(define (html-asset-url routes path)
  (unless (artifact-path? path)
    (error "HTML URL requires a relative artifact path" path))
  (string-append (html-url-prefix routes) path))

(define (html-page-url routes id)
  (html-asset-url routes (html-page-path routes id)))

(define (html-index-url routes)
  (html-asset-url routes (html-index-path routes)))
