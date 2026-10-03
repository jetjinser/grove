;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html routes)
  #:use-module (grove string)
  #:use-module (grove output)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (html-routes?
            make-html-routes
            html-page-path
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
  (let ((path ((html-page-path-procedure routes) id)))
    (unless (artifact-path? path)
      (error "HTML page requires a relative artifact path" id path))
    path))

(define (html-asset-url routes path)
  (unless (artifact-path? path)
    (error "HTML URL requires a relative artifact path" path))
  (string-append (html-url-prefix routes) path))

(define (html-page-url routes id)
  (html-asset-url routes (html-page-path routes id)))

(define (html-index-url routes)
  (html-asset-url routes (html-index-path routes)))
