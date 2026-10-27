;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html routes)
  #:use-module (grove i18n)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (grove private uri)
  #:use-module (grove output)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (html-routes?
            make-html-routes
            html-page-path
            locale-page-path
            html-route-table
            html-index-path
            html-home-page
            html-routes-with-home-page
            html-page-url
            html-index-url
            html-asset-url))

(define-record-type <html-routes>
  (%make-html-routes page-path index-path url-prefix home-page)
  html-routes?
  (page-path html-page-path-procedure)
  (index-path html-index-path/raw)
  (url-prefix html-url-prefix)
  (home-page html-home-page/raw))

(define (html-index-path routes)
  (snapshot-data (html-index-path/raw routes)))

(define (html-home-page routes)
  (snapshot-data (html-home-page/raw routes)))

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
  (unless (or (not home-page) (non-empty-string? home-page))
    (error "HTML home page ID must be a non-empty string or #f" home-page))
  (unless (and (string? url-prefix)
               (string-prefix? "/" url-prefix)
               (string-suffix? "/" url-prefix))
    (error "HTML URL prefix must begin and end with /" url-prefix))
  (%make-html-routes page-path (snapshot-data index-path)
                     (snapshot-data url-prefix) (snapshot-data home-page)))

(define (html-routes-with-home-page routes home-page)
  "Return ROUTES with an explicit home page, e.g. from the site."
  (unless (html-routes? routes)
    (error "HTML routes with home requires HTML routes" routes))
  (unless (non-empty-string? home-page)
    (error "HTML home page ID must be a non-empty string" home-page))
  (%make-html-routes (html-page-path-procedure routes)
                     (html-index-path routes)
                     (html-url-prefix routes)
                     (snapshot-data home-page)))

(define (html-page-path routes id)
  (unless (html-routes? routes)
    (error "HTML page path requires HTML routes" routes))
  (unless (non-empty-string? id)
    (error "HTML page path requires a non-empty page ID" id))
  (let ((path (if (and (html-home-page routes)
                       (string=? id (html-home-page routes)))
                  (html-index-path routes)
                  ((html-page-path-procedure routes) (snapshot-data id)))))
    (unless (artifact-path? path)
      (error "HTML page requires a relative artifact path" id path))
    (snapshot-data path)))

(define (html-route-table routes ids)
  (unless (html-routes? routes)
    (error "HTML route table requires HTML routes" routes))
  (unless (and (list? ids) (every non-empty-string? ids))
    (error "HTML route table requires a list of non-empty page IDs" ids))
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
    (snapshot-data table)))

(define (html-asset-url routes path)
  "Return the URL for a relative artifact path under ROUTES' URL prefix."
  (unless (html-routes? routes)
    (error "HTML asset URL requires HTML routes" routes))
  (unless (artifact-path? path)
    (error "HTML URL requires a relative artifact path" path))
  (string-append (html-url-prefix routes) (uri-encode-path path)))

(define (html-page-url routes id)
  (html-asset-url routes (html-page-path routes id)))

(define (html-index-url routes)
  (html-asset-url routes (html-index-path routes)))

(define* (locale-page-path locale-of #:optional (build #f)
                           #:key (default-locale #f))
  "Return a page-path procedure that localizes page paths.

LOCALE-OF maps a page id to its locale tag or #f.  BUILD, an optional
`(locale id) -> path' procedure, decides the path; the default keeps
Forester's flat layout with a locale suffix (`guide.zh-Hans.html').
Pages without a locale, and pages in DEFAULT-LOCALE, use `id.html'."
  (unless (procedure? locale-of)
    (error "Locale page path requires a procedure" locale-of))
  (unless (or (not build) (procedure? build))
    (error "Locale page path builder must be a procedure or #f" build))
  (unless (or (not default-locale) (non-empty-string? default-locale))
    (error "Locale page path default locale must be a string or #f"
           default-locale))
  (when default-locale
    (set! default-locale (locale-tag (make-locale default-locale))))
  (set! default-locale (snapshot-data default-locale))
  (let ((build (or build
                   (lambda (locale id)
                     (if locale
                         (string-append id "." locale ".html")
                         (string-append id ".html"))))))
    (lambda (id)
      (unless (non-empty-string? id)
        (error "Locale page path requires a non-empty page ID" id))
      (let ((locale (snapshot-data (locale-of (snapshot-data id)))))
        (unless (or (not locale) (non-empty-string? locale))
          (error "Locale page path requires a locale tag or #f" id locale))
        (when locale
          (set! locale (locale-tag (make-locale locale))))
        (snapshot-data
          (build (snapshot-data (and (not (equal? locale default-locale)) locale))
                 (snapshot-data id)))))))
