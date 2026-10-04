;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove site)
  #:use-module (grove string-fun)
  #:use-module (grove ir)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (site-page?
            make-site-page
            site-page-id
            site-page-title
            site-page-document
            site-page-source
            site-page-metadata
            site?
            make-site
            site-pages
            site-sources
            site-title
            site-description))

(define-record-type <site-page>
  (%make-site-page id title document source metadata)
  site-page?
  (id site-page-id)
  (title site-page-title)
  (document site-page-document)
  (source site-page-source)
  (metadata site-page-metadata))

(define* (make-site-page id title document #:optional (source #f)
                         #:key (metadata '()))
  (unless (non-empty-string? id)
    (error "site page ID must be a non-empty string" id))
  (unless (non-empty-string? title)
    (error "site page title must be a non-empty string" title))
  (unless (content-document? document)
    (error "site page requires a content document" document))
  (unless (or (not source)
              (non-empty-string? source))
    (error "site page source must be a non-empty path or #f" source))
  (unless (and (list? metadata)
               (every (lambda (entry)
                        (and (pair? entry) (symbol? (car entry))))
                      metadata)
               (let loop ((entries metadata) (keys '()))
                 (or (null? entries)
                     (and (not (memq (caar entries) keys))
                          (loop (cdr entries)
                                (cons (caar entries) keys))))))
    (error "site page metadata must have unique symbol keys" metadata))
  (%make-site-page id title document source metadata))

(define-record-type <site>
  (%make-site pages title sources description)
  site?
  (pages site-pages)
  (title site-title)
  (sources site-sources)
  (description site-description))

(define* (make-site pages #:key (title "Site") (description #f)
                    (sources '()))
  (unless (every site-page? pages)
    (error "site requires pages" pages))
  (unless (non-empty-string? title)
    (error "site requires a title" title))
  (unless (or (not description)
              (non-empty-string? description))
    (error "site description must be a non-empty string or #f" description))
  (unless (and (list? sources) (every string? sources))
    (error "site sources must be a list of paths" sources))
  (let* ((ids (map site-page-id pages))
         (duplicate (find (lambda (id)
                            (> (count (lambda (candidate)
                                        (equal? id candidate))
                                      ids)
                               1))
                          ids)))
    (when duplicate
      (error
        (format #f "duplicate site page id ~s in sources: ~{~a~^, ~}"
                duplicate
                (map (lambda (page)
                       (or (site-page-source page) "<unknown source>"))
                     (filter (lambda (page)
                               (equal? duplicate (site-page-id page)))
                             pages)))))
    (%make-site pages title sources description)))
