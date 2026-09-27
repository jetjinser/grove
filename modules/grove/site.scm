;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove site)
  #:use-module (grove ir)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (site-page?
            make-site-page
            site-page-id
            site-page-title
            site-page-document
            site-page-source
            site?
            make-site
            site-pages
            site-sources
            site-title))

(define-record-type <site-page>
  (%make-site-page id title document source)
  site-page?
  (id site-page-id)
  (title site-page-title)
  (document site-page-document)
  (source site-page-source))

(define* (make-site-page id title document #:optional (source #f))
  (unless (content-document? document)
    (error "site page requires a content document" document))
  (unless (or (not source) (string? source))
    (error "site page source must be a path or #f" source))
  (%make-site-page id title document source))

(define-record-type <site>
  (%make-site pages title sources)
  site?
  (pages site-pages)
  (title site-title)
  (sources site-sources))

(define* (make-site pages #:key (title "Site") (sources '()))
  (unless (every site-page? pages)
    (error "site requires pages" pages))
  (unless (and (string? title) (not (string=? title "")))
    (error "site requires a title" title))
  (unless (and (list? sources) (every string? sources))
    (error "site sources must be a list of paths" sources))
  (let* ((ids (map site-page-id pages))
         (duplicate (find (lambda (id)
                            (> (count (lambda (candidate)
                                        (equal? id candidate))
                                      ids)
                               1))
                          ids)))
    (when duplicate (error "duplicate site page id" duplicate))
    (%make-site pages title sources)))
