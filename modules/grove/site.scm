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
            site?
            make-site
            site-pages
            site-title))

(define-record-type <site-page>
  (%make-site-page id title document)
  site-page?
  (id site-page-id)
  (title site-page-title)
  (document site-page-document))

(define (make-site-page id title document)
  (unless (content-document? document)
    (error "site page requires a content document" document))
  (%make-site-page id title document))

(define-record-type <site>
  (%make-site pages title)
  site?
  (pages site-pages)
  (title site-title))

(define* (make-site pages #:key (title "Site"))
  (unless (every site-page? pages)
    (error "site requires pages" pages))
  (unless (and (string? title) (not (string=? title "")))
    (error "site requires a title" title))
  (let* ((ids (map site-page-id pages))
         (duplicate (find (lambda (id)
                            (> (count (lambda (candidate)
                                        (equal? id candidate))
                                      ids)
                               1))
                          ids)))
    (when duplicate (error "duplicate site page id" duplicate))
    (%make-site pages title)))
