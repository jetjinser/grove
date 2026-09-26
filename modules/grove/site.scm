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
            site-title
            site-links
            site-backlinks))

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

(define (links-in content)
  (cond
    ((content-element? content)
     (let ((nested (append-map links-in (content-element-children content))))
       (if (eq? (content-element-kind content) 'link)
           (cons (content-element-property content 'target) nested)
           nested)))
    ((content-document? content)
     (append-map links-in (content-document-children content)))
    (else '())))

(define-record-type <site>
  (%make-site pages title links)
  site?
  (pages site-pages)
  (title site-title)
  (links site-link-index))

(define* (make-site pages #:key (title "Notes"))
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
                          ids))
         (links (map (lambda (page)
                       (cons (site-page-id page)
                             (delete-duplicates
                               (links-in (site-page-document page))
                               equal?)))
                     pages)))
    (when duplicate (error "duplicate site page id" duplicate))
    (for-each
      (lambda (entry)
        (for-each
          (lambda (target)
            (unless (member target ids equal?)
              (error "unresolved Grove link" (car entry) target)))
          (cdr entry)))
      links)
    (%make-site pages title links)))

(define (site-links site id)
  (let ((entry (assoc id (site-link-index site))))
    (unless entry
      (error "unknown site page" id))
    (cdr entry)))

(define (site-backlinks site id)
  (site-links site id)
  (filter (lambda (source)
            (member id
                    (site-links site (site-page-id source))
                    equal?))
          (site-pages site)))
