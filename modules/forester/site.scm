;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (forest?
            make-forest
            forest-site
            forest-links
            forest-backlinks))

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

(define-record-type <forest>
  (%make-forest site links)
  forest?
  (site forest-site)
  (links forest-link-index))

(define (make-forest site)
  (unless (site? site)
    (error "forest requires a Grove site" site))
  (let* ((pages (site-pages site))
         (ids (map site-page-id pages))
         (links (map (lambda (page)
                       (cons (site-page-id page)
                             (links-in (site-page-document page))))
                     pages)))
    (for-each
      (lambda (entry)
        (for-each
          (lambda (target)
            (unless (member target ids equal?)
              (error "unresolved Forester link" (car entry) target)))
          (cdr entry)))
      links)
    (%make-forest site links)))

(define (forest-links forest id)
  (let ((entry (assoc id (forest-link-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (cdr entry)))

(define (forest-backlinks forest id)
  (forest-links forest id)
  (filter (lambda (source)
            (member id
                    (forest-links forest (site-page-id source))
                    equal?))
          (site-pages (forest-site forest))))
