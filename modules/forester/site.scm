;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (forest?
            make-forest
            forest-site
            forest-links
            forest-backlinks
            forest-page-id-from-link
            forest-fragment-from-link
            forest-heading-ids
            forest-source-paths
            forest-module-paths
            forest-page-source-paths))

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

(define (split-link-target target)
  (unless (and (string? target) (not (string-null? target)))
    (error "Forester link target must be a non-empty string" target))
  (let ((separator (string-index target #\#)))
    (if separator
        (let ((page-id (substring target 0 separator))
              (fragment (substring target (+ separator 1))))
          (when (or (string-null? page-id)
                    (string-null? fragment)
                    (string-index fragment #\#))
            (error "invalid Forester page fragment target" target))
          (cons page-id fragment))
        (cons target #f))))

(define (forest-page-id-from-link target)
  (car (split-link-target target)))

(define (forest-fragment-from-link target)
  (cdr (split-link-target target)))

(define (content->plain-text content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content)
     (string-concatenate (map content->plain-text
                              (content-element-children content))))
    ((content-document? content)
     (string-concatenate (map content->plain-text
                              (content-document-children content))))
    (else "")))

(define (heading-elements content)
  (cond
    ((content-element? content)
     (append (append-map heading-elements
                         (content-element-children content))
             (if (eq? (content-element-kind content) 'heading)
                 (list content)
                 '())))
    ((content-document? content)
     (append-map heading-elements (content-document-children content)))
    (else '())))

(define (heading-slug heading)
  (let loop ((characters (string->list (content->plain-text heading)))
             (result '())
             (separator? #f))
    (if (null? characters)
        (let ((slug (list->string (reverse result))))
          (if (string-null? slug) "section" slug))
        (let ((character (car characters)))
          (if (or (char-alphabetic? character)
                  (char-numeric? character))
              (loop (cdr characters)
                    (cons (char-downcase character)
                          (if (and separator? (pair? result))
                              (cons #\- result)
                              result))
                    #f)
              (loop (cdr characters) result (pair? result)))))))

(define (unique-heading-slugs headings)
  (let loop ((headings headings) (used '()) (result '()))
    (if (null? headings)
        (reverse result)
        (let* ((base (heading-slug (car headings)))
               (id (let find-id ((suffix 1))
                     (let ((candidate
                             (if (= suffix 1)
                                 base
                                 (format #f "~a-~a" base suffix))))
                       (if (member candidate used string=?)
                           (find-id (+ suffix 1))
                           candidate)))))
          (loop (cdr headings) (cons id used) (cons id result))))))

(define-record-type <forest>
  (%make-forest site links heading-index)
  forest?
  (site forest-site)
  (links forest-link-index)
  (heading-index forest-heading-index))

(define (make-forest site)
  (unless (site? site)
    (error "forest requires a Grove site" site))
  (let* ((pages (site-pages site))
         (links (map (lambda (page)
                       (cons (site-page-id page)
                             (links-in (site-page-document page))))
                     pages))
         (heading-index
           (map (lambda (page)
                  (cons (site-page-id page)
                        (unique-heading-slugs
                          (heading-elements (site-page-document page)))))
                pages)))
    (for-each
      (lambda (entry)
        (for-each
          (lambda (target)
            (let* ((page-id (forest-page-id-from-link target))
                   (fragment (forest-fragment-from-link target))
                   (page (find (lambda (candidate)
                                 (equal? page-id (site-page-id candidate)))
                               pages)))
              (unless page
                (let* ((id (car entry))
                       (source-page (find (lambda (candidate)
                                            (equal? id (site-page-id candidate)))
                                          pages)))
                  (error
                    (format #f "unresolved Forester link in page ~a (~a): ~a"
                            id
                            (or (site-page-source source-page) "<unknown source>")
                            target))))
              (when (and fragment
                         (not (member fragment
                                      (cdr (assoc page-id heading-index))
                                      string=?)))
                (error
                  (format #f "unresolved Forester fragment in page ~a (~a): ~a"
                          (car entry)
                          (or (site-page-source
                                (find (lambda (candidate)
                                        (equal? (car entry)
                                                (site-page-id candidate)))
                                      pages))
                              "<unknown source>")
                          target)))))
          (cdr entry)))
      links)
    (%make-forest site links heading-index)))

(define (forest-heading-ids forest id)
  (let ((entry (assoc id (forest-heading-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (cdr entry)))

(define (forest-links forest id)
  (let ((entry (assoc id (forest-link-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (cdr entry)))

(define (forest-backlinks forest id)
  (forest-links forest id)
  (filter (lambda (source)
            (member id
                    (map forest-page-id-from-link
                         (forest-links forest (site-page-id source)))
                    equal?))
          (site-pages (forest-site forest))))

(define (forest-source-paths forest)
  (let ((site (forest-site forest)))
    (delete-duplicates
      (append (site-sources site)
              (filter-map site-page-source (site-pages site)))
      string=?)))

(define (forest-module-paths forest)
  (let* ((site (forest-site forest))
         (page-paths (filter-map site-page-source (site-pages site))))
    (filter (lambda (path) (not (member path page-paths string=?)))
            (site-sources site))))

(define (forest-page-source-paths forest page)
  (delete-duplicates
    (append (forest-module-paths forest)
            (filter-map site-page-source
                        (cons page
                              (forest-backlinks forest
                                                (site-page-id page)))))
    string=?))
