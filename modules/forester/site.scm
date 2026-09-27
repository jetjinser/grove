;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove relation)
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
            forest-reference?
            forest-reference-source-page-id
            forest-reference-source-id
            forest-reference-target-page-id
            forest-reference-target-fragment
            forest-reference-label
            forest-heading?
            forest-heading-id
            forest-heading-label
            forest-headings
            forest-outgoing-references
            forest-incoming-references
            forest-source-paths
            forest-module-paths
            forest-page-source-paths))

(define (link-elements content)
  (cond
    ((content-element? content)
     (append (append-map link-elements
                         (content-element-children content))
             (if (eq? (content-element-kind content) 'link)
                 (list content)
                 '())))
    ((content-document? content)
     (append-map link-elements (content-document-children content)))
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

(define-record-type <forest-reference>
  (%make-forest-reference source-page-id source-id target-page-id
                          target-fragment label)
  forest-reference?
  (source-page-id forest-reference-source-page-id)
  (source-id forest-reference-source-id)
  (target-page-id forest-reference-target-page-id)
  (target-fragment forest-reference-target-fragment)
  (label forest-reference-label))

(define-record-type <forest-heading>
  (%make-forest-heading id label)
  forest-heading?
  (id forest-heading-id)
  (label forest-heading-label))

(define-record-type <forest>
  (%make-forest site reference-index heading-index page-reference-relation)
  forest?
  (site forest-site)
  (reference-index forest-reference-index)
  (heading-index forest-heading-index)
  (page-reference-relation forest-page-reference-relation))

(define (make-forest site)
  (unless (site? site)
    (error "forest requires a Grove site" site))
  (let* ((pages (site-pages site))
         (heading-index
           (map (lambda (page)
                  (let* ((elements
                           (heading-elements (site-page-document page)))
                         (ids (unique-heading-slugs elements)))
                    (cons
                      (site-page-id page)
                      (map (lambda (element id)
                             (%make-forest-heading
                               id (content->plain-text element)))
                           elements ids))))
                pages))
         (reference-index
           (map
             (lambda (source-page)
               (let* ((source-id (site-page-id source-page))
                      (elements
                        (link-elements (site-page-document source-page))))
                 (cons
                   source-id
                   (map
                     (lambda (element index)
                       (let* ((target
                                (content-element-property element 'target))
                              (target-page-id
                                (forest-page-id-from-link target))
                              (fragment (forest-fragment-from-link target))
                              (target-page
                                (find
                                  (lambda (page)
                                    (equal? target-page-id
                                            (site-page-id page)))
                                  pages)))
                         (unless target-page
                           (error
                             (format #f
                               "unresolved Forester link in page ~a (~a): ~a"
                               source-id
                               (or (site-page-source source-page)
                                   "<unknown source>")
                               target)))
                         (when (and fragment
                                    (not (member
                                           fragment
                                           (map forest-heading-id
                                                (cdr (assoc target-page-id
                                                            heading-index)))
                                           string=?)))
                           (error
                             (format #f
                               "unresolved Forester fragment in page ~a (~a): ~a"
                               source-id
                               (or (site-page-source source-page)
                                   "<unknown source>")
                               target)))
                         (%make-forest-reference
                           source-id
                           (format #f "ref-~a" index)
                           target-page-id
                           fragment
                           (content->plain-text element))))
                     elements
                     (iota (length elements) 1)))))
             pages))
         (page-reference-relation
           (make-relation
             (append-map
               (lambda (entry)
                 (map (lambda (reference)
                        (cons (forest-reference-source-page-id reference)
                              (forest-reference-target-page-id reference)))
                      (cdr entry)))
               reference-index))))
    (%make-forest site reference-index heading-index
                  page-reference-relation)))

(define (forest-heading-ids forest id)
  (map forest-heading-id (forest-headings forest id)))

(define (forest-headings forest id)
  (let ((entry (assoc id (forest-heading-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (cdr entry)))

(define (forest-links forest id)
  (map (lambda (reference)
         (let ((fragment (forest-reference-target-fragment reference)))
           (string-append (forest-reference-target-page-id reference)
                          (if fragment (string-append "#" fragment) ""))))
       (forest-outgoing-references forest id)))

(define (forest-outgoing-references forest id)
  (let ((entry (assoc id (forest-reference-index forest))))
    (unless entry
      (error "unknown forest page" id))
    (cdr entry)))

(define* (forest-incoming-references forest id #:optional fragment)
  (unless (assoc id (forest-heading-index forest))
    (error "unknown forest page" id))
  (when (and fragment
             (not (member fragment (forest-heading-ids forest id) string=?)))
    (error "unknown forest heading fragment" id fragment))
  (filter (lambda (reference)
            (and (equal? id
                         (forest-reference-target-page-id reference))
                 (or (not fragment)
                     (equal? fragment
                             (forest-reference-target-fragment reference)))))
          (append-map cdr (forest-reference-index forest))))

(define (forest-backlinks forest id)
  (unless (assoc id (forest-heading-index forest))
    (error "unknown forest page" id))
  (let ((source-ids
          (run* (source-id)
            (relationo (forest-page-reference-relation forest)
                      source-id id))))
    (filter (lambda (page) (member (site-page-id page) source-ids equal?))
            (site-pages (forest-site forest)))))

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
