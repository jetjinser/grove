;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private site resolve)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (forester private site model)
  #:use-module ((forester private site queries) #:select (require-page))
  #:use-module ((forester private trees) #:prefix tree-pass:)
  #:use-module ((forester private query-output) #:prefix query-output:)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (resolve-forest-queries
            validate-tree-cycles!))

(define (describe-tree-cycle pages cycle)
  (string-join
    (map (lambda (tree-id)
           (let ((page (find (lambda (candidate)
                               (equal? tree-id (site-page-id candidate)))
                             pages)))
             (format #f "~a (~a)" tree-id
                     (or (and page (site-page-source page))
                         "<unknown source>"))))
         cycle)
    " -> "))

(define (validate-tree-cycles! pages targets)
  (let ((edges
          (map (lambda (page)
                 (cons (site-page-id page)
                       (targets page)))
               pages)))
    (letrec ((visit
               (lambda (id path)
                 (when (member id path equal?)
                   (error
                     (format #f "cyclic Forester tree composition: ~a"
                             (describe-tree-cycle
                               pages (reverse (cons id path))))))
                 (for-each (lambda (target)
                             (visit target (cons id path)))
                           (cdr (assoc id edges))))))
      (for-each (lambda (page) (visit (site-page-id page) '())) pages))))

(define (query-in-content? content)
  (and (content-document? content)
       (any (lambda (child)
              (and (content-element? child)
                   (eq? (content-element-kind child) 'forest-query)))
            (ir:content-document-children/raw content))))

(define (nested-query-in-content? content)
  (cond
    ((content-element? content)
     (or (eq? (content-element-kind content) 'forest-query)
         (and (not (eq? (content-element-kind content) 'subtree))
              (any nested-query-in-content?
                   (ir:content-element-children/raw content)))))
    ((content-document? content)
     (any nested-query-in-content? (ir:content-document-children/raw content)))
    (else #f)))

(define (query-source-label page request)
  (let ((location (ir:content-element-location/raw request)))
    (format #f "~a~a"
      (or (and (list? location) (assq-ref location 'file))
          (site-page-source page)
          (site-page-id page))
      (if (and (list? location) (assq-ref location 'line))
          (format #f ":~a:~a"
            (assq-ref location 'line)
            (or (assq-ref location 'column) 1))
          ""))))

(define (resolve-page-queries forest anchor-index page)
  (define (resolve content)
    (if (and (content-element? content)
             (eq? (content-element-kind content) 'forest-query))
        (let* ((source (query-source-label page content))
               (query (ir:content-element-property/raw content 'query))
               (result
                 (catch #t
                   (lambda () (query forest (site-page-id page)))
                   (lambda (key . args)
                     (error (format #f "Forester query failed at ~a: ~s"
                                    source (cons key args))))))
               (known-anchor?
                 (lambda (target fragment)
                   (and anchor-index
                        (member fragment
                                (hash-ref anchor-index target '())
                                string=?)))))
          (ir:content-document-children/raw
            (query-output:prepare-query-output result source
              (lambda (target) (require-page forest target))
              known-anchor?)))
        (list content)))
  (make-content-document
    (append-map resolve (ir:content-document-children/raw
                          (site-page-document page)))
    #:source (ir:content-document-source/raw (site-page-document page))))

(define (validate-query-composition! forest resolved-pages)
  ;; Validate the resolved display graph without adding generated facts to the
  ;; authored composition snapshot.
  (validate-tree-cycles!
    resolved-pages
    (lambda (page)
      (append
        (composition:composition-target-ids
          (composition forest) (site-page-id page))
        (map (lambda (element) (ir:content-element-property/raw element 'target))
             (tree-pass:walk-owned-content
               (site-page-document page)
               (tree-pass:element-kind? 'query-transclude)))))))

(define (resolve-forest-queries forest anchor-index)
  (for-each
    (lambda (page)
      (when (any nested-query-in-content?
                 (filter (lambda (child)
                           (not (and (content-element? child)
                                     (eq? (content-element-kind child)
                                          'forest-query))))
                         (ir:content-document-children/raw
                           (site-page-document page))))
        (error "Forester query must be a top-level page command"
               (site-page-source page))))
    (pages forest))
  (let* ((query-pages
           (filter (lambda (page)
                     (query-in-content? (site-page-document page)))
                   (pages forest))))
    (if (null? query-pages)
        forest
        (let* ((query-page-ids (map site-page-id query-pages))
               (query-ids
                 (let ((ids (make-hash-table)))
                   (for-each (lambda (id) (hash-set! ids id #t))
                             query-page-ids)
                   ids))
               (resolved-pages
                 (map (lambda (page)
                        (if (hash-ref query-ids (site-page-id page))
                            (make-site-page
                              (site-page-id page)
                              (site-page-title page)
                              (resolve-page-queries forest anchor-index page)
                              #:source (site-page-source page)
                              #:metadata (site-page-metadata page))
                            page))
                      (pages forest)))
               (resolved-page-index (make-hash-table))
               (_ (for-each
                    (lambda (page)
                      (hash-set! resolved-page-index (site-page-id page) page))
                    resolved-pages))
               (resolved-site
                 (make-site resolved-pages
                   #:title (site-title (site forest))
                   #:description (site-description (site forest))
                   #:sources (site-sources (site forest))
                   #:home (site-home (site forest)))))
          (validate-query-composition! forest resolved-pages)
          (%make-forest
            resolved-site resolved-pages
            (reference-relation forest) (composition forest)
            resolved-page-index (page-relation forest)
            (described-entity-relation forest)
            (content-node-relation forest) (content-node-index forest)
            (term-definition-relation forest)
            (term-mention-relation forest)
            query-page-ids)))))
