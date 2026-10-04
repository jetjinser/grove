;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project site)
  #:use-module (grove project)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-site-project))

(define* (make-site-project #:key
                            (title "Site")
                            (description #f)
                            (source-directory #f)
                            (source-filter (lambda (path)
                                             (string-suffix? ".plant" path)))
                            (collect-sources #f)
                            (prepare (lambda () '()))
                            (extra-sources '())
                            read-page
                            (analyze (lambda (site) site))
                            (renderers '())
                            output-directory
                            (cache? #t)
                            (default-prefix #f)
                            (tree-template #f))
  (unless (procedure? read-page)
    (error "site project requires a page reader" read-page))
  (unless (procedure? analyze)
    (error "site project analyzer must be a procedure" analyze))
  (make-project
    #:source-directory source-directory
    #:source-filter source-filter
    #:collect-sources collect-sources
    #:prepare prepare
    #:extra-sources extra-sources
    #:read-source read-page
    #:analyze
    (lambda (context)
      (let* ((inputs (project-context-inputs context))
             (pages (map project-input-value inputs))
             (sources
               (delete-duplicates
                 (append (map project-input-path inputs)
                         (project-context-prepared-sources context)
                         (filter-map site-page-source pages))
                 string=?))
             (site (make-site pages #:title title #:description description
                              #:sources sources)))
        (analyze site)))
    #:renderers renderers
    #:output-directory output-directory
    #:cache? cache?
    #:default-prefix default-prefix
    #:tree-template tree-template))
