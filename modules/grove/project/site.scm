;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project site)
  #:use-module (grove i18n)
  #:use-module (grove content transform)
  #:use-module (grove project)
  #:use-module (grove private snapshot)
  #:use-module (grove site)
  #:use-module (grove string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-site-project))

(define* (make-site-project #:key
                            (title "Site")
                            (description #f)
                            (source-directory #f)
                            (source-filter (lambda (path)
                                             (and (not (string-prefix? "." path))
                                                  (string-suffix? ".plant" path))))
                            (collect-sources #f)
                            (prepare (lambda () '()))
                            (extra-sources '())
                            read-page
                            (analyze (lambda (site) site))
                            (transforms '())
                            (renderers '())
                            (pipelines '())
                            output-directory
                            (cache? #t)
                            (home #f)
                            (browse #f)
                            (create-source #f)
                            (reader-locale #f))
  "Build a project from Grove site pages.

READ-PAGE constructs each page. ANALYZE receives the assembled Site and may
return any project model. READ-PAGE and OUTPUT-DIRECTORY are required."
  ;;     These values are captured by the project callbacks below. Copy their
  ;;     declarative containers now, while keeping caller procedures shared.
  (set! title (snapshot-data title))
  (set! description (snapshot-data description))
  (set! source-directory (snapshot-data source-directory))
  (set! extra-sources (snapshot-data extra-sources))
  (set! transforms (snapshot-data transforms))
  (set! renderers (snapshot-data renderers))
  (set! pipelines (snapshot-data pipelines))
  (set! output-directory (snapshot-data output-directory))
  (set! home (snapshot-data home))
  (set! reader-locale (snapshot-data reader-locale))
  (unless (non-empty-string? title)
    (error "site title must be a non-empty string" title))
  (unless (or (not description) (non-empty-string? description))
    (error "site description must be a non-empty string or #f" description))
  (unless (and (or (not source-directory)
                   (non-empty-string? source-directory))
               (or (not collect-sources) (procedure? collect-sources))
               (or source-directory collect-sources))
    (error "site project requires a source directory or collector"
           source-directory collect-sources))
  (unless (procedure? source-filter)
    (error "site source filter must be a procedure" source-filter))
  (unless (procedure? prepare)
    (error "site prepare step must be a procedure" prepare))
  (unless (and (list? extra-sources) (every non-empty-string? extra-sources))
    (error "site extra sources must be a list of non-empty paths"
           extra-sources))
  (unless (procedure? read-page)
    (error "site project requires a page reader" read-page))
  (unless (procedure? analyze)
    (error "site analyzer must be a procedure" analyze))
  (unless (and (list? transforms) (every procedure? transforms))
    (error "site transforms must be procedures" transforms))
  (unless (non-empty-string? output-directory)
    (error "site project requires an output directory" output-directory))
  (unless (or (not home) (non-empty-string? home))
    (error "site home must be a non-empty page ID or #f" home))
  (unless (or (not create-source) (procedure? create-source))
    (error "site source creator must be a procedure or #f" create-source))
  (when reader-locale
    (unless (string? reader-locale)
      (error "site reader locale must be a tag or #f" reader-locale))
    (make-locale reader-locale))
  (let ((locale-tag (and reader-locale
                         (locale-tag (make-locale reader-locale)))))
    (make-project
      #:source-directory source-directory
      #:source-filter source-filter
      #:collect-sources collect-sources
      #:prepare prepare
      #:extra-sources extra-sources
      #:read-source
      (lambda (source)
        (parameterize ((current-reader-locale
                         locale-tag))
          (let ((page (read-page source)))
            (if (null? transforms)
                page
                (make-site-page
                  (site-page-id page)
                  (site-page-title page)
                  (transform-document
                    (apply compose-content-transforms transforms)
                    (site-page-document page))
                  (site-page-source page)
                  #:metadata (site-page-metadata page))))))
      #:analyze
      (lambda (analysis-context)
        (let* ((inputs (project-context-inputs analysis-context))
               (pages (map project-input-value inputs))
               (sources
                 (delete-duplicates
                   (append (map project-input-path inputs)
                           (project-context-prepared-sources analysis-context)
                           (filter-map site-page-source pages))
                   string=?))
               (site (make-site pages #:title title #:description description
                                #:sources sources #:home home)))
          (analyze site)))
      #:renderers renderers
      #:pipelines pipelines
      #:output-directory output-directory
      #:cache? cache?
      #:browse browse
      #:create-source create-source)))
