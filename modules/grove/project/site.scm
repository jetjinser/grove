;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project site)
  #:use-module (grove i18n)
  #:use-module (grove content transform)
  #:use-module (grove project)
  #:use-module ((grove private project model)
                #:select (default-project-source-filter))
  #:use-module (grove private snapshot)
  #:use-module (grove private procedure)
  #:use-module (grove site)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-1)
  #:export (make-site-project))

(define* (make-site-project #:key
                            (title "Site")
                            (description #f)
                            (source-directory #f)
                            (source-filter default-project-source-filter)
                            (collect-sources #f)
                            (prepare (lambda () '()))
                            (extra-sources '())
                            read-page
                            (analyze (lambda (site) site))
                            (transforms '())
                            (renderers '())
                            (pipelines '())
                            (output-directory "build")
                            (cache? #t)
                            (home #f)
                            (browse #f)
                            (create-source #f)
                            (reader-locale #f))
  "Build a project from Grove site pages.

READ-PAGE constructs each page. ANALYZE receives the assembled Site and may
return any project model. Provide SOURCE-DIRECTORY or COLLECT-SOURCES, along
with READ-PAGE. OUTPUT-DIRECTORY defaults to \"build\". When both source
options are supplied, COLLECT-SOURCES determines the inputs and
SOURCE-DIRECTORY remains available
as the default source-creation destination. PREPARE returns tracked paths not
read as pages; EXTRA-SOURCES declares fixed project dependencies, also not
read as pages or added to the site's source list. Include an extra source in
an artifact's dependencies only when that artifact actually depends on it.
TRANSFORMS run in list order on each page's Content IR document before
ANALYZE.
RENDERERS is the complete output set; this general constructor installs no
format-specific renderer."
  ;;     These values are captured by the project callbacks below. Copy their
  ;;     declarative containers now, while keeping caller procedures shared.
  (set! title (snapshot-data title))
  (set! description (snapshot-data description))
  (set! transforms (snapshot-data transforms))
  (set! home (snapshot-data home))
  (set! reader-locale (snapshot-data reader-locale))
  (unless (non-empty-string? title)
    (error "site title must be a non-empty string" title))
  (unless (or (not description) (non-empty-string? description))
    (error "site description must be a non-empty string or #f" description))
  (unless (procedure-accepts-arguments? read-page 1)
    (error "site page reader must accept one source path" read-page))
  (unless (procedure-accepts-arguments? analyze 1)
    (error "site analyzer must accept one site" analyze))
  (unless (list? transforms)
    (error "site transforms must be a list" transforms))
  (unless (or (not home) (non-empty-string? home))
    (error "site home must be a non-empty page ID or #f" home))
  (when (and reader-locale (not (string? reader-locale)))
    (error "site reader locale must be a tag or #f" reader-locale))
  (let* ((locale (and reader-locale (make-locale reader-locale)))
         (locale-tag (and locale (locale-tag locale)))
         (transform (and (pair? transforms)
                         (apply compose-content-transforms transforms))))
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
            (unless (site-page? page)
              (error "site page reader must return a Grove site page"
                     page))
            (if (not transform)
                page
                (make-site-page
                  (site-page-id page)
                  (site-page-title page)
                  (transform (site-page-document page))
                  #:source (site-page-source page)
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
