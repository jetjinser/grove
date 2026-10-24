;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project site)
  #:use-module (grove build)
  #:use-module (grove i18n)
  #:use-module (grove content transform)
  #:use-module (grove project)
  #:use-module (grove pipeline)
  #:use-module (grove render context)
  #:use-module (grove render derived)
  #:use-module (grove site)
  #:use-module (grove string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (make-site-options
            site-options?
            make-site-project))

(define-record-type <site-options>
  (%make-site-options title description source-directory source-filter
                      collect-sources prepare extra-sources read-page analyze
                      transforms renderers derived derived-context pipelines
                      output-directory cache? default-prefix tree-template
                      home browse locale)
  site-options?
  (title site-options-title)
  (description site-options-description)
  (source-directory site-options-source-directory)
  (source-filter site-options-source-filter)
  (collect-sources site-options-collect-sources)
  (prepare site-options-prepare)
  (extra-sources site-options-extra-sources)
  (read-page site-options-read-page)
  (analyze site-options-analyze)
  (transforms site-options-transforms)
  (renderers site-options-renderers)
  (derived site-options-derived)
  (derived-context site-options-derived-context)
  (pipelines site-options-pipelines)
  (output-directory site-options-output-directory)
  (cache? site-options-cache?)
  (default-prefix site-options-default-prefix)
  (tree-template site-options-tree-template)
  (home site-options-home)
  (browse site-options-browse)
  (locale site-options-locale))

(define (unique-pipeline-names? pipelines)
  (let loop ((remaining pipelines) (seen '()))
    (or (null? remaining)
        (let ((name (asset-pipeline-name (car remaining))))
          (and (not (member name seen string=?))
               (loop (cdr remaining) (cons name seen)))))))

(define* (make-site-options #:key
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
                            (transforms '())
                            (renderers '())
                            (derived '())
                            (derived-context #f)
                            (pipelines '())
                            output-directory
                            (cache? #t)
                            (default-prefix #f)
                            (tree-template #f)
                            (home #f)
                            (browse #f)
                            (locale #f))
  "Build the immutable configuration for a site project.

READ-PAGE and OUTPUT-DIRECTORY are required."
  (unless (non-empty-string? title)
    (error "site title must be a non-empty string" title))
  (unless (or (not description) (non-empty-string? description))
    (error "site description must be a non-empty string or #f" description))
  (unless (and (or (not source-directory)
                   (non-empty-string? source-directory))
               (or (not collect-sources) (procedure? collect-sources))
               (or source-directory collect-sources))
    (error "site options require a source directory or collector"
           source-directory collect-sources))
  (unless (procedure? source-filter)
    (error "site source filter must be a procedure" source-filter))
  (unless (procedure? prepare)
    (error "site prepare step must be a procedure" prepare))
  (unless (and (list? extra-sources) (every string? extra-sources))
    (error "site extra sources must be a list of paths" extra-sources))
  (unless (procedure? read-page)
    (error "site options require a page reader" read-page))
  (unless (procedure? analyze)
    (error "site analyzer must be a procedure" analyze))
  (unless (and (list? transforms) (every procedure? transforms))
    (error "site transforms must be procedures" transforms))
  (unless (and (list? renderers)
               (every (lambda (renderer)
                        (or (procedure? renderer)
                            (cached-renderer? renderer)))
                      renderers))
    (error "site renderers must be procedures or cached renderers" renderers))
  (unless (and (list? derived) (every derived-artifact? derived))
    (error "site derived values must be derived artifacts" derived))
  (unless (or (not derived-context) (procedure? derived-context))
    (error "site derived context builder must be a procedure or #f"
           derived-context))
  (unless (and (list? pipelines)
               (every asset-pipeline? pipelines)
               (unique-pipeline-names? pipelines))
    (error "site pipelines must be asset pipelines with unique names"
           pipelines))
  (unless (non-empty-string? output-directory)
    (error "site options require an output directory" output-directory))
  (unless (boolean? cache?)
    (error "site cache option must be a boolean" cache?))
  (unless (or (not default-prefix) (non-empty-string? default-prefix))
    (error "site default prefix must be a non-empty string or #f"
           default-prefix))
  (unless (or (not tree-template) (procedure? tree-template))
    (error "site tree template must be a procedure or #f" tree-template))
  (unless (or (not home) (non-empty-string? home))
    (error "site home must be a non-empty page ID or #f" home))
  (unless (or (not browse) (procedure? browse))
    (error "site browser must be a procedure or #f" browse))
  (when locale
    (unless (string? locale)
      (error "site locale must be a tag or #f" locale))
    (make-locale locale))
  (%make-site-options title description source-directory source-filter
                      collect-sources prepare extra-sources read-page analyze
                      transforms renderers derived derived-context pipelines
                      output-directory cache? default-prefix tree-template
                      home browse locale))

(define* (make-site-project #:key options)
  (unless (site-options? options)
    (error "make-site-project requires site options" options))
  (let* ((title (site-options-title options))
         (description (site-options-description options))
         (source-directory (site-options-source-directory options))
         (source-filter (site-options-source-filter options))
         (collect-sources (site-options-collect-sources options))
         (prepare (site-options-prepare options))
         (extra-sources (site-options-extra-sources options))
         (read-page (site-options-read-page options))
         (analyze (site-options-analyze options))
         (transforms (site-options-transforms options))
         (renderers (site-options-renderers options))
         (derived (site-options-derived options))
         (derived-context (site-options-derived-context options))
         (pipelines (site-options-pipelines options))
         (output-directory (site-options-output-directory options))
         (cache? (site-options-cache? options))
         (default-prefix (site-options-default-prefix options))
         (tree-template (site-options-tree-template options))
         (home (site-options-home options))
         (browse (site-options-browse options))
         (locale (site-options-locale options)))
  (unless (procedure? read-page)
    (error "site project requires a page reader" read-page))
  (unless (procedure? analyze)
    (error "site project analyzer must be a procedure" analyze))
  (unless (every procedure? transforms)
    (error "site project transforms must be procedures" transforms))
  (unless (every derived-artifact? derived)
    (error "site project derived values must be derived artifacts" derived))
  (unless (or (not derived-context) (procedure? derived-context))
    (error "site project derived context builder must be a procedure or #f"
           derived-context))
  (let* ((locale-tag (and locale (locale-tag (make-locale locale))))
         (build-derived-context
           (or derived-context
               (lambda (forest)
                 (make-render-context #:forest forest))))
         (context-forest #f)
         (context-value #f)
         (context-for
           (lambda (forest)
             (if (and context-forest (eq? context-forest forest))
                 context-value
                 (let ((value (build-derived-context forest)))
                   (set! context-forest forest)
                   (set! context-value value)
                   value))))
         (derived-renderer
           (and (pair? derived)
                (make-cached-renderer
                  (lambda (forest)
                    (render-derived-artifacts derived
                                              (context-for forest)
                                              forest))
                  #:cache? cache?
                  #:configuration
                  (lambda (forest)
                    (append
                      (list 'derived (map derived-artifact-name derived))
                      (append-map
                        (lambda (artifact)
                          ((derived-artifact-configuration artifact)
                           (context-for forest)
                           forest))
                        derived)))))))
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
      #:renderers (if derived-renderer
                      (append renderers (list derived-renderer))
                      renderers)
      #:pipelines pipelines
      #:output-directory output-directory
      #:cache? cache?
      #:default-prefix default-prefix
      #:tree-template tree-template
      #:browse browse))))
