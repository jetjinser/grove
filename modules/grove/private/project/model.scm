;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project model)
  #:use-module (grove private files)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private renderer)
  #:use-module (grove private procedure)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (project?
            make-project
            project-source-directory
            create-project-source
            project-renderers
            project-extra-sources
            project-output-directory
            project-cache?
            project-browse
            project-with-options
            project-input?
            project-input-path
            project-input-value
            project-context?
            project-context-inputs
            project-context-prepared-sources
            project-context-extra-sources
            build-report?
            build-report-sources
            build-report-artifacts
            build-report-added-sources
            build-report-modified-sources
            build-report-removed-sources
            build-report-affected-artifacts
            build-report-render-cache-hit?
            ;; Private constructors and analysis record used by lifecycle.
            make-project-input
            make-project-context
            make-build-report
            make-project-analysis
            project-analysis-source-paths
            project-analysis-prepared-sources
            project-analysis-model
            project-analysis-trace
            project-analysis-input-cache-context
            project-source-filter
            project-collect-sources
            project-prepare
            project-read-source
            project-analyze
            project-pipelines
            project-create-source-procedure
            default-project-source-filter))

(define (default-project-source-filter filename)
  (not (string-prefix? "." filename)))

(define-record-type <project-input>
  (%make-project-input path value)
  project-input?
  (path project-input-path/raw)
  (value project-input-value/raw))

(define (make-project-input path value)
  (%make-project-input (snapshot-data path) (snapshot-data value)))

(define (project-input-path input)
  (snapshot-data (project-input-path/raw input)))

(define (project-input-value input)
  (snapshot-data (project-input-value/raw input)))

(define-record-type <project-context>
  (%make-project-context inputs prepared-sources extra-sources)
  project-context?
  (inputs project-context-inputs/raw)
  (prepared-sources project-context-prepared-sources/raw)
  (extra-sources project-context-extra-sources/raw))

(define (make-project-context inputs prepared-sources extra-sources)
  (%make-project-context (snapshot-data inputs)
                         (snapshot-data prepared-sources)
                         (snapshot-data extra-sources)))

(define (project-context-inputs context)
  (snapshot-data (project-context-inputs/raw context)))

(define (project-context-prepared-sources context)
  (snapshot-data (project-context-prepared-sources/raw context)))

(define (project-context-extra-sources context)
  (snapshot-data (project-context-extra-sources/raw context)))

(define-record-type <build-report>
  (%make-build-report sources artifacts added modified removed affected
                      render-cache-hit?)
  build-report?
  (sources build-report-sources/raw)
  (artifacts build-report-artifacts/raw)
  (added build-report-added-sources/raw)
  (modified build-report-modified-sources/raw)
  (removed build-report-removed-sources/raw)
  (affected build-report-affected-artifacts/raw)
  (render-cache-hit? build-report-render-cache-hit?))

(define (make-build-report sources artifacts added modified removed affected
                           render-cache-hit?)
  (%make-build-report (snapshot-data sources)
                      (snapshot-data artifacts)
                      (snapshot-data added)
                      (snapshot-data modified)
                      (snapshot-data removed)
                      (snapshot-data affected)
                      render-cache-hit?))

(define (build-report-sources report)
  (snapshot-data (build-report-sources/raw report)))

(define (build-report-artifacts report)
  (snapshot-data (build-report-artifacts/raw report)))

(define (build-report-added-sources report)
  (snapshot-data (build-report-added-sources/raw report)))

(define (build-report-modified-sources report)
  (snapshot-data (build-report-modified-sources/raw report)))

(define (build-report-removed-sources report)
  (snapshot-data (build-report-removed-sources/raw report)))

(define (build-report-affected-artifacts report)
  "Return output paths Grove considers changed, invalidated, or removed.

This is not a record of renderer calls or files written. An uncached renderer
may run and produce the same artifact without listing it here."
  (snapshot-data (build-report-affected-artifacts/raw report)))

(define-record-type <project-analysis>
  (%make-project-analysis source-paths prepared-sources model trace
                          input-cache-context)
  project-analysis?
  (source-paths project-analysis-source-paths)
  (prepared-sources project-analysis-prepared-sources)
  (model project-analysis-model)
  (trace project-analysis-trace)
  (input-cache-context project-analysis-input-cache-context))

(define (make-project-analysis source-paths prepared-sources model trace
                               input-cache-context)
  (%make-project-analysis source-paths prepared-sources model trace
                          input-cache-context))

(define-record-type <project>
  (%make-project source-directory source-filter collect-sources prepare
                 extra-sources read-source analyze renderers
                 output-directory cache? browse pipelines create-source)
  project?
  (source-directory project-source-directory/raw)
  (source-filter project-source-filter)
  (collect-sources project-collect-sources)
  (prepare project-prepare)
  (extra-sources project-extra-sources/raw)
  (read-source project-read-source)
  (analyze project-analyze)
  (renderers project-renderers/raw)
  (output-directory project-output-directory/raw)
  (cache? project-cache?)
  (browse project-browse)
  (pipelines project-pipelines)
  (create-source project-create-source-procedure))

(define (project-source-directory project)
  (snapshot-data (project-source-directory/raw project)))

(define (project-extra-sources project)
  (snapshot-data (project-extra-sources/raw project)))

(define (project-renderers project)
  (append (project-renderers/raw project) '()))

(define* (create-project-source project #:key prefix
                                (destination
                                  (and (project? project)
                                       (project-source-directory project))))
  "Create a source through PROJECT's configured creator.

PREFIX and DESTINATION are passed to the project-owned procedure. DESTINATION
defaults to the project's source directory."
  (unless (project? project)
    (error "source creation requires a Grove project" project))
  (let ((creator (project-create-source-procedure project)))
    (unless creator
      (error "this project does not define how to create a source file"))
    (let ((path (creator prefix destination)))
      (unless (filesystem-path-string? path)
        (error "project source creator must return a non-empty path" path))
      (snapshot-data path))))

(define (project-output-directory project)
  (snapshot-data (project-output-directory/raw project)))
(define (unique-pipeline-names? pipelines)
  (let loop ((remaining pipelines) (seen '()))
    (or (null? remaining)
        (let ((name (asset-pipeline-name (car remaining))))
          (and (not (member name seen string=?))
               (loop (cdr remaining) (cons name seen)))))))

(define* (make-project #:key
                       (source-directory #f)
                       (source-filter default-project-source-filter)
                       (collect-sources #f)
                       (prepare (lambda () '()))
                       (extra-sources '())
                       read-source
                       (analyze (lambda (context) context))
                       (renderers '())
                       (pipelines '())
                       output-directory
                       (cache? #t)
                       (browse #f)
                       (create-source #f))
  "Construct a project build lifecycle.

Provide SOURCE-DIRECTORY or COLLECT-SOURCES, READ-SOURCE, and
OUTPUT-DIRECTORY. When COLLECT-SOURCES is absent, Grove recursively collects
regular files from SOURCE-DIRECTORY using SOURCE-FILTER on each basename.
When supplied, COLLECT-SOURCES determines the ordered source path list instead;
SOURCE-DIRECTORY may still be supplied as the default destination for
CREATE-PROJECT-SOURCE. PREPARE returns paths tracked before reading; they are
not passed to READ-SOURCE. EXTRA-SOURCES declares fixed project dependencies;
they are tracked and exposed in the project context, but are not collected or
read as content. ANALYZE receives a project context;
each RENDERER receives the analyzed model and returns artifacts. PIPELINES
derive artifacts from declared files independently of the model. CACHE? controls
document, renderer, and pipeline cache use. Plain renderer procedures run on
every build; wrap one with MAKE-CACHED-RENDERER only when its stable cache
identity is declared. BROWSE, when supplied, is called with the analyzed model,
input port, and output port by BROWSE-PROJECT."
  (unless (or (not source-directory)
              (filesystem-path-string? source-directory))
    (error "project source directory must be a non-empty path or #f"
           source-directory))
  (unless (or (not collect-sources)
              (procedure-accepts-arguments? collect-sources 0))
    (error "project source collector must accept zero arguments or be #f"
           collect-sources))
  (unless (or source-directory collect-sources)
    (error "project requires a source directory or collector"
           source-directory collect-sources))
  (unless (or collect-sources
              (procedure-accepts-arguments? source-filter 1))
    (error "project source filter must accept one basename when using directory collection"
           source-filter))
  (unless (procedure-accepts-arguments? prepare 0)
    (error "project preparation step must accept zero arguments" prepare))
  (unless (filesystem-path-list? extra-sources)
    (error "project extra sources must be a list of paths" extra-sources))
  (unless (unique-filesystem-path-list? extra-sources)
    (error "project extra source paths must be unique" extra-sources))
  (unless (procedure-accepts-arguments? read-source 1)
    (error "project source reader must accept one argument" read-source))
  (unless (procedure-accepts-arguments? analyze 1)
    (error "project analyzer must accept one argument" analyze))
  (unless (renderer-list? renderers)
    (error "project renderers must accept one argument or be cached renderers"
           renderers))
  (unless (and (list? pipelines)
               (every asset-pipeline? pipelines)
               (unique-pipeline-names? pipelines))
    (error "project pipelines must be asset pipelines with unique names"
           pipelines))
  (unless (filesystem-path-string? output-directory)
    (error "project requires a non-empty output directory" output-directory))
  (unless (boolean? cache?)
    (error "project cache option must be a boolean" cache?))
  (unless (or (not browse) (procedure-accepts-arguments? browse 3))
    (error "project browse callback must accept the model, input port, and output port, or be #f"
           browse))
  (unless (or (not create-source)
              (procedure-accepts-arguments? create-source 2))
    (error "project source creator must accept a prefix and destination or be #f"
           create-source))
  (%make-project (snapshot-data source-directory)
                 source-filter collect-sources prepare
                 (snapshot-data extra-sources) read-source analyze
                 (append renderers '())
                 (snapshot-data output-directory) cache? browse
                 (snapshot-data pipelines) create-source))

(define* (project-with-options project
                               #:key
                               (output-directory
                                 (and (project? project)
                                      (project-output-directory project)))
                               (extra-sources '())
                               (cache? (and (project? project)
                                            (project-cache? project))))
  "Copy PROJECT with execution options and additional tracked source paths."
  (unless (project? project)
    (error "project options require a Grove project" project))
  (unless (filesystem-path-list? extra-sources)
    (error "project additional sources must be a list of non-empty paths"
           extra-sources))
  (make-project
    #:source-directory (project-source-directory project)
    #:source-filter (project-source-filter project)
    #:collect-sources (project-collect-sources project)
    #:prepare (project-prepare project)
    #:extra-sources (delete-duplicates
                     (append (project-extra-sources project) extra-sources)
                     string=?)
    #:read-source (project-read-source project)
    #:analyze (project-analyze project)
    #:renderers (project-renderers project)
    #:pipelines (project-pipelines project)
    #:output-directory output-directory
    #:cache? cache?
    #:browse (project-browse project)
    #:create-source (project-create-source-procedure project)))
