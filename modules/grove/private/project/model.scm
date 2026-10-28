;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project model)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private renderer)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
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
                                (destination (project-source-directory project)))
  "Create a source through PROJECT's configured creator.

PREFIX and DESTINATION are passed to the project-owned procedure. DESTINATION
defaults to the project's source directory."
  (let ((creator (project-create-source-procedure project)))
    (unless creator
      (error "this project does not define how to create a source file"))
    (creator prefix destination)))

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
CREATE-PROJECT-SOURCE. PREPARE returns additional tracked paths before reading;
ANALYZE receives a project context;
each RENDERER receives the analyzed model and returns artifacts. PIPELINES
derive artifacts from declared files independently of the model. CACHE? controls
document, renderer, and pipeline cache use. Plain renderer procedures run on
every build; wrap one with MAKE-CACHED-RENDERER only when its stable cache
identity is declared."
  (unless (or (not source-directory) (string? source-directory))
    (error "project source directory must be a path or #f" source-directory))
  (when (and source-directory (not (non-empty-string? source-directory)))
    (error "project source directory must be a non-empty path"
           source-directory))
  (unless (or (not collect-sources) (procedure? collect-sources))
    (error "project source collector must be a procedure or #f"
           collect-sources))
  (unless (or source-directory collect-sources)
    (error "project requires a source directory or collector"
           source-directory collect-sources))
  (unless (or collect-sources (procedure? source-filter))
    (error "project source filter must be a procedure when using directory collection"
           source-filter))
  (unless (procedure? prepare)
    (error "project preparation step must be a procedure" prepare))
  (unless (and (list? extra-sources)
               (every non-empty-string? extra-sources))
    (error "project extra sources must be a list of paths" extra-sources))
  (unless (= (length extra-sources)
             (length (delete-duplicates extra-sources string=?)))
    (error "project extra source paths must be unique" extra-sources))
  (unless (procedure? read-source)
    (error "project requires a source reader" read-source))
  (unless (procedure? analyze)
    (error "project analyzer must be a procedure" analyze))
  (unless (and (list? renderers)
               (every (lambda (renderer)
                        (or (procedure? renderer)
                            (cached-renderer? renderer)))
                      renderers))
    (error "project renderers must be procedures or cached renderers"
           renderers))
  (unless (and (list? pipelines)
               (every asset-pipeline? pipelines)
               (unique-pipeline-names? pipelines))
    (error "project pipelines must be asset pipelines with unique names"
           pipelines))
  (unless (non-empty-string? output-directory)
    (error "project requires a non-empty output directory" output-directory))
  (unless (boolean? cache?)
    (error "project cache option must be a boolean" cache?))
  (unless (or (not browse) (procedure? browse))
    (error "project browse callback must be a procedure" browse))
  (unless (or (not create-source) (procedure? create-source))
    (error "project source creator must be a procedure or #f" create-source))
  (%make-project (snapshot-data source-directory)
                 source-filter collect-sources prepare
                 (snapshot-data extra-sources) read-source analyze
                 (append renderers '())
                 (snapshot-data output-directory) cache? browse
                 (snapshot-data pipelines) create-source))

(define* (project-with-options project
                               #:key
                               (output-directory
                                 (project-output-directory project))
                               (extra-sources '())
                               (cache? (project-cache? project)))
  "Copy PROJECT with execution options and additional tracked source paths."
  (unless (and (list? extra-sources)
               (every non-empty-string? extra-sources))
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
