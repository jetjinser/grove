;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project lifecycle)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private string-fun)
  #:use-module (grove pipeline)
  #:use-module (grove private cache-dir)
  #:use-module (grove private build-lock)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove private pipeline)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private project artifacts)
  #:use-module (grove private project publication)
  #:use-module (grove private project renderers)
  #:use-module (grove private project state)
  #:use-module (grove private renderer)
  #:use-module (grove build)
  #:use-module (grove private build-state)
  #:use-module (grove collect)
  #:use-module (grove project diagnostic)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (project?
            make-project
            project-source-directory
            create-project-source
            project-renderers
            collect-project-sources
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
            build-report-rendered?
            build-project/report
            build-project
            check-project
            analyze-project
            browse-project
            rebuild-from-model))

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
  (%make-build-report sources artifacts added modified removed affected rendered?)
  build-report?
  (sources build-report-sources/raw)
  (artifacts build-report-artifacts/raw)
  (added build-report-added-sources/raw)
  (modified build-report-modified-sources/raw)
  (removed build-report-removed-sources/raw)
  (affected build-report-affected-artifacts/raw)
  (rendered? build-report-rendered?))

(define (make-build-report sources artifacts added modified removed affected rendered?)
  (%make-build-report (snapshot-data sources)
                      (snapshot-data artifacts)
                      (snapshot-data added)
                      (snapshot-data modified)
                      (snapshot-data removed)
                      (snapshot-data affected)
                      rendered?))

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
  (%make-project-analysis source-paths prepared-sources inputs context
                          model trace input-cache-context)
  project-analysis?
  (source-paths project-analysis-source-paths)
  (prepared-sources project-analysis-prepared-sources)
  (inputs project-analysis-inputs)
  (context project-analysis-context)
  (model project-analysis-model)
  (trace project-analysis-trace)
  (input-cache-context project-analysis-input-cache-context))

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
                       (source-filter (lambda (filename)
                                        (not (string-prefix? "." filename))))
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

(define (read-source-with-context path read-source)
  (call-with-project-stage 'read path (lambda () (read-source path))))

(define (collect-project-sources project)
  "Return PROJECT's validated source paths as independent data."
  (let ((paths
          ((or (project-collect-sources project)
               (lambda ()
                 (collect-directory (project-source-directory project)
                                    (project-source-filter project)))))))
    (unless (and (list? paths)
                 (every (lambda (path)
                          (non-empty-string? path))
                        paths))
      (error "project collector must return a list of source paths" paths))
    (when (not (= (length paths) (length (delete-duplicates paths string=?))))
      (error "project collector returned duplicate paths" paths))
    (snapshot-data paths)))

(define (prepare-project-sources prepare)
  (let ((paths
          (call-with-project-stage 'prepare #f prepare)))
    (unless (and (list? paths)
                 (every (lambda (path)
                          (non-empty-string? path))
                        paths))
      (error "project preparation must return source paths" paths))
    (when (not (= (length paths) (length (delete-duplicates paths string=?))))
      (error "project preparation returned duplicate paths" paths))
    (snapshot-data paths)))

(define* (analyze-project/result project write?
                                #:key (progress (lambda (stage path) #t)))
  "Collect, read, and analyze PROJECT without rendering or writing outputs.
Return the internal analysis record used by the build pipeline."
  (unless (project? project)
    (error "project analysis requires a Grove project" project))
  (parameterize ((current-input-fingerprints (make-hash-table)))
    (let* ((source-paths (call-with-project-stage 'collect #f
                           (lambda ()
                             (progress 'collect #f)
                             (let ((paths (collect-project-sources project)))
                               (progress 'sources (length paths))
                               paths))))
           (prepared-sources (begin
                               (progress 'prepare #f)
                               (prepare-project-sources (project-prepare project))))
           (input-cache-context
             (and write?
               (make-input-cache-context
                 (project-cache? project)
                 (append prepared-sources (project-extra-sources project)
                         (runtime-inputs)))))
           (inputs
             (parameterize ((current-input-cache input-cache-context))
               (map (lambda (path)
                      (progress 'read path)
                      (let ((value (read-source-with-context path
                                     (project-read-source project))))
                        (progress 'read-done path)
                        (make-project-input path value)))
                    source-paths)))
           (_ (let ((summary (input-cache-summary input-cache-context)))
                (when (> (cdr summary) 0) (progress 'input-cache summary))))
           (context
             (make-project-context inputs prepared-sources
                                   (project-extra-sources project)))
           (trace (make-hash-table))
           (model (call-with-project-stage 'analyze #f
                    (lambda ()
                      (progress 'analyze #f)
                      (call-with-input-trace trace
                        (lambda () ((project-analyze project) context))))))
           (_ (register-input-dependencies! input-cache-context
                                            (trace-inputs trace))))
      (%make-project-analysis source-paths prepared-sources inputs context
                              model trace input-cache-context))))

(define* (analyze-project project #:key (progress (lambda (stage path) #t)))
  "Return PROJECT's analyzed model without rendering or writing outputs."
  (project-analysis-model
    (analyze-project/result project #f #:progress progress)))

(define* (browse-project project #:key (progress (lambda (stage path) #t))
                         (input (current-input-port))
                         (output (current-output-port)))
  "Analyze PROJECT without rendering and run its browse callback on the model."
  (let ((browse (project-browse project)))
    (unless (procedure? browse)
      (error "project does not define a #:browse callback"))
    (let ((model (analyze-project project #:progress progress)))
      (browse model #:input input #:output output))))

;;     Pipeline outputs that vanished from a pipeline's current set
;;     must not resurrect from the previous build's state: previous
;;     entries a pipeline owns (string renderer marker) whose paths the
;;     current run no longer emits are retired.
(define (retired-pipeline-paths pipeline-artifacts previous-artifacts)
  (let ((current (map out:artifact-path/raw pipeline-artifacts)))
    (filter-map
      (lambda (entry)
        (let ((path (build-state-artifact-path entry))
              (owner (build-state-artifact-renderer entry)))
          (and (string? owner)
               (not (member path current string=?))
               path)))
      previous-artifacts)))

(define (execute-project project write? progress)
  (unless (project? project)
    (error "project execution requires a Grove project" project))
  (let* (;;     Capture extra-sources fingerprints before anything runs:
         ;;     these files are read at project-load time and may define
         ;;     configuration values or procedures used by later stages,
         ;;     so this is the baseline
         ;;     the end-of-build drift check must compare against.
         (pre-build-sources
           (map (lambda (path) (list path (input-fingerprint path)))
                (delete-duplicates
                  (project-extra-sources project)
                  string=?)))
         (analysis (analyze-project/result project write? #:progress progress))
         (source-paths (project-analysis-source-paths analysis))
         (prepared-sources (project-analysis-prepared-sources analysis))
         (inputs (project-analysis-inputs analysis))
         (context (project-analysis-context analysis))
         (analysis-trace (project-analysis-trace analysis))
         (model (project-analysis-model analysis))
         (input-cache-context (project-analysis-input-cache-context analysis))
         (pipeline-result
           (parameterize ((current-input-fingerprints (make-hash-table)))
             (run-asset-pipelines (project-pipelines project)
                                  (current-project-key)
                                  #:cache? (and write?
                                                (project-cache? project)))))
         (pipeline-artifacts (car pipeline-result))
         (pipeline-owners (cdr pipeline-result))
         (pipeline-declared-sources
           (delete-duplicates
             (append-map
               (lambda (pipeline)
                 (map depend-on! (asset-pipeline-inputs pipeline)))
               (project-pipelines project))
             string=?))
         (pipeline-runtime-sources
           (lset-difference
             string=?
             (delete-duplicates (append-map out:artifact-sources/raw pipeline-artifacts)
                                string=?)
             pipeline-declared-sources))
         (state-directory
           (grove-cache-directory (project-output-directory project)))
         (state-path
           (string-append state-directory "/" build-state-name))
         (state-temp-path
           (string-append state-directory "/" build-state-temp-name))
         (previous-state (and write? (read-build-state state-path)))
         (previous-sources (if previous-state
                               (build-state-sources previous-state) '()))
         (previous-artifacts (if previous-state
                                 (build-state-artifacts previous-state) '()))
         (previous-identity (and previous-state
                                  (build-state-identity previous-state)))
         (declared-sources
           (delete-duplicates
             (append prepared-sources source-paths
                     pipeline-declared-sources
                     (append-map
                       (lambda (renderer)
                         (if (and (cached-renderer? renderer)
                                  (cached-renderer-inputs renderer))
                             ((cached-renderer-inputs renderer))
                             '()))
                       (project-renderers project))
                     (project-extra-sources project))
             string=?))
         ;;     Files tracked implicitly through reads and module loads
         ;;     (the input cache's dependency set) sit outside the
         ;;     project's own declarations.  Module files in particular
         ;;     resolve to different absolute paths in different build
         ;;     processes, so they are compared by content, not path.
         (runtime-sources
           (lset-difference string=?
                            (delete-duplicates
                              (append
                                (input-cache-dependencies input-cache-context)
                                pipeline-runtime-sources)
                              string=?)
                            declared-sources))
         (current-sources
           (append
             (map (lambda (path)
                    (list path (input-fingerprint path) 'source))
                  declared-sources)
             (map (lambda (path)
                    (list path (input-fingerprint path) 'runtime))
                  runtime-sources)))
         (source-changes
           (call-with-values
             (lambda () (changed-sources previous-sources current-sources))
             list))
         (added (car source-changes))
         (modified (cadr source-changes))
         (removed (caddr source-changes))
         (changed (append added modified removed))
          (identity (renderer-identity (project-cache? project) model
                                       (project-renderers project)))
          (retired (retired-pipeline-paths pipeline-artifacts previous-artifacts))
          (cache-ready?
           (and identity previous-state
                (equal? identity previous-identity)
                (load-cached-artifacts (project-output-directory project)
                                       previous-artifacts)))
         (global-inputs-changed?
           (or (input-cache-disabled? input-cache-context)
               (hash-ref analysis-trace 'uncached)
               (paths-intersect? (input-cache-dependencies input-cache-context) changed)
               (and (> (cdr (input-cache-summary input-cache-context)) 0)
                    (any (lambda (path) (not (file-exists? path))) removed))))
         (cache-hit? (and cache-ready? (null? changed) (not global-inputs-changed?)))
         (render-result
           (cond
             (cache-hit?
              (list cache-ready?
                    (map (lambda (entry)
                           (cons (build-state-artifact-path entry)
                                 (build-state-artifact-renderer entry)))
                         previous-artifacts)
                    '()))
             (global-inputs-changed?
              (progress 'render #f)
              (render-all model (project-renderers project) progress))
             (cache-ready?
              (progress 'render #f)
              (render-with-cache model (project-renderers project)
                                 previous-artifacts cache-ready?
                                 previous-sources changed progress))
             (else
              (progress 'render #f)
              (render-all model (project-renderers project) progress))))
          (artifacts (merge-pipeline-artifacts pipeline-artifacts
                                               (car render-result) retired))
          (owners (append pipeline-owners (cadr render-result)))
         (affected
           (cond
             (cache-hit? '())
             ((and identity previous-state
                   (equal? identity previous-identity)
                   (not cache-ready?))
              (map out:artifact-path/raw artifacts))
             ((and previous-state
                   (not (equal? identity previous-identity)))
              (append (map out:artifact-path/raw artifacts)
                      (map car
                           (filter (lambda (entry)
                                     (not (find
                                            (lambda (artifact)
                                              (string=? (out:artifact-path/raw artifact)
                                                        (build-state-artifact-path
                                                          entry)))
                                            artifacts)))
                                   previous-artifacts))))
             (else
              (affected-artifacts artifacts previous-artifacts changed)))))
    ;;     Expose the analysis model for long-lived preview workers:
    ;;     a resident worker re-renders from this model when only
    ;;     load-time assets changed.
    (publish-project-state!
      model
      (let ((context (project-analysis-input-cache-context analysis)))
        (and context
             (lset-difference string=?
                              (input-cache-dependencies context)
                              (project-extra-sources project)))))
    (validate-project-artifacts artifacts progress)
    (when write?
      (write-project-build!
        (project-output-directory project) state-path state-temp-path
        current-sources artifacts owners identity progress
        #:earlier-sources pre-build-sources
        #:prune (lambda () (prune-input-cache! input-cache-context))))
    (make-build-report (map build-state-source-path current-sources) artifacts added modified removed affected
                       (not cache-hit?))))

(define* (build-project/report project #:key (progress (lambda (stage path) #t)))
  "Build PROJECT, notifying PROGRESS with a stage symbol and detail.

The build takes the project's build lock: the artifact set, manifest,
and stale-output cleanup all live in the output directory, so a serve
watch and a manual build over one project must not run concurrently."
  (with-build-lock (project-output-directory project)
    (lambda () (execute-project project #t progress))))

;;     Model-level reuse: a long-lived preview worker retains the
;;     model of its last successful build and re-renders with a
;;     freshly loaded project when only load-time assets changed.
;;     Analysis is skipped wholesale; render-with-cache re-runs just
;;     the renderers whose inputs changed, and renderers without an
;;     incremental callback keep their previous artifacts outright -
;;     the retained model and the identity-checked configurations
;;     make their output byte-identical.  The on-disk build state
;;     is refreshed (with content fingerprints) so manual builds stay
;;     incremental.  Cache pruning is skipped here - the retained
;;     state carries no selection statuses, and pruning is hygiene.
(define* (rebuild-from-model project model retained-state changed removed
                              #:key (progress (lambda (stage path) #t)))
  "Render and write using a MODEL built by a previous analysis of PROJECT.
RETAINED-STATE is that build's on-disk state record; CHANGED names sources
modified or added since, and REMOVED names deleted sources. Renderers and
project configuration come from the freshly loaded PROJECT, while MODEL
preserves the prior analysis result."
  (with-build-lock (project-output-directory project)
    (lambda ()
      (let* ((cache-directory
                (grove-cache-directory (project-output-directory project)))
             (state-path
                (string-append cache-directory "/" build-state-name))
             (state-temp-path
                (string-append cache-directory "/" build-state-temp-name))
              (previous-sources (build-state-sources retained-state))
              (previous-artifacts (build-state-artifacts retained-state))
              (pipeline-result
               (parameterize ((current-input-fingerprints (make-hash-table)))
                 (run-asset-pipelines (project-pipelines project)
                                      (current-project-key)
                                      #:cache? (project-cache? project))))
              (pipeline-artifacts (car pipeline-result))
              (pipeline-owners (cdr pipeline-result))
              (retired (retired-pipeline-paths pipeline-artifacts
                                               previous-artifacts))
              (identity (renderer-identity (project-cache? project) model
                                           (project-renderers project)))
             (config-equal?
                (equal? identity (build-state-identity retained-state)))
             ;;     Capture content before rendering: the retained model
             ;;     reflects these bytes, and a source edited during the
             ;;     render must not enter the state as if the render had
             ;;     seen it.
             (current-sources
                (filter-map
                  (lambda (source)
                    (let ((digest (false-if-exception
                                    (file-fingerprint
                                      (build-state-source-path source)))))
                      (and digest
                           (list (build-state-source-path source)
                                 digest
                                 (build-state-source-kind source)))))
                  previous-sources))
             (render-result
                (begin
                  (progress 'render #f)
                  (if config-equal?
                      (render-with-cache model (project-renderers project)
                                         previous-artifacts
                                         (load-cached-artifacts
                                           (project-output-directory project)
                                           previous-artifacts)
                                         previous-sources changed progress
                                         #:model-retained? #t)
                      ;;     A configuration baked an extra-source value
                      ;;     that just changed: every artifact may differ,
                      ;;     so render everything with the fresh configs
                      ;;     - but the analysis still comes from the
                      ;;     retained model.
                      (render-all model (project-renderers project) progress))))
              (artifacts (merge-pipeline-artifacts pipeline-artifacts
                                                   (car render-result) retired))
              (owners (append pipeline-owners (cadr render-result)))
             (affected (affected-artifacts artifacts previous-artifacts changed))
             (report-removed
                (filter (lambda (p) (not (member p changed string=?))) removed)))
         (validate-project-artifacts artifacts progress)
         (write-project-build!
           (project-output-directory project) state-path state-temp-path
           current-sources artifacts owners identity progress
           #:fingerprint
           (lambda (path)
             (false-if-exception (file-fingerprint path))))
          (make-build-report
            (map build-state-source-path current-sources)
            artifacts
            '()
            changed
            report-removed
            affected
            #t)))))

(define* (check-project project #:key (progress (lambda (stage path) #t)))
  "Read, analyze, render, and validate PROJECT without accessing output state."
  (execute-project project #f progress))

(define* (build-project project #:key (progress (lambda (stage path) #t)))
  (build-report-artifacts (build-project/report project #:progress progress)))
