;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove string-fun)
  #:use-module (grove pipeline)
  #:use-module (grove private artifact-store)
  #:use-module (grove private cache-dir)
  #:use-module (grove private build-lock)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove private pipeline)
  #:use-module (grove private project state)
  #:use-module (grove build)
  #:use-module (grove private build-state)
  #:use-module (grove collect)
  #:use-module (grove output)
  #:use-module (grove project diagnostic)
  #:use-module (grove private snapshot)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (project?
            make-project
            project-source-directory
            project-tree-template
            project-default-prefix
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
            browse-project))

(define renderer-configurations
  (@@ (grove build) renderer-configurations))
(define cached-renderer?
  (@@ (grove build) cached-renderer?))
(define cached-renderer-cache?
  (@@ (grove build) cached-renderer-cache?))
(define cached-renderer-configuration
  (@@ (grove build) cached-renderer-configuration))
(define cached-renderer-procedure
  (@@ (grove build) cached-renderer-procedure))
(define cached-renderer-render-changed
  (@@ (grove build) cached-renderer-render-changed))
(define cached-renderer-inputs
  (@@ (grove build) cached-renderer-inputs))
(define asset-pipeline-name
  (@@ (grove pipeline) asset-pipeline-name))
(define asset-pipeline?
  (@@ (grove pipeline) asset-pipeline?))
(define asset-pipeline-inputs
  (@@ (grove pipeline) asset-pipeline-inputs))

(define-record-type <project-input>
  (make-project-input path value)
  project-input?
  (path project-input-path)
  (value project-input-value))

(define-record-type <project-context>
  (make-project-context inputs prepared-sources extra-sources)
  project-context?
  (inputs project-context-inputs)
  (prepared-sources project-context-prepared-sources)
  (extra-sources project-context-extra-sources))

(define-record-type <build-report>
  (%make-build-report sources artifacts added modified removed affected rendered?)
  build-report?
  (sources build-report-sources)
  (artifacts build-report-artifacts)
  (added build-report-added-sources)
  (modified build-report-modified-sources)
  (removed build-report-removed-sources)
  (affected build-report-affected-artifacts)
  (rendered? build-report-rendered?))

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
  ;;     New fields append at the end: compiled callers inline the
  ;;     slot positions of the fields they read, so inserting in the
  ;;     middle silently re-maps every later field for them.
  (%make-project source-directory source-filter collect-sources prepare
                 extra-sources read-source analyze renderers
                 output-directory cache? default-prefix tree-template browse
                 pipelines)
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
  (default-prefix project-default-prefix/raw)
  (tree-template project-tree-template)
  (browse project-browse)
  (pipelines project-pipelines))

(define (project-source-directory project)
  (snapshot-data (project-source-directory/raw project)))

(define (project-extra-sources project)
  (snapshot-data (project-extra-sources/raw project)))

(define (project-renderers project)
  (append (project-renderers/raw project) '()))

(define (project-output-directory project)
  (snapshot-data (project-output-directory/raw project)))

(define (project-default-prefix project)
  (snapshot-data (project-default-prefix/raw project)))

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
                       (default-prefix #f)
                       (tree-template #f)
                       (browse #f))
  (unless (and (or (not source-directory) (string? source-directory))
               (procedure? source-filter)
               (or (not collect-sources) (procedure? collect-sources))
               (or source-directory collect-sources))
    (error "project requires a source directory or collector"
           source-directory collect-sources))
  (when (and source-directory (not (non-empty-string? source-directory)))
    (error "project source directory must be a non-empty path"
           source-directory))
  (unless (procedure? prepare)
    (error "project prepare hook must be a procedure" prepare))
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
  (unless (or (not default-prefix) (non-empty-string? default-prefix))
    (error "project default prefix must be a non-empty string" default-prefix))
  (unless (or (not tree-template) (procedure? tree-template))
    (error "project tree template must be a procedure" tree-template))
  (unless (or (not browse) (procedure? browse))
    (error "project browse callback must be a procedure" browse))
  (%make-project (snapshot-data source-directory)
                 source-filter collect-sources prepare
                 (snapshot-data extra-sources) read-source analyze
                 (map (lambda (renderer)
                        (if (cached-renderer? renderer) renderer
                            (make-cached-renderer renderer))) renderers)
                 (snapshot-data output-directory) cache?
                 (snapshot-data default-prefix) tree-template browse
                 (snapshot-data pipelines)))

(define* (project-with-options project
                               #:key
                               (output-directory
                                 (project-output-directory project))
                               (extra-sources '())
                               (cache? (project-cache? project)))
  "Copy PROJECT with execution options and additional tracked source paths."
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
    #:default-prefix (project-default-prefix project)
    #:tree-template (project-tree-template project)
    #:browse (project-browse project)))

(define (read-source-with-context path read-source)
  (call-with-project-stage 'read path (lambda () (read-source path))))

(define (collect-project-sources project)
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
    paths))

(define (prepare-project-sources prepare)
  (let ((paths
          (call-with-project-stage 'prepare #f prepare)))
    (unless (and (list? paths)
                 (every (lambda (path)
                          (non-empty-string? path))
                        paths))
      (error "project prepare hook must return source paths" paths))
    (when (not (= (length paths) (length (delete-duplicates paths string=?))))
      (error "project prepare hook returned duplicate paths" paths))
    paths))

(define (source-paths entries)
  (map build-state-source-path entries))

(define (paths-intersect? paths candidates)
  (any (lambda (path) (member path candidates string=?)) paths))

(define (source-entry-by-path entries path)
  (find (lambda (entry)
          (string=? (build-state-source-path entry) path))
        entries))

(define (artifact-entry-by-path entries path)
  (find (lambda (entry)
          (string=? (build-state-artifact-path entry) path))
        entries))

(define (changed-sources old-sources current-sources)
  ;;     An absent file has no content and cannot shape the outputs.
  ;;     Project loads record load-path candidates that do not exist,
  ;;     and those candidate sets shift between build processes, so
  ;;     absent entries are dropped before the comparison instead of
  ;;     counting as additions and removals.  Runtime inputs (module
  ;;     files resolved through the load path) can additionally land
  ;;     under different absolute paths in different build processes
  ;;     without changing what the build produces, so a
  ;;     removed-and-re-added pair with the same content cancels.
  ;;     Declared project sources never cancel: a renamed page is a
  ;;     real change.
  (let* ((added
           (filter (lambda (entry)
                     (and (build-state-source-fingerprint entry)
                          (not (source-entry-by-path
                                 old-sources (build-state-source-path entry)))))
                   current-sources))
         (removed
           (filter (lambda (entry)
                     (and (build-state-source-fingerprint entry)
                          (not (source-entry-by-path
                                 current-sources (build-state-source-path entry)))))
                   old-sources))
         (modified
           (filter (lambda (entry)
                     (let ((previous
                             (source-entry-by-path
                               old-sources (build-state-source-path entry))))
                       (and previous
                            (not (equal? (build-state-source-fingerprint previous)
                                         (build-state-source-fingerprint
                                           entry))))))
                   current-sources))
         (churned
           (lset-intersection
             (lambda (left right)
               (equal? (build-state-source-fingerprint left)
                       (build-state-source-fingerprint right)))
             (filter (lambda (entry)
                       (eq? (build-state-source-kind entry) 'runtime))
                     added)
             (filter (lambda (entry)
                       (eq? (build-state-source-kind entry) 'runtime))
                     removed)))
         (churned-paths (source-paths churned))
         (added (filter (lambda (entry)
                          (not (member (build-state-source-path entry)
                                       churned-paths string=?)))
                        added))
         (removed (filter (lambda (entry)
                            (not (member (build-state-source-path entry)
                                         churned-paths string=?)))
                          removed)))
    (values (source-paths added)
            (source-paths modified)
            (source-paths removed))))

(define (artifact-affected? artifact previous changed)
  (let* ((path (artifact-path artifact))
         (old (artifact-entry-by-path previous path))
         (sources (artifact-sources artifact))
         (old-sources (and old (build-state-artifact-sources old))))
    (or (not old)
        (not (equal? (build-state-artifact-fingerprint old)
                     (content-fingerprint (artifact-content artifact))))
        (not (equal? sources old-sources))
        (not sources)
        (paths-intersect? (append (or sources '())
                                  (or old-sources '()))
                          changed))))

(define (affected-artifacts artifacts previous changed)
  (append
    (map artifact-path
         (filter (lambda (artifact)
                   (artifact-affected? artifact previous changed))
                 artifacts))
    (map build-state-artifact-path
         (filter (lambda (entry)
                   (not (find (lambda (artifact)
                                (string=? (artifact-path artifact)
                                          (build-state-artifact-path entry)))
                              artifacts)))
                 previous))))

(define (load-cached-artifacts directory entries)
  (let loop ((entries entries) (artifacts '()))
    (if (null? entries)
        (reverse artifacts)
        (let* ((entry (car entries))
               (path (string-append directory "/"
                                    (build-state-artifact-path entry)))
               ;;     The output file may be missing in a fresh
               ;;     checkout whose build directory was deleted: the
               ;;     shared store holds the bytes under the entry's
               ;;     fingerprint, so restore the file instead of
               ;;     paying for a re-render.
               (_ (when (and (not (file-exists? path))
                             (build-state-artifact-fingerprint entry))
                    (restore-artifact-blob!
                     (build-state-artifact-fingerprint entry) path)))
               (content
                 (and (file-exists? path)
                      (not (file-is-directory? path))
                      (catch #t
                        (lambda ()
                          (if (eq? (build-state-artifact-kind entry) 'bytes)
                              (let ((port (open-file path "rb")))
                                (dynamic-wind
                                  (lambda () #t)
                                  (lambda () (get-bytevector-all port))
                                  (lambda () (close-port port))))
                              (call-with-input-file path
                                (lambda (port)
                                  (set-port-encoding! port "UTF-8")
                                  (get-string-all port)))))
                        (lambda args #f)))))
          (if (and content
                   (equal? (content-fingerprint content)
                           (build-state-artifact-fingerprint entry)))
              (loop (cdr entries)
                    (cons ((if (eq? (build-state-artifact-kind entry) 'bytes)
                               make-byte-artifact make-artifact)
                           (build-state-artifact-path entry) content
                           (build-state-artifact-sources entry))
                          artifacts))
              #f)))))

(define (project-renderer-identity project model)
  (let ((configurations (renderer-configurations model (project-renderers project))))
    (and (project-cache? project) configurations
         (content-fingerprint
           (call-with-output-string
             (lambda (port) (write (list (version) (setlocale LC_CTYPE) configurations) port)))))))

(define (render-all model renderers progress)
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
        (let* ((start (get-internal-real-time))
               (generated (call-with-project-stage 'render #f
                            (lambda () (render-model model (list (car renderers))))))
               (seconds (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))
          (progress 'render-renderer (list index seconds (length generated)))
          (loop (cdr renderers)
                (+ index 1)
                (append artifacts generated)
                (append owners
                        (map (lambda (artifact)
                               (cons (artifact-path artifact) index))
                             generated)))))))

(define* (render-with-cache model renderers previous-entries cached-artifacts
                            previous-sources changed progress
                            #:key (model-retained? #f))
  ;;     MODEL-RETAINED? promises that MODEL is the same analysis the
  ;;     previous-entries were rendered from (the retained forest) and
  ;;     that renderer configurations are unchanged (identity checked
  ;;     by the caller).  render-model is then a pure function of
  ;;     unchanged inputs for every artifact whose recorded sources did
  ;;     not change, so a renderer without an incremental callback can
  ;;     keep its whole previous output instead of re-rendering it -
  ;;     but only when every one of its artifacts recorded sources:
  ;;     undeclared render-time file reads are not visible here, and
  ;;     such renderers conservatively re-run in full.
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
        (let* ((start (get-internal-real-time))
               (renderer (car renderers))
               (old-entries
                 (filter (lambda (entry)
                           (equal? (build-state-artifact-renderer entry)
                                   index))
                         previous-entries))
               (old-artifacts
                 (filter (lambda (artifact)
                           (and (artifact-entry-by-path
                                  old-entries (artifact-path artifact))
                                #t))
                         cached-artifacts))
               (changed-proc
                 (and (cached-renderer? renderer)
                      (cached-renderer-render-changed renderer)))
               (partial? (and changed-proc
                              (every (lambda (entry)
                                       (integer?
                                         (build-state-artifact-renderer entry)))
                                     old-entries)))
               (reusable?
                 (and model-retained?
                      (pair? old-entries)
                      (not partial?)
                      ;;     Every artifact must have recorded sources:
                      ;;     a renderer that reads files at render time
                      ;;     without declaring them is not a pure
                      ;;     function of the retained model, so its
                      ;;     output can only be trusted as unchanged
                      ;;     when its declared sources did not change.
                      (every build-state-artifact-sources old-entries)
                      (not (any (lambda (entry)
                                  (paths-intersect?
                                    (build-state-artifact-sources entry)
                                    changed))
                                old-entries))))
               (dirty-entries
                 (filter (lambda (entry)
                           (or (any (lambda (path)
                                       (not (source-entry-by-path
                                             previous-sources path)))
                                    changed)
                               (not (build-state-artifact-sources entry))
                               (paths-intersect?
                                 (build-state-artifact-sources entry) changed)))
                         old-entries))
               (dirty-paths (map car dirty-entries))
               (generated
                 (and partial?
                      (call-with-project-stage 'render #f
                        (lambda ()
                          (changed-proc model changed dirty-paths old-artifacts)))))
               (rendered-fully? (or (not partial?) (not generated)))
               (new-artifacts
                 (if reusable?
                     old-artifacts
                     (call-with-project-stage 'render #f
                       (lambda ()
                         (if rendered-fully?
                             (render-model model (list renderer))
                             (begin
                               (unless (and (list? generated) (every artifact? generated))
                                 (error "incremental renderer must return artifacts or #f"
                                        generated))
                               generated))))))
               ;; An incremental callback returns the complete current output
               ;; set for its renderer. This lets it preserve unaffected
               ;; artifacts from OLD-ARTIFACTS and omit outputs that vanished.
               (current new-artifacts)
               (current-owners
                 (map (lambda (artifact)
                        (cons (artifact-path artifact) index))
                      current))
               (seconds (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))
          (progress 'render-renderer
                    (list index seconds (length current)))
          (loop (cdr renderers)
                (+ index 1)
                (append artifacts current)
                (append owners current-owners))))))

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
                 (string-append (grove-store-directory) "/input-cache") (project-cache? project)
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
  (let ((current (map artifact-path pipeline-artifacts)))
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
         ;;     these files are read at project-load time (their content
         ;;     is baked into the style/hooks), so this is the baseline
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
                                  (current-project-key))))
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
             (delete-duplicates (append-map artifact-sources pipeline-artifacts)
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
          (identity (project-renderer-identity project model))
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
              (map artifact-path artifacts))
             ((and previous-state
                   (not (equal? identity previous-identity)))
              (append (map artifact-path artifacts)
                      (map car
                           (filter (lambda (entry)
                                     (not (find
                                            (lambda (artifact)
                                              (string=? (artifact-path artifact)
                                                        (build-state-artifact-path
                                                          entry)))
                                            artifacts)))
                                   previous-artifacts))))
             (else
              (affected-artifacts artifacts previous-artifacts changed)))))
    ;;     Expose the analysis model for long-lived preview workers:
    ;;     a resident worker re-renders from this forest when only
    ;;     load-time assets changed.
    (publish-project-state!
      model
      (let ((context (project-analysis-input-cache-context analysis)))
        (and context
             (lset-difference string=?
                              (input-cache-dependencies context)
                              (project-extra-sources project)))))
    (call-with-project-stage 'validate #f
      (lambda ()
        (progress 'validate #f)
        (validate-artifacts artifacts)))
    (when write?
      (call-with-project-stage 'write (project-output-directory project)
        (lambda ()
          (progress 'write (project-output-directory project))
          (write-artifacts (project-output-directory project) artifacts)
          (write-build-state state-path state-temp-path current-sources artifacts
                             owners identity)
          (prune-input-cache! input-cache-context)
          ;;     源文件可能在构建中途被改回（revert 正是如此）：重读
          ;;     pre-build 快照与 current-sources 的指纹并与写入时比对，
          ;;     漂移就删掉刚写的 state——下一次构建强制全量重渲染。
          (let ((drifted?
                  (any (lambda (entry)
                         (not (equal? (cadr entry)
                                      (input-fingerprint (car entry)))))
                       (append current-sources pre-build-sources))))
            (when drifted?
              (delete-file state-path))))))
    (%make-build-report (map build-state-source-path current-sources) artifacts added modified removed affected
                        (not cache-hit?))))

(define* (build-project/report project #:key (progress (lambda (stage path) #t)))
  "Build PROJECT, notifying PROGRESS with a stage symbol and detail.

The build takes the project's build lock: the artifact set, manifest,
and stale-output cleanup all live in the output directory, so a serve
watch and a manual build over one project must not run concurrently."
  (with-build-lock (project-output-directory project)
    (lambda () (execute-project project #t progress))))

;;     Forest-level reuse: a long-lived preview worker retains the
;;     forest of its last successful build and re-renders with a
;;     freshly loaded project when only load-time assets changed.
;;     Analysis is skipped wholesale; render-with-cache re-runs just
;;     the renderers whose inputs changed, and renderers without an
;;     incremental callback keep their previous artifacts outright -
;;     the retained forest and the identity-checked configurations
;;     make their output byte-identical.  The on-disk build state
;;     is refreshed (with content fingerprints) so manual builds stay
;;     incremental.  Cache pruning is skipped here - the retained
;;     state carries no selection statuses, and pruning is hygiene.
(define* (rebuild-from-forest project forest retained-state changed removed
                              #:key (progress (lambda (stage path) #t)))
  "Render and write using a FOREST built by a previous build of the
same PROJECT.  RETAINED-STATE is that build's on-disk state record;
CHANGED names the sources modified or added since, REMOVED the ones
deleted.  Renderers and style values come from the freshly loaded
PROJECT, so load-time assets (stylesheets, scripts) re-read, while
analysis results - pages, relations, indexes - come from the forest."
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
                                      (current-project-key))))
              (pipeline-artifacts (car pipeline-result))
              (pipeline-owners (cdr pipeline-result))
              (retired (retired-pipeline-paths pipeline-artifacts
                                               previous-artifacts))
              (identity (project-renderer-identity project forest))
             (config-equal?
                (equal? identity (build-state-identity retained-state)))
             ;;     Capture content before rendering: the retained forest
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
                      (render-with-cache forest (project-renderers project)
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
                      ;;     retained forest.
                      (render-all forest (project-renderers project) progress))))
              (artifacts (merge-pipeline-artifacts pipeline-artifacts
                                                   (car render-result) retired))
              (owners (append pipeline-owners (cadr render-result)))
             (affected (affected-artifacts artifacts previous-artifacts changed))
             (kept (map build-state-source-path current-sources))
             (report-removed
                (filter (lambda (p) (not (member p changed string=?))) removed)))
         (call-with-project-stage 'validate #f
           (lambda ()
             (progress 'validate #f)
             (validate-artifacts artifacts)))
         (call-with-project-stage 'write (project-output-directory project)
           (lambda ()
             (progress 'write (project-output-directory project))
             (write-artifacts (project-output-directory project) artifacts)
             (write-build-state state-path state-temp-path current-sources
                                artifacts owners identity)
             ;;     A source edited during the render is not in the
             ;;     retained forest; dropping the state forces the next
             ;;     build to analyze instead of trusting a cache hit.
             (let ((drifted?
                     (any (lambda (entry)
                            (not (equal? (cadr entry)
                                         (false-if-exception
                                           (file-fingerprint (car entry))))))
                          current-sources)))
               (when drifted?
                 (delete-file state-path)))))
          (%make-build-report
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
