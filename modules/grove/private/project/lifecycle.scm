;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project lifecycle)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private project model)
  #:use-module (grove private files)
  #:use-module (grove private cache-dir)
  #:use-module (grove private build-lock)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove private pipeline)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove private renderer)
  #:use-module (grove private project artifacts)
  #:use-module (grove private project incremental)
  #:use-module (grove private project publication)
  #:use-module (grove private project renderers)
  #:use-module (grove private build-state)
  #:use-module (grove collect)
  #:use-module (grove project diagnostic)
  #:use-module (grove private snapshot)
  #:use-module (grove private procedure)
  #:use-module (srfi srfi-1)
  #:export (collect-project-sources
            build-project/report
            build-project/report+analysis
            build-project
            check-project
            analyze-project
            browse-project))

(define (require-project project)
  (unless (project? project)
    (error "expected a Grove project" project)))

(define (require-progress progress)
  (unless (procedure-accepts-arguments? progress 2)
    (error "project progress callback must accept a stage and value" progress)))

(define (read-source-with-context path read-source)
  (call-with-project-stage 'read path (lambda () (read-source path))))

(define (snapshot-unique-paths paths who)
  (unless (filesystem-path-list? paths)
    (error "~a must be a list of non-empty paths" who paths))
  (unless (unique-filesystem-path-list? paths)
    (error "~a contains duplicate paths" who paths))
  (snapshot-data paths))

(define (renderer-dependency-paths renderer)
  (let* ((dependency-spec (cached-renderer-dependencies renderer))
         (paths (cond ((not dependency-spec) '())
                      ((procedure? dependency-spec) (dependency-spec))
                      (else dependency-spec))))
    (snapshot-unique-paths paths "renderer dependencies")))

(define (collect-project-sources project)
  "Return PROJECT's validated source paths as independent data."
  (require-project project)
  (let ((paths
          ((or (project-collect-sources project)
               (lambda ()
                 (collect-directory (project-source-directory project)
                                    (project-source-filter project)))))))
    (snapshot-unique-paths paths "project source collector result")))

(define (prepare-project-sources prepare)
  (let ((paths
          (call-with-project-stage 'prepare #f prepare)))
    (snapshot-unique-paths paths "project preparation result")))

(define* (analyze-project/result project write?
                                #:key (progress (lambda (stage path) #t)))
  "Collect, read, and analyze PROJECT without rendering or writing outputs.
Return the internal analysis record used by the build pipeline."
  (require-project project)
  (require-progress progress)
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
      (make-project-analysis source-paths prepared-sources model trace
                             input-cache-context))))

(define* (analyze-project project #:key (progress (lambda (stage path) #t)))
  "Return PROJECT's analyzed model without rendering or writing outputs."
  (project-analysis-model
    (analyze-project/result project #f #:progress progress)))

(define* (browse-project project #:key (progress (lambda (stage path) #t))
                         (input (current-input-port))
                         (output (current-output-port)))
  "Analyze PROJECT without rendering and run its browse callback on the model.
The callback receives the model, input port, and output port."
  (require-project project)
  (require-progress progress)
  (unless (input-port? input)
    (error "project browser input must be an input port" input))
  (unless (output-port? output)
    (error "project browser output must be an output port" output))
  (let ((browse (project-browse project)))
    (unless (procedure? browse)
      (error "project does not define a #:browse callback"))
    (let ((model (analyze-project project #:progress progress)))
      (browse model input output))))

(define (execute-project project write? progress)
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
                         (if (cached-renderer? renderer)
                             (renderer-dependency-paths renderer)
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
           (and previous-state
                (renderer-cache-available? identity previous-identity)
                (load-cached-artifacts (project-output-directory project)
                                       previous-artifacts)))
         (global-inputs-changed?
           (or (input-cache-disabled? input-cache-context)
               (hash-ref analysis-trace 'uncached)
               (paths-intersect? (input-cache-dependencies input-cache-context) changed)
               (and (> (cdr (input-cache-summary input-cache-context)) 0)
                    (any (lambda (path) (not (file-exists? path))) removed))))
         (cache-hit? (and cache-ready?
                          (renderer-identities-match? identity previous-identity)
                          (all-renderers-cacheable? identity)
                          (null? changed)
                          (not global-inputs-changed?)))
         (render-result
           (cond
             (cache-hit?
              (list cache-ready?
                    (map (lambda (entry)
                           (cons (build-state-artifact-path entry)
                                 (build-state-artifact-owner entry)))
                         previous-artifacts)
                    '()
                    '()))
             (global-inputs-changed?
              (progress 'render #f)
              (render-all model (project-renderers project) progress))
             (cache-ready?
              (progress 'render #f)
              (render-with-cache model (project-renderers project)
                                 previous-artifacts cache-ready?
                                 previous-sources changed progress
                                 #:renderer-identities identity
                                 #:previous-renderer-identities previous-identity))
             (else
              (progress 'render #f)
              (render-all model (project-renderers project) progress))))
          (artifacts (merge-pipeline-artifacts pipeline-artifacts
                                               (car render-result) retired))
          (owners (append pipeline-owners (cadr render-result)))
         (affected
           (project-affected-artifacts
             artifacts owners (caddr render-result) previous-artifacts changed
             identity previous-identity #:cache-hit? cache-hit?)))
    (validate-project-artifacts artifacts progress)
    (when write?
      (write-project-build!
        (project-output-directory project) state-path state-temp-path
        current-sources artifacts owners identity progress
        #:earlier-sources pre-build-sources
        #:prune (lambda () (prune-input-cache! input-cache-context))))
    (let* ((report
             (make-build-report (map build-state-source-path current-sources)
                                artifacts added modified removed affected
                                cache-hit?))
           (analysis-inputs
             (let ((context (project-analysis-input-cache-context analysis)))
               (and context
                    (lset-difference string=?
                      (input-cache-dependencies context)
                      (project-extra-sources project))))))
      (values report model analysis-inputs))))

(define* (build-project/report+analysis project
                                        #:key (progress (lambda (stage path) #t)))
  "Build PROJECT and return its report, analyzed model, and analysis inputs.

This private entry point lets the preview worker retain state explicitly."
  (require-project project)
  (require-progress progress)
  (with-build-lock (project-output-directory project)
    (lambda () (execute-project project #t progress))))

(define* (build-project/report project #:key (progress (lambda (stage path) #t)))
  "Build PROJECT, notifying PROGRESS with a stage symbol and detail.

The build takes the project's build lock: the artifact set, manifest,
and stale-output cleanup all live in the output directory, so a serve
watch and a manual build over one project must not run concurrently."
  (require-project project)
  (require-progress progress)
  (call-with-values
    (lambda ()
      (build-project/report+analysis project #:progress progress))
    (lambda (report _model _analysis-inputs)
      report)))

(define* (check-project project #:key (progress (lambda (stage path) #t)))
  "Read, analyze, render, and validate PROJECT without accessing output state.
Return its complete validated artifact list; use BUILD-PROJECT/REPORT when
source changes or cache status are needed."
  (require-project project)
  (require-progress progress)
  (call-with-values
    (lambda () (execute-project project #f progress))
    (lambda (report _model _analysis-inputs)
      (build-report-artifacts report))))

(define* (build-project project #:key (progress (lambda (stage path) #t)))
  (build-report-artifacts (build-project/report project #:progress progress)))
