;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove string-fun)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove collect)
  #:use-module (grove output)
  #:use-module (grove project diagnostic)
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
            project-analysis?
            project-analysis-model
            browse-project))

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
  (%make-project source-directory source-filter collect-sources prepare
                 extra-sources read-source analyze renderers
                 output-directory cache? default-prefix tree-template browse)
  project?
  (source-directory project-source-directory)
  (source-filter project-source-filter)
  (collect-sources project-collect-sources)
  (prepare project-prepare)
  (extra-sources project-extra-sources)
  (read-source project-read-source)
  (analyze project-analyze)
  (renderers project-renderers)
  (output-directory project-output-directory)
  (cache? project-cache?)
  (default-prefix project-default-prefix)
  (tree-template project-tree-template)
  (browse project-browse))

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
  (unless (procedure? prepare)
    (error "project prepare hook must be a procedure" prepare))
  (unless (and (list? extra-sources) (every string? extra-sources))
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
  (unless (string? output-directory)
    (error "project requires an output directory" output-directory))
  (unless (boolean? cache?)
    (error "project cache option must be a boolean" cache?))
  (unless (or (not default-prefix) (non-empty-string? default-prefix))
    (error "project default prefix must be a non-empty string" default-prefix))
  (unless (or (not tree-template) (procedure? tree-template))
    (error "project tree template must be a procedure" tree-template))
  (unless (or (not browse) (procedure? browse))
    (error "project browse callback must be a procedure" browse))
  (%make-project source-directory source-filter collect-sources prepare
                 extra-sources read-source analyze
                 (map (lambda (renderer)
                        (if (cached-renderer? renderer) renderer
                            (make-cached-renderer renderer))) renderers)
                 output-directory cache? default-prefix tree-template browse))

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
  (let ((added
          (filter (lambda (entry)
                    (not (source-entry-by-path
                           old-sources (build-state-source-path entry))))
                  current-sources))
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
        (removed
          (filter (lambda (entry)
                    (not (source-entry-by-path
                           current-sources (build-state-source-path entry))))
                  old-sources)))
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

(define (render-with-cache model renderers previous-entries cached-artifacts
                           previous-sources changed progress)
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
                 (call-with-project-stage 'render #f
                   (lambda ()
                     (if rendered-fully?
                         (render-model model (list renderer))
                         (begin
                           (unless (and (list? generated) (every artifact? generated))
                             (error "incremental renderer must return artifacts or #f"
                                    generated))
                           generated)))))
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

(define* (analyze-project project #:key (write? #f)
                          (progress (lambda (stage path) #t)))
  "Collect, read, and analyze PROJECT without rendering or writing outputs.
Return a project-analysis holding the analyzed model."
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
                 (project-output-directory project) (project-cache? project)
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

(define* (browse-project project #:key (progress (lambda (stage path) #t))
                         (input (current-input-port))
                         (output (current-output-port)))
  "Analyze PROJECT without rendering and run its browse callback on the model."
  (let ((browse (project-browse project)))
    (unless (procedure? browse)
      (error "project does not define a #:browse callback"))
    (let ((analysis (analyze-project project #:write? #f #:progress progress)))
      (browse (project-analysis-model analysis) #:input input #:output output))))

(define (execute-project project write? progress)
  (unless (project? project)
    (error "project execution requires a Grove project" project))
  (let* ((analysis (analyze-project project #:write? write? #:progress progress))
         (source-paths (project-analysis-source-paths analysis))
         (prepared-sources (project-analysis-prepared-sources analysis))
         (inputs (project-analysis-inputs analysis))
         (context (project-analysis-context analysis))
         (analysis-trace (project-analysis-trace analysis))
         (model (project-analysis-model analysis))
         (input-cache-context (project-analysis-input-cache-context analysis))
         (state-path
           (string-append (project-output-directory project) "/"
                          build-state-name))
         (state-temp-path
           (string-append (project-output-directory project) "/"
                          build-state-temp-name))
         (previous-state (and write? (read-build-state state-path)))
         (previous-sources (if previous-state
                               (build-state-sources previous-state) '()))
         (previous-artifacts (if previous-state
                                 (build-state-artifacts previous-state) '()))
         (previous-identity (and previous-state
                                  (build-state-identity previous-state)))
         (current-sources
           (map (lambda (path) (list path (input-fingerprint path)))
             (delete-duplicates
                  (append prepared-sources source-paths
                          (input-cache-dependencies input-cache-context)
                          (append-map
                            (lambda (renderer)
                              (if (and (cached-renderer? renderer)
                                       (cached-renderer-inputs renderer))
                                  ((cached-renderer-inputs renderer))
                                  '()))
                            (project-renderers project))
                          (project-extra-sources project))
                  string=?)))
         (source-changes
           (call-with-values
             (lambda () (changed-sources previous-sources current-sources))
             list))
         (added (car source-changes))
         (modified (cadr source-changes))
         (removed (caddr source-changes))
         (changed (append added modified removed))
         (identity (project-renderer-identity project model))
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
         (artifacts (car render-result))
         (owners (cadr render-result))
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
          (prune-input-cache! input-cache-context))))
    (%make-build-report (map build-state-source-path current-sources) artifacts added modified removed affected
                        (not cache-hit?))))

(define* (build-project/report project #:key (progress (lambda (stage path) #t)))
  "Build PROJECT, notifying PROGRESS with a stage symbol and detail."
  (execute-project project #t progress))

(define* (check-project project #:key (progress (lambda (stage path) #t)))
  "Read, analyze, render, and validate PROJECT without accessing output state."
  (execute-project project #f progress))

(define* (build-project project #:key (progress (lambda (stage path) #t)))
  (build-report-artifacts (build-project/report project #:progress progress)))
