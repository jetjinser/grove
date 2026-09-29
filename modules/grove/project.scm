;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove collect)
  #:use-module (grove output)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (project?
            make-project
            project-input?
            project-input-path
            project-input-value
            project-context?
            project-context-inputs
            project-context-prepared-sources
            project-context-extra-sources
            build-report?
            build-report-artifacts
            build-report-added-sources
            build-report-modified-sources
            build-report-removed-sources
            build-report-affected-artifacts
            build-report-rendered?
            build-project/report
            build-project))

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
  (%make-build-report artifacts added modified removed affected rendered?)
  build-report?
  (artifacts build-report-artifacts)
  (added build-report-added-sources)
  (modified build-report-modified-sources)
  (removed build-report-removed-sources)
  (affected build-report-affected-artifacts)
  (rendered? build-report-rendered?))

(define-record-type <project>
  (%make-project source-directory source-filter collect-sources prepare
                 extra-sources read-source analyze renderers
                 output-directory cache-key)
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
  (cache-key project-cache-key))

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
                       (cache-key #f))
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
  (unless (or (not cache-key)
              (and (string? cache-key) (not (string-null? cache-key))))
    (error "project cache key must be a non-empty string or #f" cache-key))
  (%make-project source-directory source-filter collect-sources prepare
                 extra-sources read-source analyze renderers
                 output-directory cache-key))

(define (read-source-with-context path read-source)
  (catch #t
    (lambda () (read-source path))
    (lambda (key . args)
      (error (format #f "failed to read project source ~a (~a): ~s"
                     path key args)))))

(define (collect-project-sources project)
  (let ((paths
          ((or (project-collect-sources project)
               (lambda ()
                 (collect-directory (project-source-directory project)
                                    (project-source-filter project)))))))
    (unless (and (list? paths)
                 (every (lambda (path)
                          (and (string? path) (not (string-null? path))))
                        paths))
      (error "project collector must return a list of source paths" paths))
    (when (not (= (length paths) (length (delete-duplicates paths string=?))))
      (error "project collector returned duplicate paths" paths))
    paths))

(define (prepare-project-sources prepare)
  (let ((paths
          (catch #t
            (lambda () (prepare))
            (lambda (key . args)
              (error (format #f "project preparation failed (~a): ~s"
                              key args))))))
    (unless (and (list? paths)
                 (every (lambda (path)
                          (and (string? path) (not (string-null? path))))
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
                              (call-with-input-file path get-string-all)))
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

(define (project-renderer-cache-key project)
  (let ((renderer-keys (renderer-cache-keys (project-renderers project)))
        (project-key (project-cache-key project)))
    (and project-key renderer-keys
         (call-with-output-string
           (lambda (port)
             (write (list 'grove-render-cache-v2
                          project-key renderer-keys)
                    port))))))

(define (render-all model renderers)
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
        (let ((generated (render-model model (list (car renderers)))))
          (loop (cdr renderers)
                (+ index 1)
                (append artifacts generated)
                (append owners
                        (map (lambda (artifact)
                               (cons (artifact-path artifact) index))
                             generated)))))))

(define (render-with-cache model renderers previous-entries cached-artifacts
                           previous-sources changed)
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
        (let* ((renderer (car renderers))
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
                      (changed-proc model changed dirty-paths old-artifacts)))
               (rendered-fully? (or (not partial?) (not generated)))
               (new-artifacts
                 (if rendered-fully?
                     (render-model model (list renderer))
                     (begin
                       (unless (and (list? generated) (every artifact? generated))
                         (error "incremental renderer must return artifacts or #f"
                                generated))
                       generated)))
               ;; An incremental callback returns the complete current output
               ;; set for its renderer. This lets it preserve unaffected
               ;; artifacts from OLD-ARTIFACTS and omit outputs that vanished.
               (current new-artifacts)
               (current-owners
                 (map (lambda (artifact)
                        (cons (artifact-path artifact) index))
                      current))
               )
          (loop (cdr renderers)
                (+ index 1)
                (append artifacts current)
                (append owners current-owners))))))

(define (build-project/report project)
  (unless (project? project)
    (error "build-project/report requires a Grove project" project))
  (let* ((source-paths (collect-project-sources project))
         (prepared-sources (prepare-project-sources
                             (project-prepare project)))
         (state-path
           (string-append (project-output-directory project) "/"
                          build-state-name))
         (state-temp-path
           (string-append (project-output-directory project) "/"
                          build-state-temp-name))
         (previous-state (read-build-state state-path))
         (previous-sources (if previous-state
                               (build-state-sources previous-state) '()))
         (previous-artifacts (if previous-state
                                 (build-state-artifacts previous-state) '()))
         (previous-cache-key (and previous-state
                                  (build-state-cache-key previous-state)))
         (inputs
           (map (lambda (path)
                  (make-project-input
                    path
                    (read-source-with-context path
                                              (project-read-source project))))
                source-paths))
         (current-sources
           (map (lambda (path) (list path (file-fingerprint path)))
             (delete-duplicates
                  (append prepared-sources source-paths
                          (append-map
                            (lambda (renderer)
                              (if (and (cached-renderer? renderer)
                                       (cached-renderer-inputs renderer))
                                  ((cached-renderer-inputs renderer))
                                  '()))
                            (project-renderers project))
                          (project-extra-sources project))
                  string=?)))
         (context
           (make-project-context inputs prepared-sources
                                 (project-extra-sources project)))
         (model ((project-analyze project) context))
         (source-changes
           (call-with-values
             (lambda () (changed-sources previous-sources current-sources))
             list))
         (added (car source-changes))
         (modified (cadr source-changes))
         (removed (caddr source-changes))
         (changed (append added modified removed))
         (cache-key (project-renderer-cache-key project))
         (cache-ready?
           (and cache-key previous-state
                (equal? cache-key previous-cache-key)
                (load-cached-artifacts (project-output-directory project)
                                       previous-artifacts)))
         (cache-hit? (and cache-ready? (null? changed)))
         (render-result
           (cond
             (cache-hit?
              (list cache-ready?
                    (map (lambda (entry)
                           (cons (build-state-artifact-path entry)
                                 (build-state-artifact-renderer entry)))
                         previous-artifacts)
                    '()))
             (cache-ready?
              (render-with-cache model (project-renderers project)
                                 previous-artifacts cache-ready?
                                 previous-sources changed))
             (else
              (render-all model (project-renderers project)))))
         (artifacts (car render-result))
         (owners (cadr render-result))
         (affected
           (cond
             (cache-hit? '())
             ((and cache-key previous-state
                   (equal? cache-key previous-cache-key)
                   (not cache-ready?))
              (map artifact-path artifacts))
             ((and previous-state
                   (not (equal? cache-key previous-cache-key)))
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
    (write-artifacts (project-output-directory project) artifacts)
    (write-build-state state-path state-temp-path current-sources artifacts
                       owners cache-key)
    (%make-build-report artifacts added modified removed affected
                        (not cache-hit?))))

(define (build-project project)
  (build-report-artifacts (build-project/report project)))
