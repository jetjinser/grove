;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove collect)
  #:use-module (grove output)
  #:use-module (grove site)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (project?
            make-project
            build-report?
            build-report-artifacts
            build-report-added-sources
            build-report-modified-sources
            build-report-removed-sources
            build-report-affected-artifacts
            build-report-rendered?
            build-project/report
            build-project))

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
  (%make-project title source-directory source-filter module-directory
                 module-filter load-modules extra-sources read-page analyze renderers
                 output-directory cache-key)
  project?
  (title project-title)
  (source-directory project-source-directory)
  (source-filter project-source-filter)
  (module-directory project-module-directory)
  (module-filter project-module-filter)
  (load-modules project-load-modules)
  (extra-sources project-extra-sources)
  (read-page project-read-page)
  (analyze project-analyze)
  (renderers project-renderers)
  (output-directory project-output-directory)
  (cache-key project-cache-key))

(define (source-file? filename)
  (string-suffix? ".grv" filename))

(define* (make-project #:key
                       (title "Site")
                       source-directory
                       (source-filter source-file?)
                       (module-directory #f)
                       (module-filter source-file?)
                       (load-modules #f)
                       (extra-sources '())
                       read-page
                       (analyze (lambda (site) site))
                       (renderers '())
                       output-directory
                       (cache-key #f))
  (unless (and (string? title) (not (string-null? title)))
    (error "project requires a title" title))
  (unless (and (string? source-directory)
               (procedure? source-filter))
    (error "project requires a source directory and filter"
           source-directory source-filter))
  (unless (or (not module-directory)
              (and (string? module-directory)
                   (procedure? module-filter)))
    (error "project module directory requires a module filter"
           module-directory module-filter))
  (unless (or (not module-directory)
              (procedure? load-modules))
    (error "project module directory requires a module loader"
           module-directory))
  (unless (and (list? extra-sources) (every string? extra-sources))
    (error "project extra sources must be a list of paths" extra-sources))
  (unless (procedure? read-page)
    (error "project requires a page reader" read-page))
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
  (%make-project title source-directory source-filter module-directory
                 module-filter load-modules extra-sources read-page analyze renderers
                 output-directory cache-key))

(define (source-paths entries)
  (map car entries))

(define (paths-intersect? paths candidates)
  (any (lambda (path) (member path candidates string=?)) paths))

(define (state-entry-by-path entries path)
  (find (lambda (entry) (string=? (car entry) path)) entries))

(define (changed-sources old-sources current-sources)
  (let ((added
          (filter (lambda (entry)
                    (not (state-entry-by-path old-sources (car entry))))
                  current-sources))
        (modified
          (filter (lambda (entry)
                    (let ((previous
                            (state-entry-by-path old-sources (car entry))))
                      (and previous (not (= (cadr previous) (cadr entry))))))
                  current-sources))
        (removed
          (filter (lambda (entry)
                    (not (state-entry-by-path current-sources (car entry))))
                  old-sources)))
    (values (source-paths added)
            (source-paths modified)
            (source-paths removed))))

(define (artifact-affected? artifact previous changed)
  (let* ((path (artifact-path artifact))
         (old (state-entry-by-path previous path))
         (sources (artifact-sources artifact))
         (old-sources (and old (caddr old))))
    (or (not old)
        (not (= (cadr old) (content-fingerprint (artifact-content artifact))))
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
    (map car
         (filter (lambda (entry)
                   (not (find (lambda (artifact)
                                (string=? (artifact-path artifact) (car entry)))
                              artifacts)))
                 previous))))

(define (load-cached-artifacts directory entries)
  (let loop ((entries entries) (artifacts '()))
    (if (null? entries)
        (reverse artifacts)
        (let* ((entry (car entries))
               (path (string-append directory "/" (car entry)))
               (content
                 (and (file-exists? path)
                      (not (file-is-directory? path))
                      (catch #t
                        (lambda ()
                          (call-with-input-file path get-string-all))
                        (lambda args #f)))))
          (if (and content
                   (= (content-fingerprint content) (cadr entry)))
              (loop (cdr entries)
                    (cons (make-artifact (car entry) content (caddr entry))
                          artifacts))
              #f)))))

(define (project-renderer-cache-key project)
  (let ((renderer-keys (renderer-cache-keys (project-renderers project)))
        (project-key (project-cache-key project)))
    (and project-key renderer-keys
         (call-with-output-string
           (lambda (port)
             (write (list project-key renderer-keys) port))))))

(define (render-all model renderers)
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
        (let ((generated (render-site model (list (car renderers)))))
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
                           (equal? (cadddr entry) index))
                         previous-entries))
               (old-artifacts
                 (filter (lambda (artifact)
                           (and (state-entry-by-path
                                  old-entries (artifact-path artifact))
                                #t))
                         cached-artifacts))
               (changed-proc
                 (and (cached-renderer? renderer)
                      (cached-renderer-render-changed renderer)))
               (partial? (and changed-proc
                              (every (lambda (entry)
                                       (integer? (cadddr entry)))
                                     old-entries)))
               (dirty-entries
                 (filter (lambda (entry)
                           (or (any (lambda (path)
                                      (not (state-entry-by-path
                                             previous-sources path)))
                                    changed)
                               (not (caddr entry))
                               (paths-intersect? (caddr entry) changed)))
                         old-entries))
               (dirty-paths (map car dirty-entries))
               (generated
                 (and partial?
                      (changed-proc model changed dirty-paths old-artifacts)))
               (rendered-fully? (or (not partial?) (not generated)))
               (new-artifacts
                 (if rendered-fully?
                     (render-site model (list renderer))
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
  (let* ((module-directory (project-module-directory project))
         (module-sources
           (if module-directory
               (collect-directory module-directory
                                  (project-module-filter project))
               '()))
         (page-sources
           (collect-directory (project-source-directory project)
                              (project-source-filter project)))
         (state-path
           (string-append (project-output-directory project) "/"
                          build-state-name))
         (state-temp-path
           (string-append (project-output-directory project) "/"
                          build-state-temp-name))
         (previous-state (read-build-state state-path))
         (previous-sources (if previous-state (cadr previous-state) '()))
         (previous-artifacts (if previous-state (caddr previous-state) '()))
         (previous-cache-key (and previous-state (cadddr previous-state)))
         (site
           (build-site
             page-sources
             (project-read-page project)
             #:title (project-title project)
             #:module-sources module-sources
             #:load-modules (project-load-modules project)))
         (current-sources
           (map (lambda (path) (list path (file-fingerprint path)))
                (delete-duplicates
                  (append (site-sources site)
                          (project-extra-sources project))
                  string=?)))
         (model ((project-analyze project) site))
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
                           (cons (car entry) (cadddr entry)))
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
                                                        (car entry)))
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
