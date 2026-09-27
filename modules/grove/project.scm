;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove collect)
  #:use-module (grove output)
  #:use-module (grove site)
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
            build-project/report
            build-project))

(define-record-type <build-report>
  (%make-build-report artifacts added modified removed affected)
  build-report?
  (artifacts build-report-artifacts)
  (added build-report-added-sources)
  (modified build-report-modified-sources)
  (removed build-report-removed-sources)
  (affected build-report-affected-artifacts))

(define-record-type <project>
  (%make-project title source-directory source-filter module-directory
                 module-filter load-modules read-page analyze renderers
                 output-directory)
  project?
  (title project-title)
  (source-directory project-source-directory)
  (source-filter project-source-filter)
  (module-directory project-module-directory)
  (module-filter project-module-filter)
  (load-modules project-load-modules)
  (read-page project-read-page)
  (analyze project-analyze)
  (renderers project-renderers)
  (output-directory project-output-directory))

(define (source-file? filename)
  (string-suffix? ".grv" filename))

(define* (make-project #:key
                       (title "Site")
                       source-directory
                       (source-filter source-file?)
                       (module-directory #f)
                       (module-filter source-file?)
                       (load-modules #f)
                       read-page
                       (analyze (lambda (site) site))
                       (renderers '())
                       output-directory)
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
  (unless (procedure? read-page)
    (error "project requires a page reader" read-page))
  (unless (procedure? analyze)
    (error "project analyzer must be a procedure" analyze))
  (unless (and (list? renderers) (every procedure? renderers))
    (error "project renderers must be a list of procedures" renderers))
  (unless (string? output-directory)
    (error "project requires an output directory" output-directory))
  (%make-project title source-directory source-filter module-directory
                 module-filter load-modules read-page analyze renderers
                 output-directory))

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
         (site
           (build-site
             page-sources
             (project-read-page project)
             #:title (project-title project)
             #:module-sources module-sources
             #:load-modules (project-load-modules project)))
         (current-sources
           (map (lambda (path) (list path (file-fingerprint path)))
                (site-sources site)))
         (model ((project-analyze project) site))
         (artifacts (render-site model (project-renderers project))))
    (call-with-values
      (lambda () (changed-sources previous-sources current-sources))
      (lambda (added modified removed)
        (let* ((changed (append added modified removed))
               (affected
                 (affected-artifacts artifacts previous-artifacts changed)))
          (write-artifacts (project-output-directory project) artifacts)
          (write-build-state state-path state-temp-path current-sources artifacts)
          (%make-build-report artifacts added modified removed affected))))))

(define (build-project project)
  (build-report-artifacts (build-project/report project)))
