;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project)
  #:use-module (grove build)
  #:use-module (grove collect)
  #:use-module (grove output)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (project?
            make-project
            build-project))

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

(define (build-project project)
  (unless (project? project)
    (error "build-project requires a Grove project" project))
  (let* ((module-directory (project-module-directory project))
         (module-sources
           (if module-directory
               (collect-directory module-directory
                                  (project-module-filter project))
               '())))
  (let* ((site
           (build-site
             (collect-directory (project-source-directory project)
                                (project-source-filter project))
             (project-read-page project)
             #:title (project-title project)
             #:module-sources module-sources
             #:load-modules (project-load-modules project)))
         (model ((project-analyze project) site))
         (artifacts (render-site model (project-renderers project))))
    (write-artifacts (project-output-directory project) artifacts)
    artifacts)))
