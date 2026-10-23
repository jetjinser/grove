;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project site)
  #:use-module (grove build)
  #:use-module (grove i18n)
  #:use-module (grove content transform)
  #:use-module (grove project)
  #:use-module (grove render context)
  #:use-module (grove render derived)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-site-project))

(define* (make-site-project #:key
                            (title "Site")
                            (description #f)
                            (source-directory #f)
                            (source-filter (lambda (path)
                                             (string-suffix? ".plant" path)))
                            (collect-sources #f)
                            (prepare (lambda () '()))
                            (extra-sources '())
                            read-page
                            (analyze (lambda (site) site))
                            (transforms '())
                            (renderers '())
                            (derived '())
                            (context #f)
                            (pipelines '())
                            output-directory
                             (cache? #t)
                             (default-prefix #f)
                             (tree-template #f)
                             (home #f)
                             (browse #f)
                             (locale #f))
  (unless (procedure? read-page)
    (error "site project requires a page reader" read-page))
  (unless (procedure? analyze)
    (error "site project analyzer must be a procedure" analyze))
  (unless (every content-transform? transforms)
    (error "site project transforms must be content transforms" transforms))
  (unless (every derived-artifact? derived)
    (error "site project derived values must be derived artifacts" derived))
  (unless (or (not context) (procedure? context))
    (error "site project context builder must be a procedure or #f" context))
  (let* ((locale-tag (and locale (locale-tag (make-locale locale))))
         (derived-context
           (or context
               (lambda (forest)
                 (make-render-context #:forest forest))))
         (context-forest #f)
         (context-value #f)
         (context-for
           (lambda (forest)
             (if (and context-forest (eq? context-forest forest))
                 context-value
                 (let ((value (derived-context forest)))
                   (set! context-forest forest)
                   (set! context-value value)
                   value))))
         (derived-renderer
           (and (pair? derived)
                (make-cached-renderer
                  (lambda (forest)
                    (render-derived-artifacts derived
                                              (context-for forest)
                                              forest))
                  #:cache? cache?
                  #:configuration
                  (lambda (forest)
                    (append
                      (list 'derived (map derived-artifact-name derived))
                      (append-map
                        (lambda (artifact)
                          ((derived-artifact-configuration artifact)
                           (context-for forest)
                           forest))
                        derived)))))))
    (make-project
      #:source-directory source-directory
      #:source-filter source-filter
      #:collect-sources collect-sources
      #:prepare prepare
      #:extra-sources extra-sources
      #:read-source
      (lambda (source)
        (parameterize ((current-reader-locale
                         locale-tag))
          (let ((page (read-page source)))
            (if (null? transforms)
                page
                (make-site-page
                  (site-page-id page)
                  (site-page-title page)
                  (transform-document
                    (apply compose-content-transforms transforms)
                    (site-page-document page))
                  (site-page-source page)
                  #:metadata (site-page-metadata page))))))
      #:analyze
      (lambda (analysis-context)
        (let* ((inputs (project-context-inputs analysis-context))
               (pages (map project-input-value inputs))
               (sources
                 (delete-duplicates
                   (append (map project-input-path inputs)
                           (project-context-prepared-sources analysis-context)
                           (filter-map site-page-source pages))
                   string=?))
               (site (make-site pages #:title title #:description description
                                #:sources sources #:home home)))
          (analyze site)))
      #:renderers (if derived-renderer
                      (append renderers (list derived-renderer))
                      renderers)
      #:pipelines pipelines
      #:output-directory output-directory
      #:cache? cache?
      #:default-prefix default-prefix
      #:tree-template tree-template
      #:browse browse)))
