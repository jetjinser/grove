;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (build-site
            render-site
            cached-renderer?
            cached-renderer-cache-key
            cached-renderer-procedure
            cached-renderer-render-changed
            make-cached-renderer
            renderer-cache-keys))

(define-record-type <cached-renderer>
  (%make-cached-renderer cache-key procedure render-changed)
  cached-renderer?
  (cache-key cached-renderer-cache-key)
  (procedure cached-renderer-procedure)
  (render-changed cached-renderer-render-changed))

(define* (make-cached-renderer procedure #:key cache-key render-changed)
  (unless (procedure? procedure)
    (error "cached renderer requires a procedure" procedure))
  (unless (and (string? cache-key) (not (string-null? cache-key)))
    (error "cached renderer requires a non-empty cache key" cache-key))
  (unless (or (not render-changed) (procedure? render-changed))
    (error "incremental renderer must be a procedure or #f" render-changed))
  (%make-cached-renderer cache-key procedure render-changed))

(define (renderer-cache-keys renderers)
  (and (every cached-renderer? renderers)
       (map cached-renderer-cache-key renderers)))

(define (read-page-with-context path read-page)
  (catch #t
    (lambda () (read-page path))
    (lambda (key . args)
      (error (format #f "failed to read project source ~a (~a): ~s"
                     path key args)))))

(define* (build-site sources read-page #:key
                     (title "Site")
                     (module-sources '())
                     (load-modules #f))
  (unless (null? module-sources)
    (unless (procedure? load-modules)
      (error "site module sources require a module loader" module-sources))
    (catch #t
      (lambda () (load-modules module-sources))
      (lambda (key . args)
        (error (format #f "failed to load project module sources ~s (~a): ~s"
                       module-sources key args)))))
  (let ((pages (map (lambda (path)
                      (read-page-with-context path read-page))
                    sources)))
  (make-site pages
             #:title title
             #:sources (delete-duplicates
                         (filter string?
                             (append module-sources sources
                             (filter-map site-page-source pages)))
                         string=?))))

(define (render-site site renderers)
  (apply append
    (map (lambda (renderer)
           ((if (cached-renderer? renderer)
                (cached-renderer-procedure renderer)
                renderer)
            site))
         renderers)))
