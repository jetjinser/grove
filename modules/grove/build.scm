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
            make-cached-renderer
            renderer-cache-keys))

(define-record-type <cached-renderer>
  (%make-cached-renderer cache-key procedure)
  cached-renderer?
  (cache-key cached-renderer-cache-key)
  (procedure cached-renderer-procedure))

(define* (make-cached-renderer procedure #:key cache-key)
  (unless (procedure? procedure)
    (error "cached renderer requires a procedure" procedure))
  (unless (and (string? cache-key) (not (string-null? cache-key)))
    (error "cached renderer requires a non-empty cache key" cache-key))
  (%make-cached-renderer cache-key procedure))

(define (renderer-cache-keys renderers)
  (and (every cached-renderer? renderers)
       (map cached-renderer-cache-key renderers)))

(define* (build-site sources read-page #:key
                     (title "Site")
                     (module-sources '())
                     (load-modules #f))
  (unless (null? module-sources)
    (unless (procedure? load-modules)
      (error "site module sources require a module loader" module-sources))
    (load-modules module-sources))
  (let ((pages (map read-page sources)))
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
