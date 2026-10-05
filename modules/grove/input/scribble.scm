;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove input scribble)
  #:use-module (grove collect)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (system base language)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-1)
  #:use-module (grove ir)
  #:use-module ((language scribble spec)
                #:select (scribble scribble-evaluate-document))
  #:export (load-scribble-modules!
            make-scribble-module-setup
            make-scribble-module-session
            make-scribble-document-reader)
  #:re-export (depend-on! disable-cache!))

(define (scribble-source-file? filename)
  (string-suffix? ".plant" filename))

(define* (load-scribble-modules! paths
                                #:key
                                (read-document
                                 (make-scribble-document-reader)))
  "Evaluate PATHS as Scribble sources before reading documents that import
their named modules.  Each source selects its module through Guile's normal
`define-module' form."
  (for-each read-document paths)
  (values))

(define* (make-scribble-module-setup module-directory
                                     #:key
                                     (module-filter scribble-source-file?)
                                     (load-modules load-scribble-modules!))
  "Return a project prepare hook that loads Scribble module sources.
The hook returns the loaded paths so Grove can track them as build inputs."
  (unless (and (string? module-directory) (procedure? module-filter))
    (error "Scribble module setup requires a directory and filter"
           module-directory module-filter))
  (unless (procedure? load-modules)
    (error "Scribble module setup requires a loader" load-modules))
  (lambda ()
    (let ((paths (collect-directory module-directory module-filter)))
      (load-modules paths)
      paths)))

(define* (make-scribble-module-session paths
                                      #:key
                                      (read-document (make-scribble-document-reader)))
  "Return two procedures: a project prepare hook and its document reader.
PREPARE evaluates module articles in PATHS order, once per preparation. The
reader reuses those document results within that preparation and delegates
other paths. Preparing again resets reuse; this is not a persistent cache."
  (unless (and (list? paths) (every string? paths) (procedure? read-document))
    (error "module session requires source paths and a reader" paths read-document))
  (let ((prepared (make-hash-table)))
    (define (replay! trace)
      (for-each depend-on! (trace-inputs trace))
      (when (hash-ref trace 'uncached) (disable-cache!)))
    (define (prepare)
      (set! prepared (make-hash-table))
      (let* ((next (make-hash-table))
             (paths (delete-duplicates (map canonicalize-path paths) string=?)))
        (for-each
          (lambda (path)
            (let* ((trace (make-hash-table))
                   (document
                     (call-with-input-trace trace
                       (lambda () (depend-on! path) (read-document path)))))
              (hash-set! next path (cons document trace))
              (replay! trace)))
          paths)
        (set! prepared next)
        paths))
    (define (read path)
      (let ((entry (hash-ref prepared (canonicalize-path path))))
        (if entry
            (begin (replay! (cdr entry)) (car entry))
            (read-document path))))
    (values prepare read)))

(define* (make-scribble-document-reader
           #:optional (environment #f)
           #:key (timings #f)
           (cache? (lambda (path) #t))
           (cache-dependencies (lambda () '()))
           (environment-factory
                   (lambda () (default-environment scribble))))
  "Reuse data-only documents by default during builds. CACHE? may be a boolean
or path predicate. Imported modules are tracked automatically; register other
file inputs with DEPEND-ON! or CACHE-DEPENDENCIES. DISABLE-CACHE! opts out from
within a document. Shared environments and checks always evaluate sources."
  (define (read-document path)
    (call-with-input-file path
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (let* ((environment (or environment (environment-factory)))
               (defined-modules '())
               (document
                (scribble-evaluate-document port
                                            environment
                                            path
                                            #:module-changed
                                            (lambda (module)
                                              (set! defined-modules (cons module defined-modules))
                                              (disable-cache!))
                                            #:timings (and timings
                                                       (lambda (phase seconds)
                                                         (timings path phase seconds))))))
          (when (current-input-trace)
            (for-each record-module-inputs! (cons environment defined-modules)))
          (unless (content-document? document)
            (error "Scribble source requires a content document" path))
          document))))
  (lambda (path)
    (if (current-input-cache)
        (cached-document path "scribble" (cache-dependencies)
                         (lambda () (read-document path)) (lambda _ #t)
                         (and (not environment)
                              (if (procedure? cache?) (cache? path) cache?)))
        (read-document path))))
