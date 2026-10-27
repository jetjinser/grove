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
  #:export (make-scribble-module-setup
            make-scribble-module-session
            make-scribble-document-reader)
  #:re-export (depend-on! disable-cache!))

(define (scribble-source-file? filename)
  (string-suffix? ".plant" filename))

(define* (make-scribble-module-setup module-directory
                                     #:key
                                     (module-filter scribble-source-file?))
  "Return a project preparation procedure that loads Scribble module sources.
MODULE-DIRECTORY must be non-empty and is copied at construction. The
procedure returns loaded paths so Grove can track them as build inputs."
  (unless (and (string? module-directory)
               (not (string-null? module-directory))
               (procedure? module-filter))
    (error "Scribble module setup requires a directory and filter"
           module-directory module-filter))
  (set! module-directory (string-copy module-directory))
  (let ((read-document (make-scribble-document-reader)))
    (lambda ()
      (let ((paths (collect-directory module-directory module-filter)))
        (for-each read-document paths)
        paths))))

(define* (make-scribble-module-session paths
                                      #:key
                                      (read-document (make-scribble-document-reader)))
  "Return a project preparation procedure and its document reader.
PATHS is a list of non-empty path strings, copied at construction. PREPARE
evaluates module articles in PATHS order, once per preparation. The reader
reuses those document results within that preparation and delegates other
paths. Preparing again resets reuse; this is not a persistent cache."
  (unless (and (list? paths)
               (every (lambda (path)
                        (and (string? path) (not (string-null? path))))
                      paths)
               (procedure? read-document))
    (error "module session requires source paths and a reader" paths read-document))
  (set! paths (map string-copy paths))
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
file inputs with DEPEND-ON! or CACHE-DEPENDENCIES, which returns a list of
non-empty paths. DISABLE-CACHE! opts out from within a document. A supplied
ENVIRONMENT must be a module; shared environments and checks always evaluate
sources."
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
  (unless (or (not environment) (module? environment))
    (error "Scribble reader environment must be a module or #f" environment))
  (unless (or (boolean? cache?) (procedure? cache?))
    (error "Scribble reader cache option must be a boolean or path predicate"
           cache?))
  (unless (procedure? cache-dependencies)
    (error "Scribble cache dependencies must be a procedure"
           cache-dependencies))
  (unless (or (not timings) (procedure? timings))
    (error "Scribble reader timings must be a procedure or #f" timings))
  (unless (procedure? environment-factory)
    (error "Scribble reader environment factory must be a procedure"
           environment-factory))
  (lambda (path)
    (if (current-input-cache)
        (let ((dependencies (cache-dependencies)))
          (unless (and (list? dependencies)
                       (every (lambda (path)
                                (and (string? path)
                                     (not (string-null? path))))
                              dependencies))
            (error "Scribble cache dependency collector must return paths"
                   dependencies))
          (cached-document path "scribble" dependencies
                           (lambda () (read-document path)) (lambda _ #t)
                           (and (not environment)
                                (if (procedure? cache?) (cache? path) cache?))))
        (read-document path))))
