;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private pipeline)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove output)
  #:use-module (grove private artifact-store)
  #:use-module (grove private content-store)
  #:use-module (grove private fingerprint)
  #:use-module (grove private input dependencies)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (run-asset-pipelines
            merge-pipeline-artifacts))

;;     Run declared asset pipelines against the shared store.  Each
;;     pipeline's identity is its name, version, the project file's
;;     fingerprint, and its inputs' fingerprints; the outputs of one
;;     identity are indexed under that key and every output blob is
;;     content-addressed.  A hit materializes artifacts without
;;     running the procedure at all; a miss - changed inputs, edited
;;     project, corrupted or missing index - runs it.  Determinism
;;     (same inputs, same bytes) is a protocol contract the content
;;     addressing verifies in passing.

(define (pipeline-input-table pipeline)
  ;;     (declared path -> materialized path), registered so the
  ;;     fingerprints memoize across the build.
  (map (lambda (path) (cons path (depend-on! path)))
       (asset-pipeline-inputs pipeline)))

(define (pipeline-artifact-sources inputs dependencies)
  (delete-duplicates
    (append (map cdr inputs) (map car dependencies))
    string=?))

(define (pipeline-identity pipeline project-key input-table)
  (content-fingerprint
   (call-with-output-string
    (lambda (port)
      (write (list "grove-asset-pipeline"
                   (asset-pipeline-name pipeline)
                   (asset-pipeline-version pipeline)
                   project-key
                   (map (lambda (pair) (input-fingerprint (cdr pair)))
                        input-table))
             port)))))

(define (restore-pipeline-artifacts identity inputs)
  (let ((index (read-pipeline-index identity)))
    (and index
         (every (lambda (dependency)
                  (equal? (cadr dependency)
                          (input-fingerprint (car dependency))))
                (car index))
         (let ((sources (pipeline-artifact-sources inputs (car index))))
         (let loop ((entries (cadr index)) (artifacts '()))
           (if (null? entries)
               (reverse artifacts)
               (let* ((entry (car entries))
                      (bytes (restore-content-bytes (cdr entry))))
                 (and bytes
                      (loop (cdr entries)
                            (cons (make-byte-artifact
                                   (car entry) bytes sources)
                                  artifacts))))))))))

(define (read-pipeline-index identity)
  ;; Runtime Scheme loads are discovered while the procedure runs. Keep their
  ;; fingerprints in this pipeline-specific index beside its emitted outputs.
  (catch #t
    (lambda ()
      (let ((path (artifact-blob-path identity)))
        (and (file-exists? path)
             (call-with-input-file path
               (lambda (port)
                 (let ((index (read port)))
                   (and (list? index)
                        (= (length index) 4)
                        (eq? (car index) 'grove-pipeline-index)
                        (= (cadr index) 1)
                        (list? (caddr index))
                        (every (lambda (entry)
                                 (and (list? entry)
                                      (= (length entry) 2)
                                      (string? (car entry))
                                      (fingerprint? (cadr entry))))
                               (caddr index))
                        (list? (cadddr index))
                        (every (lambda (entry)
                                 (and (pair? entry)
                                      (artifact-path? (car entry))
                                      (fingerprint? (cdr entry))))
                               (cadddr index))
                        (list (caddr index) (cadddr index)))))))))
    (lambda args #f)))

(define (write-pipeline-index identity dependencies entries)
  (publish-artifact-blob! identity
    (lambda (port)
      (write (list 'grove-pipeline-index 1 dependencies entries) port))))

(define (run-pipeline-procedure pipeline identity inputs)
  (let ((entries '()) (artifacts '()))
    (define (emit path content)
      (unless (or (string? content) (bytevector? content))
        (error "asset pipelines emit strings or bytevectors" path content))
      (unless (artifact-path? path)
        (error "asset pipeline emitted an invalid artifact path" path))
      (let ((fingerprint (content-fingerprint content)))
        (publish-artifact-blob! fingerprint
          (lambda (port)
            (if (string? content)
                (display content port)
                (put-bytevector port content))))
        (set! entries (cons (cons path fingerprint) entries))
        (set! artifacts
              (cons ((if (string? content) make-artifact make-byte-artifact)
                     path content (map cdr inputs))
                    artifacts))))
    ;;     A procedure result is ignored on purpose: outputs reach
    ;;     Grove only through emit, which keeps the content
    ;;     addressing honest even when a tool wrote stray files.
    (let ((trace (make-hash-table)))
      (call-with-input-trace trace
        (lambda ()
          ((asset-pipeline-procedure pipeline)
           (lambda (declared)
             (or (assoc-ref inputs declared)
                 (error "asset pipeline asked for an undeclared input" declared)))
           emit)))
      (let ((entry-list (reverse entries))
            (artifact-list (reverse artifacts))
            (dependencies
             (sort (map (lambda (path)
                          (list path (input-fingerprint path)))
                        (trace-inputs trace))
                   (lambda (left right) (string<? (car left) (car right))))))
        (unless (pair? entry-list)
          (error "asset pipeline emitted no outputs"
                 (asset-pipeline-name pipeline)))
        (write-pipeline-index identity dependencies entry-list)
        (map (lambda (artifact)
               ((if (string? (out:artifact-content/raw artifact))
                    make-artifact make-byte-artifact)
                (out:artifact-path/raw artifact)
                (out:artifact-content/raw artifact)
                (pipeline-artifact-sources inputs dependencies)))
             artifact-list)))))

(define (run-asset-pipelines pipelines project-key)
  "Run every declared asset pipeline; return (artifacts . owners),
where each owner maps the artifact path to the pipeline name for the
build state's renderer field."
  (let loop ((pipelines pipelines) (artifacts '()) (owners '()))
    (if (null? pipelines)
        (cons (reverse artifacts) (reverse owners))
        (let* ((pipeline (car pipelines))
               (name (asset-pipeline-name pipeline))
               (inputs (pipeline-input-table pipeline))
               (identity (pipeline-identity pipeline project-key inputs))
               (produced (or (restore-pipeline-artifacts identity inputs)
                             (run-pipeline-procedure pipeline identity inputs)))
               (markers (map (lambda (artifact)
                               (cons (out:artifact-path/raw artifact) name))
                             produced)))
          (loop (cdr pipelines)
                (append (reverse produced) artifacts)
                (append (reverse markers) owners))))))

;;     Pipeline outputs shadow same-path artifacts from renderers and
;;     the previous state, and outputs retired from a pipeline's
;;     current set must not resurrect from that state either.
(define (merge-pipeline-artifacts pipeline-artifacts rendered retired-paths)
  (if (and (null? pipeline-artifacts) (null? retired-paths))
      rendered
      (let ((shadowed (append (map out:artifact-path/raw pipeline-artifacts)
                              retired-paths)))
        (append pipeline-artifacts
                (remove (lambda (artifact)
                          (member (out:artifact-path/raw artifact) shadowed string=?))
                        rendered)))))
