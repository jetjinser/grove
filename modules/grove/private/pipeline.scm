;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private pipeline)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove output)
  #:use-module (grove private artifact-store)
  #:use-module (grove private fingerprint)
  #:use-module (grove private input dependencies)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:export (run-asset-pipelines
            merge-pipeline-artifacts))

;;     Run declared asset pipelines against the shared store. Each
;;     pipeline's identity includes its name, version, project fingerprint,
;;     each declared and materialized input path, and each content
;;     fingerprint. Outputs are indexed under that key, and every output
;;     blob is content-addressed.
;;     A hit materializes artifacts without
;;     running the procedure at all; a miss - changed inputs, edited
;;     project, corrupted or missing index - runs it. The caller must ensure
;;     that identical tracked inputs and version produce identical bytes.
;;     Content addressing verifies stored-byte integrity, not determinism.

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
                   (map car input-table)
                   (map cdr input-table)
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
                      (bytes (read-content-blob (cdr entry))))
                 (and bytes
                      (loop (cdr entries)
                            (cons (make-byte-artifact
                                   (car entry) bytes sources)
                                  artifacts))))))))))

(define (read-pipeline-index identity)
  ;; Runtime Scheme loads are discovered while the procedure runs. Keep their
  ;; fingerprints in this pipeline-specific index beside its returned outputs.
  (catch #t
    (lambda ()
      (let ((path (artifact-blob-path identity)))
        (and (artifact-blob-exists? identity)
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

(define (run-pipeline-procedure pipeline identity inputs cache?)
  (let* ((trace (make-hash-table))
         (artifact-list
          (call-with-input-trace trace
            (lambda ()
              (let ((artifacts
                      ((asset-pipeline-procedure pipeline)
                       (lambda (declared)
                         (or (assoc-ref inputs declared)
                             (error "asset pipeline asked for an undeclared input"
                                    declared))))))
                (unless (and (list? artifacts) (every artifact? artifacts))
                  (error "asset pipeline must return a list of artifacts"
                         (asset-pipeline-name pipeline) artifacts))
                (validate-artifacts artifacts)
                ;; Artifact-local sources are tracked like explicit reads.
                (for-each
                  (lambda (artifact)
                    (for-each depend-on! (or (artifact-sources artifact) '())))
                  artifacts)
                artifacts))))
         (dependencies
          (sort (map (lambda (path)
                       (list path (input-fingerprint path)))
                     (trace-inputs trace))
                (lambda (left right) (string<? (car left) (car right)))))
         (sources (pipeline-artifact-sources inputs dependencies)))
    (let* ((entries
            (map (lambda (artifact)
                   (let ((content (artifact-content artifact)))
                     (cons (artifact-path artifact)
                       (if cache?
                           (store-content-blob! content)
                           (content-fingerprint content)))))
                 artifact-list))
           (outputs
            (map (lambda (artifact)
                   ((if (string? (artifact-content artifact))
                        make-artifact make-byte-artifact)
                    (artifact-path artifact) (artifact-content artifact) sources))
                 artifact-list)))
      (when (and cache?
                 (every (lambda (entry) (cdr entry)) entries))
        (write-pipeline-index identity dependencies entries))
      outputs)))

(define* (run-asset-pipelines pipelines project-key #:key (cache? #t))
  "Run every declared asset pipeline; return (artifacts . owners),
where each owner maps the artifact path to the pipeline name for the
build state's renderer field. CACHE? controls reads and writes to the
shared content store."
  (unless (boolean? cache?)
    (error "pipeline cache option must be a boolean" cache?))
  (let loop ((pipelines pipelines) (artifacts '()) (owners '()))
    (if (null? pipelines)
        (cons (reverse artifacts) (reverse owners))
        (let* ((pipeline (car pipelines))
               (name (asset-pipeline-name pipeline))
               (inputs (pipeline-input-table pipeline))
               (identity (pipeline-identity pipeline project-key inputs))
               (produced (or (and cache?
                                  (restore-pipeline-artifacts identity inputs))
                             (run-pipeline-procedure pipeline identity inputs
                                                     cache?)))
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
