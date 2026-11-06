;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private pipeline)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private asset-pipeline)
  #:use-module (grove output)
  #:use-module (grove private artifact-store)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:use-module (grove private input dependencies)
  #:use-module (grove private procedure)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (run-asset-pipelines
            merge-pipeline-artifacts))

(define (pipeline-output-inputs pipeline)
  (filter pipeline-output? (asset-pipeline-inputs pipeline)))

(define (pipeline-declaration-order pipelines)
  (let ((states '()) (ordered '()))
    (define (state pipeline)
      (let ((entry (assq pipeline states))) (and entry (cdr entry))))
    (define (visit pipeline)
      (case (state pipeline)
        ((done) #t)
        ((visiting) (error "asset pipeline dependency cycle"
                           (asset-pipeline-name pipeline)))
        (else
         (set! states (acons pipeline 'visiting states))
         (for-each
           (lambda (reference)
             (let ((producer (pipeline-output-pipeline reference)))
               (unless (memq producer pipelines)
                 (error "asset pipeline producer is not included in project"
                        (asset-pipeline-name producer)
                        (asset-pipeline-name pipeline)))
               (visit producer)))
           (pipeline-output-inputs pipeline))
         (set! states (assq-set! states pipeline 'done))
         (set! ordered (append ordered (list pipeline))))))
    (for-each visit pipelines)
    ordered))

(define (pipeline-consumed-output? pipeline path pipelines)
  (any (lambda (consumer)
         (any (lambda (reference)
                (and (eq? pipeline (pipeline-output-pipeline reference))
                     (string=? path (pipeline-output-path reference))))
              (pipeline-output-inputs consumer)))
       pipelines))

(define (pipeline-input-table pipeline produced)
  (map (lambda (input)
         (if (string? input)
             (cons input (depend-on! input))
             (let* ((producer (pipeline-output-pipeline input))
                    (path (pipeline-output-path input))
                    (artifacts (assq-ref produced producer))
                    (artifact (and artifacts
                                   (find (lambda (candidate)
                                           (string=? path
                                             (out:artifact-path/raw candidate)))
                                         artifacts))))
               (unless artifact
                 (error "asset pipeline did not produce referenced output"
                        (asset-pipeline-name producer) path
                        (asset-pipeline-name pipeline)))
               (cons input artifact))))
       (asset-pipeline-inputs pipeline)))

(define (pipeline-input-description input-table)
  (map (lambda (entry)
         (let ((input (car entry)) (value (cdr entry)))
           (if (string? input)
               (list 'file input value (input-fingerprint value))
               (list 'output
                     (asset-pipeline-name (pipeline-output-pipeline input))
                     (pipeline-output-path input)
                     (content-fingerprint (out:artifact-content/raw value))))))
       input-table))

(define (pipeline-input-sources input-table)
  (delete-duplicates
    (append-map
      (lambda (entry)
        (if (string? (car entry))
            (list (cdr entry))
            (or (out:artifact-sources/raw (cdr entry)) '())))
      input-table)
    string=?))

(define (pipeline-artifact-sources inputs dependencies)
  (delete-duplicates
    (append (pipeline-input-sources inputs) (map car dependencies))
    string=?))

(define (pipeline-identity pipeline project-key input-table)
  (scheme-datum-fingerprint
    (list "grove-asset-pipeline"
          (asset-pipeline-name pipeline)
          (asset-pipeline-version pipeline)
          project-key
          (pipeline-input-description input-table))))

(define (restore-index-artifacts entries sources)
  (let loop ((remaining entries) (artifacts '()))
    (if (null? remaining)
        (reverse artifacts)
        (let* ((entry (car remaining))
               (bytes (read-content-blob (cdr entry))))
          (and bytes
               (loop (cdr remaining)
                 (cons (make-byte-artifact (car entry) bytes #:sources sources)
                       artifacts)))))))

(define (valid-cached-artifact-set? artifacts)
  ;; An index can be well-formed Scheme data while still describing an
  ;; impossible artifact set. Treat that as a cache miss and regenerate it.
  (catch #t
    (lambda () (validate-artifacts artifacts) #t)
    (lambda args #f)))

(define (restore-pipeline-artifacts identity inputs)
  (let ((index (read-pipeline-index identity)))
    (and index
         (every (lambda (dependency)
                  (equal? (cadr dependency)
                          (input-fingerprint (car dependency))))
                (car index))
         (let* ((sources (pipeline-artifact-sources inputs (car index)))
                (artifacts (restore-index-artifacts (cadr index) sources)))
           (and artifacts
                (valid-cached-artifact-set? artifacts)
                (list artifacts (map car (car index))))))))

(define (valid-pipeline-index? index)
  (and (list? index)
       (= (length index) 4)
       (eq? (car index) 'grove-pipeline-index)
       (= (cadr index) 1)
       (list? (caddr index))
       (every (lambda (entry)
                (and (list? entry)
                     (= (length entry) 2)
                     (filesystem-path-string? (car entry))
                     (fingerprint? (cadr entry))))
              (caddr index))
       (unique-filesystem-path-list? (map car (caddr index)))
       (list? (cadddr index))
       (every (lambda (entry)
                (and (pair? entry)
                     (artifact-path? (car entry))
                     (fingerprint? (cdr entry))))
              (cadddr index))))

(define (read-pipeline-index identity)
  (let ((index (read-artifact-index identity valid-pipeline-index?)))
    (and index (list (caddr index) (cadddr index)))))

(define (write-pipeline-index identity dependencies entries)
  (write-artifact-index!
    identity (list 'grove-pipeline-index 1 dependencies entries)))

(define (materialize-input! workspace pipeline reference artifact)
  (let* ((relative (pipeline-output-path reference))
         (key (scheme-datum-fingerprint
                (list "grove-pipeline-input"
                      (asset-pipeline-name pipeline)
                      (asset-pipeline-name
                        (pipeline-output-pipeline reference))
                      relative)))
         (path (string-append workspace "/" key "/" (basename relative)))
         (parent (dirname path))
         (content (out:artifact-content/raw artifact)))
    (ensure-directory parent)
    (if (string? content)
        (call-with-output-file path
          (lambda (port)
            (set-port-encoding! port "UTF-8")
            (display content port)))
        (let ((port (open-file path "wb")))
          (dynamic-wind
            (lambda () #t)
            (lambda () (put-bytevector port content))
            (lambda () (close-port port)))))
    path))

(define (resolve-pipeline-input workspace pipeline input-table requested)
  (let ((entry (find (lambda (candidate)
                       (pipeline-input-key=? requested (car candidate)))
                     input-table)))
    (unless entry
      (error "asset pipeline asked for an undeclared input" requested))
    (if (string? (car entry))
        (cdr entry)
        (materialize-input! workspace pipeline (car entry) (cdr entry)))))

(define (run-pipeline-procedure pipeline identity inputs workspace cache?)
  (let* ((trace (make-hash-table))
         (artifact-list
          (call-with-input-trace trace
            (lambda ()
              (let ((artifacts
                      ((asset-pipeline-procedure pipeline)
                       (lambda (declared)
                         (resolve-pipeline-input workspace pipeline inputs
                                                 declared)))))
                (unless (and (list? artifacts) (every artifact? artifacts))
                  (error "asset pipeline must return a list of artifacts"
                         (asset-pipeline-name pipeline) artifacts))
                (validate-artifacts artifacts)
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
                    (artifact-path artifact) (artifact-content artifact)
                    #:sources sources))
                 artifact-list)))
      (when (and cache?
                 (every (lambda (entry) (cdr entry)) entries))
        (write-pipeline-index identity dependencies entries))
      (list outputs (map car dependencies)))))

(define (remove-tree path)
  (let ((info (false-if-exception (lstat path))))
    (when info
      (if (eq? (stat:type info) 'directory)
          (begin
            (for-each (lambda (name)
                        (unless (member name '("." ".."))
                          (remove-tree (string-append path "/" name))))
                      (scandir path))
            (rmdir path))
          (delete-file path)))))

(define* (run-asset-pipelines pipelines project-key #:key (cache? #t)
                             (progress (lambda (_stage _detail) #t)))
  "Run declared asset pipelines in dependency order; return published artifacts,
owners, and runtime inputs. Output references are materialized only on a cache
miss. CACHE? controls reads and writes to the shared content store. PROGRESS
receives a `pipeline-start` event with the pipeline name and a `pipeline` event
with its name, cache status, elapsed seconds, and output count."
  (unless (boolean? cache?)
    (error "pipeline cache option must be a boolean" cache?))
  (unless (procedure-accepts-arguments? progress 2)
    (error "pipeline progress callback must accept a stage and detail"
           progress))
  (let ((ordered (pipeline-declaration-order pipelines))
        (workspace #f))
    (define (ensure-workspace!)
      (or workspace
          (set! workspace
            (mkdtemp (string-append
                       (or (getenv "TMPDIR") "/tmp")
                       "/grove-pipeline-XXXXXX"))))
      workspace)
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let loop ((remaining ordered) (produced '()) (published '())
                   (owners '()) (runtime-inputs '()))
          (if (null? remaining)
              (list (reverse published) (reverse owners)
                    (delete-duplicates (reverse runtime-inputs) string=?))
              (let* ((pipeline (car remaining))
                     (name (asset-pipeline-name pipeline))
                     (started (get-internal-real-time))
                     (_started (progress 'pipeline-start name))
                     (inputs (pipeline-input-table pipeline produced))
                     (identity (pipeline-identity pipeline project-key inputs))
                     (cached (and cache?
                                  (restore-pipeline-artifacts identity inputs)))
                     (execution
                       (or cached
                           (begin
                             (when (pair? (pipeline-output-inputs pipeline))
                               (ensure-workspace!))
                             (run-pipeline-procedure pipeline identity inputs
                                                     workspace cache?))))
                     (all-produced (car execution))
                     (dependencies (cadr execution))
                     (status (cond (cached 'hit)
                                   (cache? 'miss)
                                   (else 'disabled)))
                     (seconds (/ (- (get-internal-real-time) started)
                                 internal-time-units-per-second))
                     (_finished (progress 'pipeline
                                          (list name status seconds
                                                (length all-produced))))
                     (published-here
                       (if (asset-pipeline-publish-consumed? pipeline)
                           all-produced
                           (remove
                             (lambda (artifact)
                               (pipeline-consumed-output?
                                 pipeline (out:artifact-path/raw artifact)
                                 pipelines))
                             all-produced)))
                     (markers
                       (map (lambda (artifact)
                              (cons (out:artifact-path/raw artifact) name))
                            published-here)))
                (loop (cdr remaining)
                      (acons pipeline all-produced produced)
                      (append (reverse published-here) published)
                      (append (reverse markers) owners)
                      (append (reverse dependencies) runtime-inputs))))))
      (lambda () (when workspace (remove-tree workspace))))))

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
