;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove output)
  #:use-module (grove private artifact-store)
  #:use-module (grove private cache-dir)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:use-module (grove string-fun)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (artifact?
            artifact-path?
            make-artifact
            make-byte-artifact
            artifact-path
            artifact-content
            artifact-sources
            prefix-artifacts
            validate-artifacts
            write-artifacts))

(define-record-type <artifact>
  (%make-artifact path content sources)
  artifact?
  (path artifact-path)
  (content artifact-content)
  (sources artifact-sources))

(define (artifact-path? path)
  (and (non-empty-string? path)
       (every (lambda (part)
                (and (not (string-null? part))
                     (not (member part '("." "..")))))
              (string-split path #\/))))

(define (make-artifact* path content sources)
  (unless (artifact-path? path)
    (error "artifact requires a relative path" path))
  (unless (or (not sources)
              (and (list? sources) (every string? sources)))
    (error "artifact sources must be #f or a list of paths" sources))
  (%make-artifact path content
                  (and sources (delete-duplicates sources string=?))))

(define* (make-artifact path content #:optional (sources #f))
  (unless (string? content)
    (error "artifact requires text content" content))
  (make-artifact* path content sources))

(define* (make-byte-artifact path content #:optional (sources #f))
  (unless (bytevector? content)
    (error "byte artifact requires a bytevector" content))
  (make-artifact* path content sources))

(define (prefix-artifacts prefix artifacts)
  (unless (artifact-path? prefix)
    (error "artifact prefix requires a relative path" prefix))
  (map (lambda (artifact)
         ((if (string? (artifact-content artifact)) make-artifact
              make-byte-artifact)
          (string-append prefix "/" (artifact-path artifact))
          (artifact-content artifact) (artifact-sources artifact)))
       artifacts))

(define artifact-manifest-name ".grove-artifacts")
(define artifact-manifest-temp-name ".grove-artifacts.tmp")

(define legacy-metadata-names
  '(".grove-artifacts" ".grove-artifacts.tmp"
    ".grove-build-state" ".grove-build-state.tmp"
    ".grove-input-cache"
    ".grove-build-lock"))

(define (delete-tree path)
  (if (file-is-directory? path)
      (begin
        (for-each (lambda (entry)
                    (unless (member entry '("." ".."))
                      (delete-tree (string-append path "/" entry))))
                  (scandir path))
        (rmdir path))
      (delete-file path)))

(define (delete-legacy-metadata directory)
  ;;     Earlier versions kept these inside the output directory.
  (for-each (lambda (name)
              (let ((path (string-append directory "/" name)))
                (when (file-exists? path)
                  (delete-tree path))))
            legacy-metadata-names))

(define (reserved-artifact-path? path)
  (any (lambda (reserved)
         (or (string=? path reserved)
             (string-prefix? (string-append reserved "/") path)))
       (list artifact-manifest-name artifact-manifest-temp-name
             ".grove-build-state" ".grove-build-state.tmp"
             ".grove-input-cache" ".grove-build-lock")))

(define (read-artifact-manifest path)
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (let ((manifest (read port)))
            (unless (and (list? manifest)
                         (= (length manifest) 2)
                         (eq? (car manifest) 'grove-artifacts-v1)
                         (list? (cadr manifest))
                         (every (lambda (artifact-path)
                                  (and (artifact-path? artifact-path)
                                       (not (reserved-artifact-path?
                                              artifact-path))))
                                (cadr manifest))
                         (eof-object? (read port)))
              (error "invalid Grove artifact manifest" path manifest))
            (cadr manifest))))))

(define (write-artifact-manifest path temp-path paths)
  (call-with-output-file temp-path
    (lambda (port)
      (set-port-encoding! port "UTF-8")
      (write (list 'grove-artifacts-v1 paths) port)
      (newline port)))
  (rename-file temp-path path))

(define (artifact-content-current? path content)
  (and (file-exists? path)
       (not (file-is-directory? path))
       (catch #t
         (lambda ()
           (if (string? content)
               (call-with-input-file path
                 (lambda (port)
                   (set-port-encoding! port "UTF-8")
                   (string=? content (get-string-all port))))
               (let ((port (open-file path "rb")))
                 (dynamic-wind
                   (lambda () #t)
                   (lambda () (bytevector=? content (get-bytevector-all port)))
                   (lambda () (close-port port))))))
         (lambda args #f))))

(define (validate-artifacts artifacts)
  "Validate the complete output set without accessing the filesystem."
  (unless (and (list? artifacts) (every artifact? artifacts))
    (error "renderers must return a list of artifacts" artifacts))
  (let ((paths (map artifact-path artifacts)))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "duplicate artifact path" paths))
    (when (any reserved-artifact-path? paths)
      (error "artifact path is reserved for Grove metadata" paths))
    (for-each
      (lambda (path)
        (let loop ((parent (dirname path)))
          (unless (string=? parent ".")
            (when (member parent paths string=?)
              (error "artifact paths conflict as file and directory" parent path))
            (loop (dirname parent)))))
      paths))
  artifacts)

(define (write-artifact-bytes! path content)
  ;;     Staged temp file plus rename: the artifact path may currently
  ;;     be a hard link to a store blob, and in-place truncation would
  ;;     corrupt the shared inode.
  (call-with-staging-file! path
    (lambda (port)
      (if (string? content)
          (display content port)
          (put-bytevector port content)))))

(define (artifact-paths-overlap? left right)
  (or (string=? left right)
      (string-prefix? (string-append left "/") right)
      (string-prefix? (string-append right "/") left)))

(define (remove-empty-artifact-directories directory artifact-path)
  (let loop ((parent (dirname artifact-path)))
    (unless (string=? parent ".")
      (let ((full-path (string-append directory "/" parent)))
        (cond
         ((not (file-exists? full-path))
          (loop (dirname parent)))
         ((not (file-is-directory? full-path)) #t)
         ((null? (filter (lambda (entry)
                           (not (member entry '("." ".."))))
                         (scandir full-path)))
          (rmdir full-path)
          (loop (dirname parent))))))))

(define (remove-stale-artifact! directory stale-path)
  (let ((full-path (string-append directory "/" stale-path)))
    (when (file-exists? full-path)
      (when (file-is-directory? full-path)
        (error "stale artifact path is a directory; refusing to remove"
               full-path))
      (delete-file full-path)
      (remove-empty-artifact-directories directory stale-path))))

(define (write-artifacts directory artifacts)
  (validate-artifacts artifacts)
  (let* ((paths (map artifact-path artifacts))
         (cache-directory (grove-cache-directory directory))
         (manifest-path
           (string-append cache-directory "/" artifact-manifest-name))
         (manifest-temp-path
           (string-append cache-directory "/" artifact-manifest-temp-name))
         (previous-paths (read-artifact-manifest manifest-path))
         (conflicting-stale-paths
           (filter (lambda (stale-path)
                     (and (not (member stale-path paths string=?))
                          (any (lambda (path)
                                 (artifact-paths-overlap? stale-path path))
                               paths)))
                   previous-paths)))
    (ensure-directory directory)
    (ensure-directory cache-directory)
    ;;     Remove only manifest-owned paths that obstruct this build's
    ;;     file/directory layout. Other stale outputs remain until all current
    ;;     artifacts have been written.
    (for-each (lambda (path) (remove-stale-artifact! directory path))
              conflicting-stale-paths)
    (for-each
      (lambda (artifact)
        (let* ((path (string-append directory "/" (artifact-path artifact)))
               (content (artifact-content artifact))
               (fingerprint (content-fingerprint content)))
          (when (and (file-exists? path) (file-is-directory? path))
            (error "artifact path is occupied by a directory; refusing to replace"
                   path))
          (ensure-directory (dirname path))
          (if (artifact-content-current? path content)
              ;;     The bytes are already in place; make sure the
              ;;     shared store has the blob so other checkouts and
              ;;     deleted build directories can restore it.
              (publish-artifact-blob! fingerprint
                (lambda (port)
                  (if (string? content)
                      (display content port)
                      (put-bytevector port content))))
              ;;     A store blob under this fingerprint holds exactly
              ;;     the rendered bytes: restore it, hard-linked when
              ;;     possible, and fall back to writing them.
              (unless (restore-artifact-blob! fingerprint path)
                (write-artifact-bytes! path content)
                (publish-artifact-blob! fingerprint
                  (lambda (port)
                    (if (string? content)
                        (display content port)
                        (put-bytevector port content))))))))
      artifacts)
    (for-each
      (lambda (stale-path)
        (unless (or (member stale-path paths string=?)
                    (member stale-path conflicting-stale-paths string=?))
          (remove-stale-artifact! directory stale-path)))
      previous-paths)
    (write-artifact-manifest manifest-path manifest-temp-path paths)
    (delete-legacy-metadata directory)))
