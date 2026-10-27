;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove output)
  #:use-module ((grove private artifact) #:prefix artifact:)
  #:use-module (grove private snapshot)
  #:use-module (grove private artifact-store)
  #:use-module (grove private cache-dir)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:use-module (grove private string-fun)
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

(define (artifact? value)
  (artifact:artifact? value))

(define (artifact-path artifact)
  (snapshot-data (artifact:artifact-path/raw artifact)))

(define (artifact-content artifact)
  (let ((content (artifact:artifact-content/raw artifact)))
    (if (string? content) (string-copy content) (bytevector-copy content))))

(define (artifact-sources artifact)
  (snapshot-data (artifact:artifact-sources/raw artifact)))

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
              (and (list? sources) (every non-empty-string? sources)))
    (error "artifact sources must be #f or a list of paths" sources))
  (artifact:%make-artifact (snapshot-data path)
                  (if (string? content) (string-copy content)
                      (bytevector-copy content))
                  (and sources
                       (snapshot-data
                         (delete-duplicates sources string=?)))))

(define* (make-artifact path content #:optional (sources #f))
  (unless (string? content)
    (error "artifact requires text content" content))
  (make-artifact* path content sources))

(define* (make-byte-artifact path content #:optional (sources #f))
  (unless (bytevector? content)
    (error "byte artifact requires a bytevector" content))
  (make-artifact* path content sources))

(define (prefix-artifacts prefix artifacts)
  "Prefix every artifact path while preserving its content and sources.
Validate ARTIFACTS as one complete set before prefixing it."
  (unless (artifact-path? prefix)
    (error "artifact prefix requires a relative path" prefix))
  (validate-artifacts artifacts)
  (map (lambda (artifact)
         ((if (string? (artifact:artifact-content/raw artifact)) make-artifact
              make-byte-artifact)
          (string-append prefix "/" (artifact:artifact-path/raw artifact))
          (artifact:artifact-content/raw artifact)
          (artifact:artifact-sources/raw artifact)))
       artifacts))

(define artifact-manifest-name ".grove-artifacts")
(define artifact-manifest-temp-name ".grove-artifacts.tmp")

(define (read-artifact-manifest path)
  (if (not (regular-file? path #:follow-links? #f))
      '()
      (call-with-input-file path
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (let ((manifest (read port)))
            (unless (and (list? manifest)
                         (= (length manifest) 2)
                         (eq? (car manifest) 'grove-artifacts-v1)
                         (list? (cadr manifest))
                         (every artifact-path? (cadr manifest))
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
  (and (regular-file? path #:follow-links? #f)
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
    (error "expected a list of artifacts" artifacts))
  (let ((paths (map artifact:artifact-path/raw artifacts)))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "duplicate artifact path" paths))
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
  (let* ((paths (map artifact:artifact-path/raw artifacts))
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
        (let* ((path (string-append directory "/" (artifact:artifact-path/raw artifact)))
               (content (artifact:artifact-content/raw artifact))
               (fingerprint (content-fingerprint content)))
          (when (and (file-exists? path) (file-is-directory? path))
            (error "artifact path is occupied by a directory; refusing to replace"
                   path))
          (ensure-directory (dirname path))
          (if (artifact-content-current? path content)
              ;;     The bytes are already in place; make sure the
              ;;     shared store has the blob so other checkouts and
              ;;     deleted build directories can restore it.
              (publish-content-blob! content)
              ;;     A store blob under this fingerprint holds exactly
              ;;     the rendered bytes: restore it, hard-linked when
              ;;     possible, and fall back to writing them.
              (unless (restore-artifact-blob! fingerprint path)
                (write-artifact-bytes! path content)
                (publish-content-blob! content)))))
      artifacts)
    (for-each
      (lambda (stale-path)
        (unless (or (member stale-path paths string=?)
                    (member stale-path conflicting-stale-paths string=?))
          (remove-stale-artifact! directory stale-path)))
      previous-paths)
    (write-artifact-manifest manifest-path manifest-temp-path paths)))
