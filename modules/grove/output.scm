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
  "Return ARTIFACT's tracked input paths, #f when its dependencies are unknown.
An empty list means the artifact is known to have no file dependencies."
  (snapshot-data (artifact:artifact-sources/raw artifact)))

(define (artifact-path? path)
  (and (filesystem-path-string? path)
       (every (lambda (part)
                (and (not (string-null? part))
                     (not (member part '("." "..")))))
              (string-split path #\/))))

(define (make-artifact* path content sources)
  (unless (artifact-path? path)
    (error "artifact requires a relative path" path))
  (unless (or (not sources)
              (and (list? sources) (every filesystem-path-string? sources)))
    (error "artifact sources must be #f or a list of paths" sources))
  (when (and sources
             (not (= (length sources)
                     (length (delete-duplicates sources string=?)))))
    (error "artifact source paths must be unique" sources))
  (artifact:%make-artifact (snapshot-data path)
                  (if (string? content) (string-copy content)
                      (bytevector-copy content))
                  (and sources (snapshot-data sources))))

(define* (make-artifact path content #:optional (sources #f))
  "Construct a text artifact at PATH with optional source dependencies.

SOURCES is a list of input paths, or #f when the dependencies are unknown.
An explicit empty list asserts that the artifact depends on no input files."
  (unless (string? content)
    (error "artifact requires text content" content))
  (make-artifact* path content sources))

(define* (make-byte-artifact path content #:optional (sources #f))
  "Construct a byte artifact at PATH with optional source dependencies.

SOURCES is a list of input paths, or #f when the dependencies are unknown.
An explicit empty list asserts that the artifact depends on no input files."
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

(define (safe-artifact-path directory path create-parents?)
  "Resolve PATH below DIRECTORY without following artifact parent symlinks.

The output directory itself may be a symlink chosen by the project. Once
inside that root, every artifact parent must be an ordinary directory. Return
#f when a parent is missing and CREATE-PARENTS? is false."
  (let* ((parts (string-split path #\/))
         (filename (last parts)))
    (let loop ((parent directory) (components (drop-right parts 1)))
      (if (null? components)
          (string-append parent "/" filename)
          (let* ((next (string-append parent "/" (car components)))
                 (info (false-if-exception (lstat next))))
            (cond
             ((not info)
              (if (not create-parents?)
                  #f
                  (begin
                    ;; Recheck after mkdir so a concurrent creator is accepted
                    ;; only when it made a real directory, not a symlink.
                    (false-if-exception (mkdir next))
                    (let ((created (false-if-exception (lstat next))))
                      (unless (and created (eq? (stat:type created) 'directory))
                        (error "artifact parent is not a real directory" next))
                      (loop next (cdr components))))))
             ((eq? (stat:type info) 'directory)
              (loop next (cdr components)))
             (else
              (error "artifact parent must not be a symlink or file" next))))))))

(define (remove-stale-artifact! directory stale-path)
  (let ((full-path (safe-artifact-path directory stale-path #f)))
    (when (and full-path (file-exists? full-path))
      (when (file-is-directory? full-path)
        (error "stale artifact path is a directory; refusing to remove"
               full-path))
      (delete-file full-path)
      (remove-empty-artifact-directories directory stale-path))))

(define (write-artifacts directory artifacts)
  (validate-artifacts artifacts)
  (ensure-directory directory)
  ;; A configured output root may itself be a symlink. Resolve it once, then
  ;; refuse symlinks below it so artifact paths cannot escape that root.
  (set! directory (canonicalize-path directory))
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
    (ensure-directory cache-directory)
    ;;     Remove only manifest-owned paths that obstruct this build's
    ;;     file/directory layout. Other stale outputs remain until all current
    ;;     artifacts have been written.
    (for-each (lambda (path) (remove-stale-artifact! directory path))
              conflicting-stale-paths)
    (for-each
      (lambda (artifact)
        (let* ((path (safe-artifact-path
                       directory (artifact:artifact-path/raw artifact) #t))
               (content (artifact:artifact-content/raw artifact))
               (fingerprint (content-fingerprint content)))
          (when (and (file-exists? path) (file-is-directory? path))
            (error "artifact path is occupied by a directory; refusing to replace"
                   path))
          (if (artifact-content-current? path content)
              ;;     The bytes are already in place; make sure the
              ;;     shared store has the blob so other checkouts and
              ;;     deleted build directories can restore it.
              (store-content-blob! content)
              ;;     A store blob under this fingerprint holds exactly
              ;;     the rendered bytes: restore it, hard-linked when
              ;;     possible, and fall back to writing them.
              (unless (restore-artifact-blob! fingerprint path)
                (write-artifact-bytes! path content)
                (store-content-blob! content)))))
      artifacts)
    (for-each
      (lambda (stale-path)
        (unless (or (member stale-path paths string=?)
                    (member stale-path conflicting-stale-paths string=?))
          (remove-stale-artifact! directory stale-path)))
      previous-paths)
    (write-artifact-manifest manifest-path manifest-temp-path paths)))
