;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove output)
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

(define (ensure-directory directory)
  (unless (or (string=? directory ".")
              (string=? directory "/")
              (file-exists? directory))
    (let ((parent (dirname directory)))
      (unless (string=? parent directory)
        (ensure-directory parent))
      (mkdir directory))))

(define artifact-manifest-name ".grove-artifacts")
(define artifact-manifest-temp-name ".grove-artifacts.tmp")

(define (reserved-artifact-path? path)
  (any (lambda (reserved)
         (or (string=? path reserved)
             (string-prefix? (string-append reserved "/") path)))
       (list artifact-manifest-name artifact-manifest-temp-name
             ".grove-build-state" ".grove-build-state.tmp")))

(define (read-artifact-manifest path)
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
        (lambda (port)
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
                   (string=? content (get-string-all port))))
               (let ((port (open-file path "rb")))
                 (dynamic-wind
                   (lambda () #t)
                   (lambda () (bytevector=? content (get-bytevector-all port)))
                   (lambda () (close-port port))))))
         (lambda args #f))))

(define (write-artifacts directory artifacts)
  (let* ((paths (map artifact-path artifacts))
         (manifest-path
           (string-append directory "/" artifact-manifest-name))
         (manifest-temp-path
           (string-append directory "/" artifact-manifest-temp-name))
         (previous-paths (read-artifact-manifest manifest-path)))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "duplicate artifact path" paths))
    (when (any reserved-artifact-path? paths)
      (error "artifact path is reserved for Grove metadata" paths))
    (ensure-directory directory)
    (for-each
      (lambda (artifact)
        (let ((path (string-append directory "/" (artifact-path artifact))))
          (ensure-directory (dirname path))
          (unless (artifact-content-current? path (artifact-content artifact))
            (if (string? (artifact-content artifact))
                (call-with-output-file path
                  (lambda (port)
                    (display (artifact-content artifact) port)))
                (let ((port (open-file path "wb")))
                  (dynamic-wind
                    (lambda () #t)
                    (lambda ()
                      (put-bytevector port (artifact-content artifact)))
                    (lambda () (close-port port))))))))
      artifacts)
    (for-each
      (lambda (stale-path)
        (unless (member stale-path paths string=?)
          (let ((full-path (string-append directory "/" stale-path)))
            (when (file-is-directory? full-path)
              (error "stale artifact path is a directory; refusing to remove"
                     full-path))
            (when (file-exists? full-path)
              (delete-file full-path)))))
      previous-paths)
    (write-artifact-manifest manifest-path manifest-temp-path paths)))
