;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove output)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (artifact?
            artifact-path?
            make-artifact
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
  (and (string? path)
       (not (string-null? path))
       (every (lambda (part)
                (and (not (string-null? part))
                     (not (member part '("." "..")))))
              (string-split path #\/))))

(define* (make-artifact path content #:optional (sources #f))
  (unless (artifact-path? path)
    (error "artifact requires a relative path" path))
  (unless (string? content)
    (error "artifact requires text content" content))
  (unless (or (not sources)
              (and (list? sources) (every string? sources)))
    (error "artifact sources must be #f or a list of paths" sources))
  (%make-artifact path content
                  (and sources (delete-duplicates sources string=?))))

(define (prefix-artifacts prefix artifacts)
  (unless (artifact-path? prefix)
    (error "artifact prefix requires a relative path" prefix))
  (map (lambda (artifact)
         (make-artifact (string-append prefix "/" (artifact-path artifact))
                        (artifact-content artifact)
                        (artifact-sources artifact)))
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
       (list artifact-manifest-name artifact-manifest-temp-name)))

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
          (call-with-output-file path
            (lambda (port)
              (display (artifact-content artifact) port)))))
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
