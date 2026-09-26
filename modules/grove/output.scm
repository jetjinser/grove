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
            prefix-artifacts
            write-artifacts))

(define-record-type <artifact>
  (%make-artifact path content)
  artifact?
  (path artifact-path)
  (content artifact-content))

(define (artifact-path? path)
  (and (string? path)
       (not (string-null? path))
       (every (lambda (part)
                (and (not (string-null? part))
                     (not (member part '("." "..")))))
              (string-split path #\/))))

(define (make-artifact path content)
  (unless (artifact-path? path)
    (error "artifact requires a relative path" path))
  (unless (string? content)
    (error "artifact requires text content" content))
  (%make-artifact path content))

(define (prefix-artifacts prefix artifacts)
  (unless (artifact-path? prefix)
    (error "artifact prefix requires a relative path" prefix))
  (map (lambda (artifact)
         (make-artifact (string-append prefix "/" (artifact-path artifact))
                        (artifact-content artifact)))
       artifacts))

(define (write-artifacts directory artifacts)
  (let ((paths (map artifact-path artifacts)))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "duplicate artifact path" paths)))
  (for-each
    (lambda (artifact)
      (let ((path (string-append directory "/" (artifact-path artifact))))
        (mkdir-p (dirname path))
        (call-with-output-file path
          (lambda (port)
            (display (artifact-content artifact) port)))))
    artifacts))
