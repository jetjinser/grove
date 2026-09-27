;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove output)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64)
  #:use-module (grove output))

(define output-directory
  (format #f "/tmp/grove-output-test-~a" (getpid)))
(define output-path (string-append output-directory "/nested/page.txt"))
(define obsolete-path (string-append output-directory "/obsolete.html"))
(define unmanaged-path (string-append output-directory "/user.txt"))
(define manifest-path (string-append output-directory "/.grove-artifacts"))

(test-begin "grove output")

(dynamic-wind
  (lambda () #t)
  (lambda ()
    (write-artifacts output-directory
      (list (make-artifact "nested/page.txt" "artifact content")
            (make-artifact "obsolete.html" "old page")))
    (test-assert "artifact writer creates nested output directories"
      (file-exists? output-path))
    (test-equal "artifact writer writes artifact contents"
      "artifact content"
      (call-with-input-file output-path get-string-all))
    (call-with-output-file unmanaged-path
      (lambda (port) (display "user-owned" port)))
    (write-artifacts output-directory
      (list (make-artifact "nested/page.txt" "updated content")))
    (test-assert "artifact writer removes outputs absent from the next build"
      (not (file-exists? obsolete-path)))
    (test-equal "artifact writer preserves files absent from its manifest"
      "user-owned"
      (call-with-input-file unmanaged-path get-string-all))
    (test-equal "artifact writer refreshes current artifact contents"
      "updated content"
      (call-with-input-file output-path get-string-all))
    (write-artifacts output-directory '())
    (test-assert "empty builds remove previously generated artifacts"
      (not (file-exists? output-path)))
    (test-error "artifact manifest path is reserved" #t
      (write-artifacts output-directory
        (list (make-artifact ".grove-artifacts" "user data"))))
    (test-error "artifact manifest directory is reserved" #t
      (write-artifacts output-directory
        (list (make-artifact ".grove-artifacts/data" "user data")))))
  (lambda ()
    (when (file-exists? output-path)
      (delete-file output-path))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list obsolete-path unmanaged-path manifest-path
            (string-append output-directory "/.grove-artifacts.tmp")))
    (let ((nested (string-append output-directory "/nested")))
      (when (file-exists? nested)
        (rmdir nested)))
    (when (file-exists? output-directory)
      (rmdir output-directory))))

(test-end "grove output")
