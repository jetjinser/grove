;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove output)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:use-module (grove output))

(define output-directory
  (format #f "/tmp/grove-output-test-~a" (getpid)))
(define output-path (string-append output-directory "/nested/page.txt"))
(define obsolete-path (string-append output-directory "/obsolete.html"))
(define unmanaged-path (string-append output-directory "/user.txt"))
(define manifest-path (string-append output-directory "/.grove-artifacts"))
(define binary-path (string-append output-directory "/images/pixel.bin"))

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
    (test-equal "undeclared artifact sources remain unknown"
      #f
      (artifact-sources
        (make-artifact "unknown.txt" "unknown source set")))
    (let ((inode (stat:ino (stat output-path))))
      (write-artifacts output-directory
        (list (make-artifact "nested/page.txt" "artifact content")
              (make-artifact "obsolete.html" "old page")))
      (test-equal "unchanged artifact contents are not rewritten"
        inode
        (stat:ino (stat output-path))))
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
    (let ((bytes (u8-list->bytevector '(0 255 1 128))))
      (write-artifacts output-directory
        (list (make-byte-artifact "images/pixel.bin" bytes)))
      (test-assert "artifact writer preserves binary artifact bytes"
        (let ((port (open-file binary-path "rb")))
          (dynamic-wind
            (lambda () #t)
            (lambda () (bytevector=? bytes (get-bytevector-all port)))
            (lambda () (close-port port)))))
      (write-artifacts output-directory '())
      (test-assert "artifact writer removes stale binary artifacts"
        (not (file-exists? binary-path))))
    (write-artifacts output-directory '())
    (test-assert "empty builds remove previously generated artifacts"
      (not (file-exists? output-path)))
    (with-fluids ((%default-port-encoding "ISO-8859-1"))
      (let* ((text "平方 ◆:x")
             (artifacts (list (make-artifact "nested/page.txt" text))))
        (write-artifacts output-directory artifacts)
        (test-assert "text artifacts use UTF-8 independently of default port encoding"
          (let ((port (open-file output-path "rb")))
            (dynamic-wind
              (lambda () #t)
              (lambda () (bytevector=? (string->utf8 text) (get-bytevector-all port)))
              (lambda () (close-port port)))))
        (utime output-path 0 0)
        (write-artifacts output-directory artifacts)
        (test-equal "UTF-8 comparison preserves unchanged artifacts" 0
          (stat:mtime (stat output-path)))
        (write-artifacts output-directory '())))
    (test-error "artifact manifest path is reserved" #t
      (write-artifacts output-directory
        (list (make-artifact ".grove-artifacts" "user data"))))
    (test-error "artifact manifest directory is reserved" #t
      (write-artifacts output-directory
        (list (make-artifact ".grove-artifacts/data" "user data"))))
    (test-error "build state path is reserved" #t
      (write-artifacts output-directory
        (list (make-artifact ".grove-build-state" "user data"))))
    (test-error "build state directory is reserved" #t
      (write-artifacts output-directory
        (list (make-artifact ".grove-build-state/data" "user data")))))
  (lambda ()
    (when (file-exists? output-path)
      (delete-file output-path))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list obsolete-path unmanaged-path manifest-path
            binary-path
            (string-append output-directory "/.grove-artifacts.tmp")
            (string-append output-directory "/.grove-build-state")
            (string-append output-directory "/.grove-build-state.tmp")))
    (let ((nested (string-append output-directory "/nested")))
      (when (file-exists? nested)
        (rmdir nested)))
    (let ((images (string-append output-directory "/images")))
      (when (file-exists? images)
        (rmdir images)))
    (when (file-exists? output-directory)
      (rmdir output-directory))))

(test-end "grove output")
