;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove static)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:use-module (grove output)
  #:use-module (grove build)
  #:use-module (grove render static))

(define cached-renderer-procedure
  (@@ (grove build) cached-renderer-procedure))
(define cached-renderer-inputs
  (@@ (grove build) cached-renderer-inputs))
(define cached-renderer-render-changed
  (@@ (grove build) cached-renderer-render-changed))

(define static-root (format #f "/tmp/grove-static-test-~a" (getpid)))
(define source-directory (string-append static-root "/static"))
(define asset-path (string-append source-directory "/images/data.bin"))

(test-begin "grove static assets")

(dynamic-wind
  (lambda ()
    (when (file-exists? asset-path) (delete-file asset-path))
    (when (file-exists? (string-append source-directory "/images"))
      (rmdir (string-append source-directory "/images")))
    (when (file-exists? source-directory) (rmdir source-directory))
    (when (file-exists? static-root) (rmdir static-root))
    (mkdir static-root)
    (mkdir source-directory)
    (mkdir (string-append source-directory "/images"))
    (let ((port (open-file asset-path "wb")))
      (put-bytevector port (u8-list->bytevector '(0 255 2)))
      (close-port port)))
  (lambda ()
    (let* ((renderer (make-static-assets-renderer
                       #:source-directory source-directory
                       #:prefix "assets"))
           (artifact
             (car ((cached-renderer-procedure renderer) #f))))
      (test-equal "static renderer preserves relative paths with prefix"
        "assets/images/data.bin" (artifact-path artifact))
      (test-assert "static renderer reads binary file contents"
        (bytevector=? (artifact-content artifact)
                      (u8-list->bytevector '(0 255 2))))
      (test-equal "static renderer declares the source dependency"
        (list asset-path) (artifact-sources artifact))
      (test-equal "static renderer tracks its source files as project inputs"
        (list asset-path)
        ((cached-renderer-inputs renderer)))
      (let ((port (open-file asset-path "wb")))
        (put-bytevector port (u8-list->bytevector '(3 4 5)))
        (close-port port))
      (let* ((rendered
               ((cached-renderer-render-changed renderer)
                #f (list asset-path) (list "assets/images/data.bin")
                (list artifact)))
             (updated (car rendered)))
        (test-assert "incremental renderer rereads changed bytes"
          (bytevector=? (artifact-content updated)
                        (u8-list->bytevector '(3 4 5))))
        (delete-file asset-path)
        (test-equal "incremental renderer drops removed assets"
          '()
          ((cached-renderer-render-changed renderer)
           #f (list asset-path) (list "assets/images/data.bin")
           (list updated))))))
  (lambda ()
    (when (file-exists? asset-path) (delete-file asset-path))
    (when (file-exists? (string-append source-directory "/images"))
      (rmdir (string-append source-directory "/images")))
    (when (file-exists? source-directory) (rmdir source-directory))
    (when (file-exists? static-root) (rmdir static-root))))

(test-end "grove static assets")
