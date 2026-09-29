;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove state)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove build state)
  #:use-module (grove output))

(define state-directory
  (format #f "/tmp/grove-state-test-~a" (getpid)))
(define input-path (string-append state-directory "/source.grv"))
(define state-path (string-append state-directory "/state"))
(define temp-path (string-append state-directory "/state.tmp"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(test-begin "grove build state")

(dynamic-wind
  (lambda ()
    (mkdir state-directory)
    (write-test-file input-path "first"))
  (lambda ()
    (test-equal "content fingerprints are stable"
      (content-fingerprint "same")
      (content-fingerprint "same"))
    (test-equal "content fingerprints use SHA-256"
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      (content-fingerprint "abc"))
    (test-assert "content fingerprints detect different content"
      (not (equal? (content-fingerprint "first")
                   (content-fingerprint "other"))))
    (let* ((sources (list (list input-path (file-fingerprint input-path))))
           (artifacts
             (list (make-artifact "page.html" "rendered"
                                  (list input-path))))
           (state (list 'grove-build-state-v4
                        sources
                        (list (list "page.html"
                                    (content-fingerprint "rendered")
                                    (list input-path)
                                    0))
                        "test-project-renderers-v1")))
      (write-build-state state-path temp-path sources artifacts
                         '(("page.html" . 0))
                         "test-project-renderers-v1")
      (test-equal "build state round trips input and artifact provenance"
        state
        (read-build-state state-path))
      (write-test-file input-path "other")
      (test-assert "file fingerprints read source bytes"
        (equal? (cadar sources)
                (content-fingerprint "first")))
      (test-assert "file fingerprints detect source changes"
        (not (equal? (cadar sources)
                     (file-fingerprint input-path))))
      (write-test-file state-path
        "(grove-build-state-v3 () () #f)")
      (test-equal "older build states are discarded"
        #f
        (read-build-state state-path))
      (write-test-file state-path "(grove-build-state-v9 () () #f)")
      (test-equal "unknown build-state versions are discarded"
        #f
        (read-build-state state-path))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list input-path state-path temp-path))
    (when (file-exists? state-directory)
      (rmdir state-directory))))

(test-end "grove build state")
