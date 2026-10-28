;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove state)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove private build-state)
  #:use-module (grove output))

(define state-directory
  (format #f "/tmp/grove-state-test-~a" (getpid)))
(define input-path (string-append state-directory "/source.plant"))
(define state-path (string-append state-directory "/state"))
(define temp-path (string-append state-directory "/state.tmp"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(define (write-test-datum path datum)
  (call-with-output-file path
    (lambda (port) (write datum port))))

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
    (test-assert "content fingerprints accept binary artifact bytes"
      (equal? (content-fingerprint (u8-list->bytevector '(0 255 1)))
              (content-fingerprint (u8-list->bytevector '(0 255 1)))))
    (let* ((sources (list (list input-path (file-fingerprint input-path) 'source)))
           (artifacts
             (list (make-artifact "page.html" "rendered"
                                  #:sources (list input-path))))
           (identities (list (content-fingerprint "renderer")))
           (state (list 'grove-build-state
                        sources
                        (list (list "page.html"
                                    (content-fingerprint "rendered")
                                    (list input-path)
                                    0
                                    'text))
                        identities)))
      (write-build-state state-path temp-path sources artifacts
                         '(("page.html" . 0))
                         identities)
      (test-equal "build state round trips input and artifact provenance"
        state
        (read-build-state state-path))
      (let ((identities (list (content-fingerprint "html") #f)))
        (write-build-state state-path temp-path sources artifacts
                           '(("page.html" . 0)) identities)
        (test-equal "build state round trips per-renderer cache identities"
          identities
          (build-state-identity (read-build-state state-path))))
      (write-test-file input-path "other")
      (test-assert "file fingerprints read source bytes"
        (equal? (cadar sources)
                (content-fingerprint "first")))
      (test-assert "file fingerprints detect source changes"
        (not (equal? (cadar sources)
                     (file-fingerprint input-path))))
      (write-test-file state-path
        "(grove-build-state ((\"source.plant\" #f)) () #f)")
      (test-equal "source entries without a kind are discarded"
        #f
        (read-build-state state-path))
      (write-test-file state-path
        "(grove-build-state () () \"legacy-renderer-set-fingerprint\")")
      (test-equal "legacy renderer-set identities are discarded"
        #f
        (read-build-state state-path))
      (write-test-file state-path
        "(grove-build-state-v4 () () #f)")
      (test-equal "previous versioned build states are discarded"
        #f
        (read-build-state state-path))
      (write-test-file state-path "(grove-build-state-v9 () () #f)")
      (test-equal "unknown build-state versions are discarded"
        #f
        (read-build-state state-path))
      (write-test-file state-path "(grove-build-state")
      (test-equal "a syntactically corrupt build state is discarded"
        #f
        (read-build-state state-path))
      (write-test-datum state-path
        (list 'grove-build-state
              (list (list (string #\nul) #f 'source))
              '() #f))
      (test-equal "source paths with NUL characters are discarded"
        #f
        (read-build-state state-path))
      (write-test-datum state-path
        (list 'grove-build-state '()
              (list (list "page.html" (content-fingerprint "rendered")
                          (list (string #\nul)) #f 'text))
              #f))
      (test-equal "artifact dependency paths with NUL characters are discarded"
        #f
        (read-build-state state-path))
      (write-test-datum state-path
        (list 'grove-build-state '()
              (list (list "page.html" (content-fingerprint "rendered")
                          '("source.plant" "source.plant") #f 'text))
              #f))
      (test-equal "duplicate artifact dependency paths are discarded"
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
