;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove artifact-store)
  #:use-module (grove private artifact-store)
  #:use-module (grove private fingerprint)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))

(test-begin "grove artifact store")

;;     Both store and outputs live under a process-unique root. Remove
;;     leftovers in case the operating system has reused this PID.
(define root
  (string-append "/tmp/kilo/artifact-store-test-"
                 (number->string (getpid))))
(define store (string-append root "/store"))
(define output (string-append root "/output"))

(system* "rm" "-rf" root)
(setenv "GROVE_STORE_DIR" store)

(define text "hello rendered page\n")
(define fingerprint (content-fingerprint text))

(test-assert "absent blob does not exist"
  (not (artifact-blob-exists? fingerprint)))

(test-equal "restore from an absent blob reports failure"
  #f
  (restore-artifact-blob! fingerprint (string-append output "/missing")))

(test-assert "publish stores the bytes under the fingerprint"
  (publish-artifact-blob! fingerprint
    (lambda (port)
      (display text port))))

(test-assert "published blob exists"
  (artifact-blob-exists? fingerprint))

(test-equal "published blob holds the exact bytes"
  text
  (call-with-input-file (artifact-blob-path fingerprint)
    (lambda (port)
      (get-string-all port))))

(test-equal "restore materializes the bytes at the output path"
  #t
  (restore-artifact-blob! fingerprint
    (string-append output "/nested/page.html")))

(test-equal "restored bytes match the content"
  text
  (call-with-input-file (string-append output "/nested/page.html")
    (lambda (port)
      (get-string-all port))))

(test-equal "restoring over an existing output replaces the bytes"
  #t
  (restore-artifact-blob! fingerprint
    (string-append output "/nested/page.html")))

;;     The output may be a hard link to the store blob after a
;;     restore; writing new bytes through a truncating in-place
;;     write would corrupt the shared inode, so publishes write to a
;;     temporary file and rename.
(test-assert "republish over an existing output succeeds"
  (publish-artifact-blob! fingerprint
    (lambda (port)
      (display text port))))

(test-equal "store blob survives a republish"
  text
  (call-with-input-file (artifact-blob-path fingerprint)
    (lambda (port)
      (get-string-all port))))

(define other "other bytes")
(define other-fingerprint (content-fingerprint other))

(test-assert "second content gets its own blob"
  (publish-artifact-blob! other-fingerprint
    (lambda (port)
      (display other port))))

(test-assert "both blobs coexist"
  (and (artifact-blob-exists? fingerprint)
       (artifact-blob-exists? other-fingerprint)))

;;     A corrupt blob is a cache miss: restoring reports failure
;;     instead of producing broken bytes.
(call-with-output-file (artifact-blob-path fingerprint)
  (lambda (port)
    (display "corrupted garbage" port)))

(test-equal "corrupt blob restore reports failure"
  #f
  (restore-artifact-blob! fingerprint (string-append output "/corrupt")))

(test-end "grove artifact store")
(system* "rm" "-rf" root)
