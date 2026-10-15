;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private cache-dir)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (cache-base grove-cache-directory grove-store-directory))

(define (path-fingerprint text)
  (let ((digest (sha256 (string->utf8 text)))
        (digits "0123456789abcdef"))
    (list->string
     (append-map
      (lambda (index)
        (let ((byte (bytevector-u8-ref digest index)))
          (list (string-ref digits (quotient byte 16))
                (string-ref digits (modulo byte 16)))))
      (iota (bytevector-length digest))))))

(define (cache-base)
  (or (getenv "GROVE_CACHE_DIR")
      (string-append
        (or (getenv "XDG_CACHE_HOME")
            (string-append (or (getenv "HOME") "/tmp") "/.cache"))
        "/grove")))

(define (grove-cache-directory output-directory)
  "The per-project directory holding Grove's build metadata, keyed by
the output path so it never lands inside the deployable output. The
parent directory is canonicalized so relative output paths from
different projects do not collide; a not-yet-existing parent falls
back to the path as given."
  (let ((canonical
          (or (false-if-exception
                (string-append (canonicalize-path (dirname output-directory))
                               "/"
                               (basename output-directory)))
              output-directory)))
    (string-append (cache-base) "/"
                   (substring (path-fingerprint canonical) 0 16))))

(define (grove-store-directory)
  "The machine-global content store, shared by every checkout and
every output directory of every project this user builds.  Keys are
content-derived, so concurrent writers land on identical objects and
the write discipline (temp file + rename) makes races no-ops.  Callers
append their own subtree name: input-cache for document analyses,
artifacts for rendered output blobs."
  (string-append (or (getenv "GROVE_STORE_DIR")
                     (string-append (cache-base) "/store"))))
