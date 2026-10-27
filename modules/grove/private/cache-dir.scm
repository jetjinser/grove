;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private cache-dir)
  #:use-module (grove private fingerprint)
  #:export (cache-base grove-cache-directory grove-store-directory))

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
                   (substring (content-fingerprint canonical) 0 16))))

(define (grove-store-directory)
  "The per-user content store, shared by every checkout and
every output directory of every project this user builds.  Keys are
content-derived, so concurrent writers land on identical objects and
the write discipline (temp file + rename) makes races no-ops.  Callers
append their own subtree name: input-cache for document analyses,
artifacts for build outputs, pipeline products, and extension cache blobs."
  (string-append (or (getenv "GROVE_STORE_DIR")
                     (string-append (cache-base) "/store"))))
