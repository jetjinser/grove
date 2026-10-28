;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Content-addressed content blobs under the per-user shared Grove
;;; store. Every blob is keyed by the hex SHA-256 of its bytes, so
;;; concurrent writers never collide and every checkout can share build
;;; outputs, pipeline products, and extension cache values. Unusable or
;;; missing entries are cache misses: the store only saves recomputation.

(define-module (grove private artifact-store)
  #:use-module (grove private cache-dir)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (artifact-blob-path
            artifact-blob-exists?
            publish-artifact-blob!
            read-artifact-index
            write-artifact-index!
            store-content-blob!
            read-content-blob
            restore-artifact-blob!))

(define (artifact-blob-path fingerprint)
  (string-append (grove-store-directory) "/artifacts/" fingerprint))

(define (artifact-blob-exists? fingerprint)
  (regular-file? (artifact-blob-path fingerprint) #:follow-links? #f))

;;     Write BYTES into the store under FINGERPRINT.  WRITE! receives
;;     the output port.  Best effort: failures leave the store without
;;     the blob and callers simply keep their own copy of the bytes.
(define (publish-artifact-blob! fingerprint write!)
  (catch #t
    (lambda ()
      (let ((root (string-append (grove-store-directory) "/artifacts")))
        (ensure-directory root)
        (call-with-staging-file!
         (string-append root "/" fingerprint) write!)
        #t))
    (lambda args #f)))

(define (read-artifact-index fingerprint valid?)
  "Read one validated Scheme datum from the store, or return #f on a miss.

VALID? checks the index's domain-specific shape. Trailing data and read errors
make the index unusable, just like a missing or corrupt content blob."
  (catch #t
    (lambda ()
      (and (artifact-blob-exists? fingerprint)
           (call-with-input-file (artifact-blob-path fingerprint)
             (lambda (port)
               (let ((index (read port)))
                 (and (valid? index)
                      (eof-object? (read port))
                      index))))))
    (lambda args #f)))

(define (write-artifact-index! fingerprint index)
  "Publish INDEX as one Scheme datum; return #f if the store is unavailable."
  (publish-artifact-blob! fingerprint
    (lambda (port)
      (write index port)
      (newline port))))

(define (store-content-blob! content)
  "Store text or bytes and return the fingerprint only when its blob is available."
  (unless (or (string? content) (bytevector? content))
    (error "content blob requires text or bytes" content))
  (let* ((bytes (if (string? content) (string->utf8 content) content))
         (fingerprint (fingerprint-bytes bytes)))
    (if (read-content-blob fingerprint)
        fingerprint
        (and (publish-artifact-blob! fingerprint
               (lambda (port) (put-bytevector port bytes)))
             fingerprint))))

(define (read-content-blob fingerprint)
  "Return verified bytes for FINGERPRINT, or #f when the blob is unavailable."
  (and (artifact-blob-exists? fingerprint)
       (catch #t
         (lambda ()
           (let* ((blob (artifact-blob-path fingerprint))
                  (bytes
                    (call-with-input-file blob
                      (lambda (port) (get-bytevector-all port))
                      #:binary #t)))
             (and (string=? (fingerprint-bytes bytes) fingerprint)
                  bytes)))
         (lambda args #f))))

;;     Materialize the blob for FINGERPRINT at OUTPUT-PATH.  The
;;     bytes are re-hashed before use: a corrupted or truncated blob
;;     is a cache miss, never a source of wrong bytes.  Return #t
;;     when the output now holds the verified bytes, #f when the
;;     store has no usable blob - callers then produce the bytes
;;     themselves.
(define (restore-artifact-blob! fingerprint output-path)
  (let ((bytes (read-content-blob fingerprint)))
    (and bytes
         (catch #t
           (lambda ()
             (ensure-directory (dirname output-path))
             (call-with-staging-file! output-path
               (lambda (staging) (put-bytevector staging bytes)))
             #t)
           (lambda args #f)))))
