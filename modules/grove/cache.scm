;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove cache)
  #:use-module ((grove private fingerprint) #:prefix private:)
  #:use-module (grove private pipeline)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (make-content-cache
            content-cache?
            content-cache-ref
            content-cache-store!
            content-fingerprint
            file-fingerprint))

(define-record-type <content-cache>
  (%make-content-cache namespace)
  content-cache?
  (namespace content-cache-namespace))

(define content-fingerprint private:content-fingerprint)
(define file-fingerprint private:file-fingerprint)

(define (make-content-cache namespace)
  "Create a machine-wide cache namespace for deterministic byte values."
  (unless (and (string? namespace) (not (string-null? namespace)))
    (error "content cache requires a non-empty namespace" namespace))
  (%make-content-cache namespace))

(define (content-cache-identity cache key)
  (unless (content-cache? cache)
    (error "content cache operation requires a cache" cache))
  (unless (and (string? key) (not (string-null? key)))
    (error "content cache key must be a non-empty string" key))
  (private:content-fingerprint
    (string-append "grove-content-cache-v1\0"
                   (content-cache-namespace cache) "\0" key)))

(define (content-cache-ref cache key)
  "Return the verified bytevector for KEY, or #f when it is not cached."
  (let* ((identity (content-cache-identity cache key))
         (entries (read-boundary-index identity))
         (entry (and entries (assoc "value" entries))))
    (and entry (restore-boundary-bytes (cdr entry)))))

(define (content-cache-store! cache key bytes)
  "Store deterministic bytevector BYTES for KEY; return #f if the cache is unavailable."
  (unless (bytevector? bytes)
    (error "content cache stores bytevectors" bytes))
  (let* ((identity (content-cache-identity cache key))
         (fingerprint (publish-boundary-bytes bytes)))
    (write-boundary-index identity (list (cons "value" fingerprint)))))
