;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove cache)
  #:use-module ((grove private fingerprint) #:prefix private:)
  #:use-module (grove private artifact-store)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
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
  "Create a per-user shared cache namespace addressed by caller keys."
  (unless (non-empty-string? namespace)
    (error "content cache requires a non-empty namespace" namespace))
  (%make-content-cache (snapshot-data namespace)))

(define (content-cache-identity cache key)
  (unless (content-cache? cache)
    (error "content cache operation requires a cache" cache))
  (unless (non-empty-string? key)
    (error "content cache key must be a non-empty string" key))
  (private:content-fingerprint
    (string-append "grove-content-cache-v2\0"
                   (number->string
                     (string-length (content-cache-namespace cache))) ":"
                   (content-cache-namespace cache)
                   (number->string (string-length key)) ":" key)))

(define (read-cache-index identity)
  (catch #t
    (lambda ()
      (let ((path (artifact-blob-path identity)))
        (and (artifact-blob-exists? identity)
             (call-with-input-file path
               (lambda (port)
                 (let ((index (read port)))
                   (and (list? index)
                        (= (length index) 1)
                        (pair? (car index))
                        (string=? (caar index) "value")
                        (private:fingerprint? (cdar index))
                        (eof-object? (read port))
                        (cdar index))))))))
    (lambda args #f)))

(define (write-cache-index identity fingerprint)
  (publish-artifact-blob! identity
    (lambda (port)
      (write (list (cons "value" fingerprint)) port))))

(define (content-cache-ref cache key)
  "Return the verified bytevector for KEY, or #f when it is not cached."
  (let* ((identity (content-cache-identity cache key))
         (fingerprint (read-cache-index identity)))
    (and fingerprint (read-content-blob fingerprint))))

(define (content-cache-store! cache key bytes)
  "Store bytevector BYTES for KEY; return #t only when its blob and index are available.
Later reads may still miss if an entry disappears or becomes invalid. Callers
must ensure that a reused KEY remains stable for the intended lifetime."
  (unless (bytevector? bytes)
    (error "content cache stores bytevectors" bytes))
  (let* ((identity (content-cache-identity cache key))
         (fingerprint (store-content-blob! bytes)))
    (and fingerprint (write-cache-index identity fingerprint))))
