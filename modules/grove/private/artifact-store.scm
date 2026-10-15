;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Content-addressed artifact blobs under the machine-global Grove
;;; store.  Every blob is keyed by the hex SHA-256 of its bytes, so
;;; concurrent writers never collide and every checkout of a project
;;; shares the same rendering output.  Every accessor treats an
;;; unusable or missing entry as an absent cache line: the store can
;;; only make builds faster, never produce different bytes.

(define-module (grove private artifact-store)
  #:use-module (grove private cache-dir)
  #:use-module (grove private fingerprint)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 binary-ports)
  #:export (artifact-blob-path
            artifact-blob-exists?
            publish-artifact-blob!
            restore-artifact-blob!))


(define (ensure-parent-directory directory)
  (unless (or (string=? directory ".") (file-exists? directory))
    (let ((parent (dirname directory)))
      (unless (string=? parent directory)
        (ensure-parent-directory parent))
      (false-if-exception (mkdir directory)))))

(define (artifact-blob-path fingerprint)
  (string-append (grove-store-directory) "/artifacts/" fingerprint))

(define (artifact-blob-exists? fingerprint)
  (let ((path (artifact-blob-path fingerprint)))
    (and (file-exists? path)
         (not (file-is-directory? path)))))

;;     Write BYTES into the store under FINGERPRINT.  WRITE! receives
;;     a binary port.  Best effort: failures leave the store without
;;     the blob and callers simply keep their own copy of the bytes.
(define (publish-artifact-blob! fingerprint write!)
  (catch #t
    (lambda ()
      (let* ((root (string-append (grove-store-directory) "/artifacts"))
             (path (string-append root "/" fingerprint))
             (temp (string-append path ".tmp-" (number->string (getpid)))))
        (ensure-parent-directory root)
        (call-with-output-file temp
          (lambda (port)
            (write! port)
            (force-output port))
          #:binary #t)
        (rename-file temp path)
        #t))
    (lambda args #f)))

;;     Materialize the blob for FINGERPRINT at OUTPUT-PATH.  The
;;     bytes are re-hashed before use: a corrupted or truncated blob
;;     is a cache miss, never a source of wrong bytes.  Return #t
;;     when the output now holds the verified bytes, #f when the
;;     store has no usable blob - callers then produce the bytes
;;     themselves.
(define (restore-artifact-blob! fingerprint output-path)
  (and (artifact-blob-exists? fingerprint)
       (catch #t
         (lambda ()
           (let* ((blob (artifact-blob-path fingerprint))
                  (bytes
                    (call-with-input-file blob
                      (lambda (port)
                        (get-bytevector-all port))
                      #:binary #t)))
             (and (string=? (fingerprint-bytes bytes) fingerprint)
                  (begin
                    (ensure-parent-directory (dirname output-path))
                    (let ((temp (string-append output-path
                                               ".restore-tmp-"
                                               (number->string (getpid)))))
                      (call-with-output-file temp
                        (lambda (output)
                          (put-bytevector output bytes))
                        #:binary #t)
                      (rename-file temp output-path))
                    #t))))
         (lambda args #f))))
