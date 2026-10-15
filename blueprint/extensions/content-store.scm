;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Content-addressed sharing of compiled Guile modules across
;;; checkouts, implemented purely as BLUE extensions: no BLUE source
;;; changes, only methods on BLUE's globally-shared generics, its
;;; exported build-pass hook, and its documented slot-ref escape hatch
;;; (goops-slot-ref lives in (blue types); BLUE's gate keeper only
;;; guards the (oop goops) binding).
;;;
;;; How it works.  A build key for <guile-module> covers the content
;;; digest of every input, the blueprint-relative outputs, the
;;; toolchain, the warning and optimization settings, and the
;;; effective compiler version - deliberately not the canonicalized
;;; checkout-local absolute paths.  Before BLUE's base build logic
;;; runs, the module's output is restored from the content store when
;;; a blob under the current key exists, and the key is recorded so
;;; BLUE's own bookkeeping sees the definition as known; the base
;;; method - reached through next-method - then skips the compile on
;;; its own freshness checks (the restored output is newer than the
;;; unchanged inputs).  After every successful module build the output
;;; bytes are published into the store, so another checkout with the
;;; same definition - or this checkout after deleting its build
;;; directory - restores them instead of compiling.

(define-module (blueprint extensions content-store)
  #:use-module (oop goops)
  #:use-module (blue types)
  #:use-module (blue stencils guile)
  #:use-module (blue build)
  #:use-module (blue store)
  #:use-module (blue utils hash)
  #:use-module (blue file-system directory)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (file-digest))

;;     BLUE deliberately seals its instances against (oop goops)
;;     slot-ref, keeping its gate keeper only in this module's own
;;     binding - reachable from here through the module object.
(define slot* (module-ref (resolve-module '(blue types))
                          'goops-slot-ref))

;;; FNV-1a over the bytes, as a hex string.  A cache key only:
;;; collisions move work between files, they never change semantics.
(define (bytes-fnv-hex bytes)
  (let loop ((hash 14695981039346656037) (i 0) (n (bytevector-length bytes)))
    (if (= i n)
        (number->string hash 16)
        (loop (logand (* (logxor hash (bytevector-u8-ref bytes i))
                         1099511628211)
                      #xffffffffffffffff)
              (+ i 1)
              n))))

(define (file-digest path)
  (false-if-exception
   (call-with-input-file path
     (lambda (port)
       (bytes-fnv-hex (get-bytevector-all port)))
     #:binary #t)))

(define (shareable? buildable)
  (is-a? buildable <guile-module>))

(define (content-key buildable)
  (hash (list (ask-build-key buildable)
              (slot* buildable '%buildable-outputs))))

(define (content-root)
  (or (getenv "BLUE_CONTENT_STORE_DIR")
      (string-append
        (or (getenv "XDG_CACHE_HOME")
            (string-append (or (getenv "HOME") "/tmp") "/.cache"))
        "/blue/content")))

(define (blob-path key)
  (string-append (content-root) "/artifacts/"
                 (bytes-fnv-hex (string->utf8 (number->string key)))))

(define (restore-output! key output)
  "Restore the bytes stored under KEY into OUTPUT.  The blob is
re-hashed before use: a corrupt or truncated entry is a cache miss,
never a source of wrong bytes.  Return #t when the output now holds
verified bytes."
  (let ((path (blob-path key)))
    (and (file-exists? path)
         (not (file-is-directory? path))
         (catch #t
           (lambda ()
             (let* ((bytes
                      (call-with-input-file path
                        (lambda (port)
                          (get-bytevector-all port))
                        #:binary #t))
                    (digest (bytes-fnv-hex bytes)))
               (and (string=? digest
                              (bytes-fnv-hex (string->utf8
                                               (number->string key))))
                    (begin
                      (mkdir-p (dirname output))
                      (let ((temp (string-append output
                                                 ".restore-tmp-"
                                                 (number->string (getpid)))))
                        (call-with-output-file temp
                          (lambda (out)
                            (put-bytevector out bytes))
                          #:binary #t)
                        (rename-file temp output))
                      #t)))))
         (lambda args #f))))

(define (publish-output! key output)
  "Store the bytes of OUTPUT under KEY.  Best effort: failures leave
the store without the blob, and the build simply remains local."
  (false-if-exception
   (let* ((path (blob-path key))
          (root (dirname path)))
     (unless (file-exists? root)
       (mkdir-p root))
     (unless (file-exists? path)
       (let ((temp (string-append path ".tmp-" (number->string (getpid)))))
         (call-with-output-file temp
           (lambda (port)
             (call-with-input-file output
               (lambda (input)
                 (let loop ((chunk (get-bytevector-some input)))
                   (unless (eof-object? chunk)
                     (put-bytevector port chunk)
                     (loop (get-bytevector-some input))))))
             #:binary #t)
         (rename-file temp path))
       #t))))

;;; The key override: content digests and compiler identity instead
;;; of checkout-local absolute paths.
(define-blue-method (ask-build-key (this <guile-module>))
  (vector
   (slot* this '%buildable-outputs)
   (slot* this 'toolchain)
   (slot* this 'warnings)
   (slot* this 'warning-level)
   (slot* this 'optimizations)
   (slot* this 'optimization-level)
   (map file-digest (slot* this '%buildable-inputs))
   (effective-version)))

(define-blue-method (shareable-output? (this <guile-module>))
  #t)

(define-blue-method (ask-build-request
                     context
                     (this <guile-module>))
  (let* ((key (content-key this))
         (output (first (slot* this '%buildable-outputs)))
         (restored (restore-output! key output)))
    (when restored
      ;;     Record the key so BLUE's base method - reached through
      ;;     next-method below - sees the definition as known and
      ;;     skips the compile on its own freshness checks.
      (store-set! (hash output) (ask-build-key this)
                  #:namespace "buildable"))
    (if restored
        #f
        (next-method))))

;;     Publish the compiled bytes after every successful module
;;     build, so later builds - here or in another checkout - can
;;     restore them.
(add-hook! build-pass-hook
  (lambda (buildable)
    (when (is-a? buildable <guile-module>)
      (publish-output! (content-key buildable)
                       (first (slot* buildable
                                        '%buildable-outputs))))))
)
