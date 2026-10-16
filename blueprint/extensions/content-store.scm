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
;;;
;;; Stage-1 outputs live inside BLUE's store, whose path differs per
;;; checkout; the key keeps only the artifact below "stencils/" so a
;;; checkout-local store still shares them.

(define-module (blueprint extensions content-store)
  #:use-module (oop goops)
  #:use-module (blue types)
  #:use-module (blue stencils guile)
  #:use-module (blue build)
  #:use-module (blue store)
  #:use-module (blue utils hash)
  #:use-module (blue file-system directory)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
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

;;; The key must not see checkout-local store prefixes: stage-1
;;; outputs live inside BLUE's store, whose path differs between
;;; checkouts, while the artifact below "stencils/" is the same.
(define (key-output-name output)
  (let ((mark (and (string? output)
                   (string-contains output "/stencils/"))))
    (if mark (substring output (+ mark 1)) output)))

(define (content-key buildable)
  (hash (list (ask-build-key buildable)
              (map key-output-name (slot* buildable 'outputs)))))

(define (content-root)
  (or (getenv "BLUE_CONTENT_STORE_DIR")
      (string-append
        (or (getenv "XDG_CACHE_HOME")
            (string-append (or (getenv "HOME") "/tmp") "/.cache"))
        "/blue/content")))

(define (key-hex key)
  (bytes-fnv-hex (string->utf8 (number->string key))))

(define (key-index-path key)
  (string-append (content-root) "/keys/" (key-hex key)))

(define (blob-path content)
  (string-append (content-root) "/artifacts/" content))

(define (read-file-bytes path)
  (call-with-input-file path
    (lambda (port)
      (get-bytevector-all port))
    #:binary #t))

(define (write-file-bytes! path bytes)
  ;;     The staging file reserves a unique mkstemp! name (concurrent
  ;;     writers of one path never share a temp path) and lands with
  ;;     the umask-derived mode a plain write would have produced.
  (let ((temp (string-append path ".tmp-XXXXXX")))
    (call-with-port (mkstemp! temp)
      (lambda (port)
        (put-bytevector port bytes)))
    (chmod temp (logand #o666 (lognot (umask))))
    (rename-file temp path)))

(define (restore-output! key output)
  "Restore the bytes recorded for KEY into OUTPUT.  The blob the key
index names is re-hashed before use: a corrupt, truncated, or missing
entry is a cache miss, never a source of wrong bytes.  Return #t when
the output now holds verified bytes."
  (let ((index (key-index-path key)))
    (and (file-exists? index)
         (catch #t
           (lambda ()
             (let* ((content
                      (string-trim
                       (call-with-input-file index
                         (lambda (port)
                           (set-port-encoding! port "UTF-8")
                           (get-string-all port)))))
                    (blob (blob-path content)))
               (and (file-exists? blob)
                    (let ((bytes (read-file-bytes blob)))
                      (and (string=? (bytes-fnv-hex bytes) content)
                           (begin
                             (mkdir-p (dirname output))
                             (write-file-bytes! output bytes)
                             #t))))))
           (lambda args #f)))))

(define (publish-output! key output)
  "Record the bytes of OUTPUT under KEY: a content-addressed blob
plus a key index entry pointing at it.  Best effort: failures leave
the store without the entry and the build simply remains local."
  (false-if-exception
   (let* ((bytes (read-file-bytes output))
          (content (bytes-fnv-hex bytes))
          (blob (blob-path content))
          (index (key-index-path key)))
     (mkdir-p (dirname blob))
     (unless (file-exists? blob)
       (write-file-bytes! blob bytes))
     (mkdir-p (dirname index))
     (unless (file-exists? index)
       (let ((temp (string-append index ".tmp-XXXXXX")))
         (call-with-port (mkstemp! temp)
           (lambda (port)
             (set-port-encoding! port "UTF-8")
             (display content port)))
         (chmod temp (logand #o666 (lognot (umask))))
         (rename-file temp index)))
     #t)))

;;; The key override: content digests and compiler identity instead
;;; of checkout-local absolute paths.
(define-blue-method (ask-build-key (this <guile-module>))
  (vector
   (map key-output-name (slot* this 'outputs))
   (slot* this 'toolchain)
   (slot* this 'warnings)
   (slot* this 'warning-level)
   (slot* this 'optimizations)
   (slot* this 'optimization-level)
   (map file-digest (slot* this 'inputs))
   (effective-version)))

(define-blue-method (shareable-output? (this <guile-module>))
  #t)

(define-blue-method (ask-build-request
                     context
                     (this <guile-module>))
  (let* ((key (content-key this))
         (output (car (slot* this 'outputs)))
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
                       (car (slot* buildable
                                      'outputs))))))
