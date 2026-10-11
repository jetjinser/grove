;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private build-lock)
  #:use-module (system foreign)
  #:export (with-build-lock))

(define (ensure-directory directory)
  (unless (or (string=? directory ".")
              (string=? directory "/")
              (file-exists? directory))
    (let ((parent (dirname directory)))
      (unless (string=? parent directory)
        (ensure-directory parent)))
    (mkdir directory)))

;;     glibc flock through the foreign function interface: Guile ships
;;     no file-lock binding of its own, and the kernel releases an
;;     flock when the holder dies - a lock file's existence would need
;;     stale-entry reasoning that flock makes unnecessary.

(define flock
  (pointer->procedure int
                      (dynamic-func "flock" (dynamic-link))
                      (list int int)))

(define LOCK_EX 2)
(define LOCK_UN 8)
(define LOCK_NB 4)

(define (acquire-exclusive-lock port)
  (let loop ((announced? #f))
    (catch 'system-error
      (lambda ()
        (flock (fileno port) LOCK_EX))
      (lambda arguments
        (let ((errno (system-error-errno arguments)))
          (cond
            ((memv errno (list EAGAIN EWOULDBLOCK EINTR))
             ;;     Another Grove build holds the lock.  Announce the
             ;;     wait once, then poll: the holder is a live build
             ;;     and will release when it finishes or dies.
             (unless announced?
               (display "Waiting for the project build lock; another Grove build is running...\n"
                        (current-error-port))
               (force-output (current-error-port)))
             (sleep 1)
             (loop #t))
            (else (apply throw arguments))))))))

(define (canonical-directory directory)
  (ensure-directory directory)
  (canonicalize-path directory))

(define* (with-build-lock directory thunk)
  "Run THUNK holding an exclusive build lock on DIRECTORY - the build
output directory.  A build writes the whole artifact set, the
manifest, and the stale-output cleanup into that directory, so two
builds over one project (a serve watch and a manual build, say) must
not overlap: the loser's stale cleanup would delete the winner's fresh
files.  The lock key is the canonical directory, so every process
resolves to the same lock regardless of its working directory, and the
kernel drops the lock if a holder dies.  Waits, announcing once, when
another build holds the lock."
  (let* ((directory (canonical-directory directory))
         (path (string-append directory "/.grove-build-lock"))
         (port (open-file path "a")))
    (dynamic-wind
      (lambda () (acquire-exclusive-lock port))
      thunk
      (lambda ()
        (catch 'system-error
          (lambda () (flock (fileno port) LOCK_UN))
          (lambda _ #f))
        (close-port port)))))
