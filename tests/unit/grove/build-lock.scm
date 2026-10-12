;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove build-lock)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64)
  #:use-module (grove private build-lock)
  #:use-module (grove private cache-dir))

(test-begin "grove build lock")

;;     The lock file lives in the per-project cache directory keyed by
;;     the build directory, so the suite pins XDG_CACHE_HOME to a
;;     test-local base and cleans both sides up afterwards.
(define root (string-append "/tmp/grove-lock-test-"
                            (number->string (getpid))))
(define cache-base (string-append root "/cache"))
(define original-cache #f)

(define (lock-path-for directory)
  (string-append (grove-cache-directory directory) "/.grove-build-lock"))

(define (poll-until predicate timeout-seconds)
  (let loop ((deadline (+ (current-time) timeout-seconds)))
    (or (predicate)
        (> (current-time) deadline)
        (begin (usleep 50000) (loop deadline)))))

(dynamic-wind
  (lambda ()
    (set! original-cache (getenv "XDG_CACHE_HOME"))
    (setenv "XDG_CACHE_HOME" cache-base))
  (lambda ()
    (define lock-path (lock-path-for root))

    (test-assert "the lock file lives in the per-project cache directory"
      (begin
        (with-build-lock root (lambda () #t))
        (file-exists? lock-path)))

    (test-assert "try-exclusive-lock fails while another port holds the lock"
      ;;     Regression: flock's -1 on would-block is a value, not an
      ;;     exception - treating it as success swept live workspaces.
      (let ((p1 (open-file lock-path "a"))
            (p2 (open-file lock-path "a")))
        (and (try-exclusive-lock p1)
             (not (try-exclusive-lock p2))
             (begin (release-lock p1) #t))))

    (test-assert "a second holder waits for the first to release"
      (let ((entered #f) (left #f) (second-entered #f))
        (let ((first (call-with-new-thread
                      (lambda ()
                        (with-build-lock root
                          (lambda ()
                            (set! entered #t)
                            (usleep 1500000)
                            (set! left #t)))))))
          (poll-until (lambda () entered) 5)
          (let ((second (call-with-new-thread
                         (lambda ()
                           (with-build-lock root
                             (lambda ()
                               (set! second-entered #t)))))))
            ;;     While the first holder is still inside, the second
            ;;     must be waiting on the lock.
            (usleep 700000)
            (test-assert "the waiting build has not entered the locked section"
              (and entered (not left) (not second-entered)))
            (test-assert "the waiting build enters after release"
              (and (poll-until (lambda () second-entered) 10)
                   second-entered))
            (join-thread second)
            (join-thread first)))))

    (test-assert "the lock key survives a symlinked path"
      (let* ((link (string-append root "/link"))
             (target (string-append root "/real")))
        (mkdir target)
        (symlink target link)
        (with-build-lock link (lambda () #t))
        ;;     The lock resolves the output path through symlinks, so
        ;;     locking through the link locks the target's lock.
        (file-exists? (lock-path-for target)))))
  (lambda ()
    (if original-cache
        (setenv "XDG_CACHE_HOME" original-cache)
        (unsetenv "XDG_CACHE_HOME"))
    (system* "rm" "-rf" root)))

(test-end "grove build lock")
