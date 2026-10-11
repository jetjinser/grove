;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove build-lock)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64)
  #:use-module (grove private build-lock))

(test-begin "grove build lock")

(define root (string-append "/tmp/grove-lock-test-"
                            (number->string (getpid))))
(define lock-path (string-append root "/.grove-build-lock"))

(define (poll-until predicate timeout-seconds)
  (let loop ((deadline (+ (current-time) timeout-seconds)))
    (or (predicate)
        (> (current-time) deadline)
        (begin (usleep 50000) (loop deadline)))))


(test-assert "the lock file lives inside the build directory"
  (begin
    (with-build-lock root (lambda () #t))
    (file-exists? lock-path)))

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
        ;;     While the first holder is still inside, the second must
        ;;     be waiting on the lock.
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
    (file-exists? (string-append target "/.grove-build-lock"))))

(system* "rm" "-rf" root)

(test-end "grove build lock")
