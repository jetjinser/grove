;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove collect)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (grove collect))

(define test-directory
  (format #f "/tmp/grove-collect-test-~a" (getpid)))
(define nested-directory (string-append test-directory "/nested"))
(define hidden-directory (string-append test-directory "/.hidden"))

(define (write-test-file path)
  (call-with-output-file path (lambda (port) (display "test" port))))

(test-begin "grove collect")

(dynamic-wind
  (lambda ()
    (mkdir test-directory)
    (mkdir nested-directory)
    (mkdir hidden-directory)
    (write-test-file (string-append test-directory "/root.grv"))
    (write-test-file (string-append nested-directory "/nested.grv"))
    (write-test-file (string-append hidden-directory "/hidden.grv"))
    (write-test-file (string-append nested-directory "/ignored.txt")))
  (lambda ()
    (test-equal "collection descends into subdirectories and keeps full paths"
      (list (string-append nested-directory "/nested.grv")
            (string-append test-directory "/root.grv"))
      (collect-directory test-directory
        (lambda (filename) (string-suffix? ".grv" filename)))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list (string-append test-directory "/root.grv")
            (string-append nested-directory "/nested.grv")
            (string-append nested-directory "/ignored.txt")
            (string-append hidden-directory "/hidden.grv")))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (rmdir path)))
      (list nested-directory hidden-directory test-directory))))

(test-end "grove collect")
