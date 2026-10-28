;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove collect)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove collect))

(define test-directory
  (format #f "/tmp/grove-collect-test-~a" (getpid)))
(define nested-directory (string-append test-directory "/nested"))
(define hidden-directory (string-append test-directory "/.hidden"))
(define cycle-directory (string-append nested-directory "/cycle"))
(define alias-file (string-append nested-directory "/alias.plant"))
(define fifo-file (string-append nested-directory "/blocked.plant"))

(define (write-test-file path)
  (call-with-output-file path (lambda (port) (display "test" port))))

(test-begin "grove collect")

(test-error "collection requires an existing directory" #t
  (collect-directory (string-append test-directory "/missing")))
(test-error "collection rejects an empty directory path" #t
  (collect-directory ""))
(test-error "collection rejects filesystem NUL characters in paths" #t
  (collect-directory (string-append "directory" (string #\nul))))

(dynamic-wind
  (lambda ()
    (mkdir test-directory)
    (mkdir nested-directory)
    (mkdir hidden-directory)
    (symlink nested-directory cycle-directory)
    (write-test-file (string-append test-directory "/root.plant"))
    (symlink (string-append test-directory "/root.plant") alias-file)
    (unless (zero? (system* "mkfifo" fifo-file))
      (error "could not create collection FIFO" fifo-file))
    (write-test-file (string-append nested-directory "/nested.plant"))
    (write-test-file (string-append hidden-directory "/hidden.plant"))
    (write-test-file (string-append nested-directory "/ignored.txt")))
  (lambda ()
    (test-error "collection filter must be a procedure" #t
      (collect-directory test-directory 'not-a-procedure))
    (test-error "collection filters must accept a basename" #t
      (collect-directory test-directory (lambda () #t)))
    (test-equal "collection descends, includes file aliases, and skips special files"
      (list alias-file
            (string-append nested-directory "/nested.plant")
            (string-append test-directory "/root.plant"))
      (collect-directory test-directory
        (lambda (filename) (string-suffix? ".plant" filename))))
    (test-equal "collection does not duplicate a trailing directory separator"
      (collect-directory test-directory)
      (collect-directory (string-append test-directory "/")))
    (test-assert "collection skips directory symlinks that could leave the tree or loop"
      (not (member (string-append nested-directory "/cycle/nested.plant")
                   (collect-directory test-directory)
                   string=?))))
  (lambda ()
    (when (file-exists? cycle-directory)
      (delete-file cycle-directory))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list alias-file
            (string-append test-directory "/root.plant")
            fifo-file
            (string-append nested-directory "/nested.plant")
            (string-append nested-directory "/ignored.txt")
            (string-append hidden-directory "/hidden.plant")))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (rmdir path)))
      (list nested-directory hidden-directory test-directory))))

(test-end "grove collect")
