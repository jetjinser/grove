;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove collect)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (collect-directory))

(define* (collect-directory directory #:optional (include? (lambda (_) #t)))
"Recursively collect regular files below DIRECTORY that satisfy INCLUDE?.
DIRECTORY must name an existing directory.
INCLUDE? receives each regular file's basename; returned paths retain their
full directory structure. Symlinks to regular files are included. Hidden
directories, directory symlinks, and special files are skipped."
  (define (directory-entry? path)
    (let ((info (false-if-exception (lstat path))))
      (and info (eq? (stat:type info) 'directory))))
  (define (regular-file-entry? path)
    ;; STAT follows file symlinks, keeping source aliases useful while
    ;; excluding FIFOs, sockets, devices, and dangling links.
    (let ((info (false-if-exception (stat path))))
      (and info (eq? (stat:type info) 'regular))))
  (define (collect path)
    (append-map
      (lambda (filename)
        (let ((child (string-append path "/" filename)))
          (cond
            ((and (not (string-prefix? "." filename))
                  (directory-entry? child))
             (collect child))
            ((and (regular-file-entry? child) (include? filename))
             (list child))
            (else '()))))
      (scandir path
               (lambda (filename)
                 (not (member filename '("." ".."))))
               string<?)))
  (unless (procedure? include?)
    (error "directory collector predicate must be a procedure" include?))
  (unless (and (string? directory)
               (not (string-null? directory))
               (file-is-directory? directory))
    (error "directory collector requires an existing directory" directory))
  (collect directory))
