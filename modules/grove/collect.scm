;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove collect)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (collect-directory))

(define* (collect-directory directory #:optional (include? (lambda (_) #t)))
  "Recursively collect files below DIRECTORY that satisfy INCLUDE?.
INCLUDE? receives each file's basename; returned paths retain their full
directory structure. Hidden directories are skipped."
  (define (collect path)
    (append-map
      (lambda (filename)
        (let ((child (string-append path "/" filename)))
          (cond
            ((and (not (string-prefix? "." filename))
                  (file-is-directory? child))
             (collect child))
            ((and (not (file-is-directory? child)) (include? filename))
             (list child))
            (else '()))))
      (scandir path
               (lambda (filename)
                 (not (member filename '("." ".."))))
               string<?)))
  (unless (procedure? include?)
    (error "directory collector predicate must be a procedure" include?))
  (collect directory))
