;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove collect)
  #:use-module (ice-9 ftw)
  #:export (collect-directory))

(define* (collect-directory directory #:optional (include? (lambda (_) #t)))
  (unless (procedure? include?)
    (error "directory collector predicate must be a procedure" include?))
  (map (lambda (filename)
         (string-append directory "/" filename))
       (scandir
         directory
         (lambda (filename)
           (and (not (member filename '("." "..")))
                (not (file-is-directory? (string-append directory "/" filename)))
                (include? filename)))
         string<?)))
