;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove project)
  #:use-module (grove site))

(define project-root
  (format #f "/tmp/grove-project-test-~a" (getpid)))
(define source-directory (string-append project-root "/source"))
(define module-directory (string-append source-directory "/modules"))
(define output-directory (string-append project-root "/output"))
(define output-path (string-append output-directory "/text/summary.txt"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(test-begin "grove project")

(dynamic-wind
  (lambda ()
    (mkdir project-root)
    (mkdir source-directory)
    (mkdir module-directory)
    (write-test-file (string-append module-directory "/shared.grv") "module")
    (write-test-file (string-append source-directory "/first.grv") "first")
    (write-test-file (string-append source-directory "/second.grv") "second")
    (write-test-file (string-append source-directory "/ignore.txt") "ignored"))
  (lambda ()
    (let ((events '())
          (observed-site-title #f))
      (let* ((project
               (make-project
                 #:title "Configured"
                 #:source-directory source-directory
                 #:module-directory module-directory
                 #:load-modules
                 (lambda (paths)
                   (set! events
                     (append events
                       (list (list 'load (map basename paths))))))
                 #:read-page
                 (lambda (path)
                   (let ((id (string-drop-right (basename path) 4)))
                     (set! events (append events (list (list 'read id))))
                     (make-site-page id id (make-content-document '()))))
                 #:analyze
                 (lambda (site)
                   (set! observed-site-title (site-title site))
                   (set! events (append events '(analyze)))
                   site)
                 #:renderers
                 (list
                   (lambda (site)
                     (set! events (append events '(render)))
                     (list
                       (make-artifact
                         "text/summary.txt"
                         (number->string (length (site-pages site)))))))
                 #:output-directory output-directory))
             (artifacts (build-project project)))
        (test-equal "project phases run modules, readers, analysis, renderers"
          '((load ("shared.grv"))
            (read "first")
            (read "second")
            analyze
            render)
          events)
        (test-equal "project configuration selects the site title"
          "Configured"
          observed-site-title)
        (test-equal "project returns artifacts from all configured renderers"
          '("text/summary.txt")
          (map artifact-path artifacts))
        (test-assert "project writes renderer artifacts to its output directory"
          (file-exists? output-path))
        (test-equal "project output contains renderer content"
          "2"
          (call-with-input-file output-path get-string-all)))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list output-path
            (string-append output-directory "/.grove-artifacts")
            (string-append output-directory "/.grove-artifacts.tmp")))
    (let ((text-directory (string-append output-directory "/text")))
      (when (file-exists? text-directory)
        (rmdir text-directory)))
    (when (file-exists? output-directory)
      (rmdir output-directory))
    (for-each delete-file
      (list (string-append module-directory "/shared.grv")
            (string-append source-directory "/first.grv")
            (string-append source-directory "/second.grv")
            (string-append source-directory "/ignore.txt")))
    (rmdir module-directory)
    (rmdir source-directory)
    (rmdir project-root)))

(test-end "grove project")
