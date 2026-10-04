;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private preview worker)
  #:use-module (grove project)
  #:use-module (grove project load)
  #:use-module (grove private cli progress)
  #:use-module (grove project diagnostic)
  #:use-module (grove build state)
  #:use-module (grove output)
  #:use-module (grove private preview watch)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (worker-main))

(define (existing-files paths)
  (filter (lambda (path)
            (let ((info (stat path #f)))
              (and info (eq? (stat:type info) 'regular))))
          (delete-duplicates paths string=?)))

(define (watch-roots filename project sources modules)
  (let* ((extra-files (map absolute-path (project-extra-sources project)))
         (roots
           (delete-duplicates
             (map (lambda (path)
                    (let ((absolute (absolute-path path)))
                      (if (file-exists? absolute) (canonicalize-path absolute) absolute)))
               (append (list (dirname filename))
                       (if (project-source-directory project)
                           (list (project-source-directory project)) '())
                       (map dirname
                         (filter (lambda (path)
                                   (not (or (member path modules string=?)
                                            (member path extra-files string=?))))
                                 sources))))
             string=?)))
    ;; Do not rescan nested roots, or recursively scan the parent directory
    ;; of a single declared external file.
    (filter (lambda (root)
              (not (any (lambda (parent)
                          (and (not (string=? root parent))
                               (string-prefix? (string-append parent "/") root)))
                        roots)))
            roots)))

(define (build-preview request result-path)
  (match request
    ((filename output previous-modules previous-roots previous-files debug?)
     (let ((loaded '()) (loaded-stamps '()) (old-hook %load-hook))
       (dynamic-wind
         (lambda ()
           (set! %load-hook
             (lambda (path)
               (when old-hook (old-hook path))
               (when (and (string? path) (string-suffix? ".scm" path))
                 (let ((path (absolute-path path)))
                   (set! loaded (cons path loaded))
                   (set! loaded-stamps (cons (file-stamp path) loaded-stamps)))))))
         (lambda ()
           (call-with-loaded-project filename
             (lambda (project)
               (let* ((modules (existing-files (append previous-modules loaded)))
                      (key (project-cache-key project))
                      (configured
                        (project-with-options project
                          #:extra-sources modules
                          #:cache-key
                          (and key
                               (format #f "~s"
                                 (list key
                                   (map (lambda (path) (list path (file-fingerprint path)))
                                        (sort (list-copy modules) string<?)))))))
                      (output (absolute-path (project-output-directory configured)))
                      (files (map absolute-path
                                  (append previous-files
                                          (project-extra-sources configured))))
                      (roots (delete-duplicates
                               (append previous-roots
                                       (watch-roots filename project files modules))
                               string=?))
                      (before (watch-snapshot roots files output))
                      (report (call-with-cli-progress "Building" filename
                                (lambda (progress)
                                  (build-project/report configured #:progress progress))))
                      (modules (existing-files (append modules loaded)))
                      (files (delete-duplicates
                               (append (map absolute-path (build-report-sources report)) modules)
                               string=?))
                      (roots (watch-roots filename project files modules)))
                 ;; This is a private process protocol, not a second artifact API.
                 (call-with-output-file result-path
                   (lambda (port)
                     (write
                       (list 'preview output roots files modules
                         (delete-duplicates (append before loaded-stamps)
                           (lambda (left right) (string=? (car left) (car right))))
                         (map (lambda (artifact)
                                (list (artifact-path artifact) (artifact-content artifact)))
                              (build-report-artifacts report)))
                       port)
                     (newline port)))
                 (format #t "Built ~d artifact~:p (~d affected).~%Output: ~a~%"
                         (length (build-report-artifacts report))
                         (length (build-report-affected-artifacts report)) output)
                 (unless (build-report-rendered? report)
                   (display "Renderer cache hit.\n"))))
             #:output-directory output))
         (lambda () (set! %load-hook old-hook)))))
    (_ (error "invalid preview worker request" request))))

(define (worker-main arguments)
  (match arguments
    ((request-path result-path)
     (let ((request (call-with-input-file request-path read)))
       (catch 'grove-project-error
         (lambda ()
           (call-with-project-stage 'build #f
             (lambda () (build-preview request result-path)))
           0)
         (lambda (_ error)
           (print-project-error error (list-ref request 5))
           1))))
    (_ (error "invalid preview worker arguments" arguments))))
