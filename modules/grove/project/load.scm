;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project load)
  #:use-module (grove private project model)
  #:use-module (grove private project lifecycle)
  #:use-module (grove project diagnostic)
  #:use-module (grove private project timing)
  #:use-module (grove private fingerprint)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (call-with-loaded-project))

(define (project-load-key root paths)
  (let* ((prefix (string-append root "/"))
         (logical-path
          (lambda (path)
            (if (string-prefix? prefix path)
                (substring path (string-length prefix))
                path)))
         (entries
          (sort
            (map (lambda (path)
                   (list (logical-path path) (input-fingerprint path)))
                 (delete-duplicates paths string=?))
            (lambda (left right) (string<? (car left) (car right))))))
    (content-fingerprint
      (call-with-output-string (lambda (port) (write entries port))))))

(define (read-project path)
  (save-module-excursion
    (lambda ()
      (let ((environment (make-fresh-user-module)))
        (set-current-module environment)
        (let ((value (primitive-load path)))
          (record-module-inputs! environment)
          (unless (project? value)
            (error "project file must return a Grove project" value))
          value)))))

(define* (call-with-loaded-project filename procedure
                                   #:key (output-directory #f))
  "Load FILENAME and call PROCEDURE with its project in the project directory.
OUTPUT-DIRECTORY, when supplied, is resolved relative to the caller's directory."
  (let* ((caller-directory (getcwd))
         (absolute (lambda (path)
                     (if (absolute-file-name? path) path
                         (string-append caller-directory "/" path))))
         (path (call-with-project-stage 'load filename
                 (lambda ()
                   ;;     A missing file deserves a direction, not a
                   ;;     bare stat error from canonicalize-path.
                   (let ((candidate (absolute filename)))
                     (unless (file-exists? candidate)
                       (error (string-append
                                "no project file at " candidate
                                "; pass --project or run from the project root")))
                     (canonicalize-path candidate)))))
         (output (and output-directory (absolute output-directory))))
    (dynamic-wind
      (lambda () (chdir (dirname path)))
      (lambda ()
        (let* ((trace (make-hash-table))
               (started (get-internal-real-time))
               (project (call-with-project-stage 'load path
                          (lambda ()
                            (call-with-input-trace trace
                              (lambda () (read-project path))))))
               (_ (current-project-key
                    (project-load-key (canonicalize-path (getcwd))
                                      (trace-inputs trace))))
               (_ (let ((timings (current-analysis-timings)))
                    ;;     Profile mode: the project file's own
                    ;;     evaluation - style values, module loads,
                    ;;     inlined assets - is a floor component the
                    ;;     document-level numbers hide.
                    (when timings
                      (timings 'project-load
                               (/ (- (get-internal-real-time) started)
                                  internal-time-units-per-second)))))
               (configured
                 (project-with-options project
                   #:output-directory (or output (project-output-directory project))
                   #:extra-sources (cons path (trace-inputs trace)))))
          (procedure configured)))
      (lambda () (chdir caller-directory)))))
