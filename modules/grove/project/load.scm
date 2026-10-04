;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project load)
  #:use-module (grove project)
  #:use-module (grove project diagnostic)
  #:use-module (grove private input dependencies)
  #:use-module (ice-9 ftw)
  #:export (call-with-loaded-project))

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
                 (lambda () (canonicalize-path (absolute filename)))))
         (output (and output-directory (absolute output-directory))))
    (dynamic-wind
      (lambda () (chdir (dirname path)))
      (lambda ()
        (let* ((trace (make-hash-table))
               (project (call-with-project-stage 'load path
                          (lambda ()
                            (call-with-input-trace trace
                              (lambda () (read-project path))))))
               (configured
                 (project-with-options project
                   #:output-directory (or output (project-output-directory project))
                   #:extra-sources (cons path (trace-inputs trace)))))
          (procedure configured)))
      (lambda () (chdir caller-directory)))))
