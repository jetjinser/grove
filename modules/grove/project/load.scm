;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project load)
  #:use-module (grove project)
  #:use-module (grove project diagnostic)
  #:use-module (grove build state)
  #:use-module (ice-9 ftw)
  #:export (call-with-loaded-project))

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
        (let* ((project
                 (call-with-project-stage 'load path
                   (lambda ()
                     (let ((value
                             (save-module-excursion
                               (lambda ()
                                 (set-current-module (make-fresh-user-module))
                                 (primitive-load path)))))
                       (unless (project? value)
                         (error "project file must return a Grove project" value))
                       value))))
               (key (project-cache-key project))
               (configured
                 (project-with-options project
                   #:output-directory (or output (project-output-directory project))
                   #:extra-sources (list path)
                   #:cache-key
                   (and key (format #f "~s" (list key path (file-fingerprint path)))))))
          (procedure configured)))
      (lambda () (chdir caller-directory)))))
