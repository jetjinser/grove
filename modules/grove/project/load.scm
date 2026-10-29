;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project load)
  #:use-module (grove private files)
  #:use-module (grove private procedure)
  #:use-module (grove private project model)
  #:use-module (grove project diagnostic)
  #:use-module (grove private project timing)
  #:use-module (grove private fingerprint)
  #:use-module (grove private input cache)
  #:use-module (grove private input dependencies)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
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
    (scheme-datum-fingerprint entries)))

(define (read-project path trace)
  (save-module-excursion
    (lambda ()
      (let ((environment (make-fresh-user-module)))
        (set-current-module environment)
        (let* ((tmpdir (getenv "TMPDIR"))
               (directory
                (if (and tmpdir (> (string-length tmpdir) 0)
                         (absolute-file-name? tmpdir))
                    tmpdir
                    "/tmp"))
               (template (string-append directory "/grove-project-XXXXXX"))
               (port (mkstemp! (string-copy template)))
               (compiled-path (port-filename port)))
          (close-port port)
          (dynamic-wind
            (lambda () #t)
            (lambda ()
              ;; Compile before observing project imports, so compiler
              ;; implementation modules do not become project dependencies.
              (compile-file path #:output-file compiled-path
                            #:env environment)
              (call-with-input-trace trace
                (lambda ()
                  (let ((value (load-compiled compiled-path)))
                    (record-module-inputs! environment)
                    (unless (project? value)
                      (error "project file must return a Grove project" value))
                    value))))
            (lambda ()
              (false-if-exception (delete-file compiled-path)))))))))

(define* (call-with-loaded-project filename procedure
                                   #:key (output-directory #f))
  "Load FILENAME and call PROCEDURE with its project in the project directory.
OUTPUT-DIRECTORY, when supplied, is resolved relative to the caller's directory."
  (unless (filesystem-path-string? filename)
    (error "project filename must be a non-empty string" filename))
  (unless (procedure-accepts-arguments? procedure 1)
    (error "project loader callback must accept one project" procedure))
  (unless (or (not output-directory)
              (filesystem-path-string? output-directory))
    (error "project output directory must be a non-empty string or #f"
           output-directory))
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
                            (read-project path trace))))
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
