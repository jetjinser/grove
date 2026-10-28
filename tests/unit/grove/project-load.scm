;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project-load)
  #:use-module (grove project)
  #:use-module (grove private project model)
  #:use-module (grove project load)
  #:use-module (grove project diagnostic)
  #:use-module (grove output)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))

(define root (format #f "/tmp/grove-load-test-~a" (getpid)))
(define config (string-append root "/project.scm"))
(define load-config (string-append root "/load-input-project.scm"))
(define load-dependency (string-append root "/load-input.txt"))
(define load-output (string-append root "/load-output/result.txt"))
(define pipeline-config (string-append root "/pipeline-project.scm"))
(define pipeline-adapter (string-append root "/adapter.scm"))
(define pipeline-output (string-append root "/pipeline-output/generated.txt"))
(define caller (getcwd))
(define environment (current-module))
(define first-pipeline-report #f)

(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))

(test-error "loaded-project callbacks must accept the project" #t
  (call-with-loaded-project "missing-project.scm" (lambda () #t)))
(test-error "project stage actions must be zero-argument thunks" #t
  (call-with-project-stage 'read #f (lambda (_) #t)))

(define (write-project result)
  (write-file config
    (format #f "(use-modules (grove project) (grove build) (grove output) (ice-9 textual-ports))
(define private-project-binding 'private)
(make-project
  #:source-directory \"sources\"
  #:read-source (lambda (path) (call-with-input-file path get-string-all))
  #:renderers (list (make-cached-renderer
                     (lambda (_) (list (make-artifact \"result.txt\" ~s)))))
  #:output-directory \"output\")\n" result)))

(define (write-pipeline-project)
  (write-file pipeline-config
    "(use-modules (grove project) (grove build) (grove output) (grove pipeline) (ice-9 textual-ports))
(define pipeline-environment (current-module))
(define pipeline-output \"unset\")
(make-project
  #:source-directory \"pipeline-sources\"
  #:read-source (lambda (path) (call-with-input-file path get-string-all))
  #:pipelines
    (list (make-asset-pipeline
            #:name \"adapter\"
            #:inputs '(\"pipeline-sources/input.txt\")
            #:procedure (lambda (_input)
                         (save-module-excursion
                           (lambda ()
                             (set-current-module pipeline-environment)
                             (load \"adapter.scm\")))
                         (list (make-artifact \"generated.txt\"
                                 (module-ref pipeline-environment 'pipeline-output))))))
  #:output-directory \"pipeline-output\")\n"))

(define (write-load-input-project)
  (write-file load-config
    "(use-modules (grove input) (grove project) (grove build) (grove output) (ice-9 textual-ports))
(define configured-value
  (call-with-input-file (depend-on! \"load-input.txt\") get-string-all))
(make-project
  #:collect-sources (lambda () '())
  #:read-source (lambda (_) #f)
  #:analyze (lambda (_) configured-value)
  #:renderers (list (make-cached-renderer
                      (lambda (value)
                        (list (make-artifact \"result.txt\" value)))))
  #:output-directory \"load-output\")\n"))

(define (error-stage thunk)
  (catch 'grove-project-error thunk
    (lambda (_ error) (project-error-stage error))))

(define (fails-before-project-load? thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . _arguments)
      (not (eq? key 'grove-project-error)))))

(test-begin "project loading")
(test-assert "loader validates a filename before resolving it"
  (fails-before-project-load?
    (lambda () (call-with-loaded-project "" check-project))))
(test-assert "loader rejects filesystem NUL characters in filenames"
  (fails-before-project-load?
    (lambda ()
      (call-with-loaded-project
        (string-append "project" (string #\nul) ".scm")
        check-project))))
(test-assert "loader validates its callback before loading the file"
  (fails-before-project-load?
    (lambda ()
      (call-with-loaded-project "/tmp/missing-grove-project.scm" #f))))
(test-assert "loader validates output overrides before loading the file"
  (fails-before-project-load?
    (lambda ()
      (call-with-loaded-project "/tmp/missing-grove-project.scm"
        check-project #:output-directory ""))))
(test-assert "loader rejects filesystem NUL characters in output paths"
  (fails-before-project-load?
    (lambda ()
      (call-with-loaded-project "/tmp/missing-grove-project.scm"
        check-project
        #:output-directory (string-append "output" (string #\nul))))))
(dynamic-wind
  (lambda ()
    (setenv "XDG_CACHE_HOME" (string-append root "/.cache"))
    (mkdir root)
    (mkdir (string-append root "/sources"))
    (write-file (string-append root "/sources/input.txt") "input")
    (write-project "one")
    (write-file load-dependency "before")
    (write-load-input-project)
    (mkdir (string-append root "/pipeline-sources"))
    (write-file (string-append root "/pipeline-sources/input.txt") "pipeline input")
    (write-file pipeline-adapter
      "(set! pipeline-output \"before\")\n")
    (write-pipeline-project))
  (lambda ()
    (call-with-loaded-project config
      (lambda (project)
        (test-equal "project callback runs relative to project file" root (getcwd))
        (test-equal "relative paths work when invoked outside project" 1
          (length (build-report-artifacts (build-project/report project))))))
    (test-equal "loader restores caller directory" caller (getcwd))
    (test-assert "loader restores caller module" (eq? environment (current-module)))
    (test-assert "project definitions do not leak into caller"
      (not (module-defined? environment 'private-project-binding)))
    (let ((initial
            (call-with-loaded-project load-config
              (lambda (project) (build-project/report project)))))
      (test-assert "project load depend-on! inputs enter build provenance"
        (member (canonicalize-path load-dependency)
                (build-report-sources initial)))
      (write-file load-dependency "after")
      (call-with-loaded-project load-config
        (lambda (project)
          (let ((updated (build-project/report project)))
            (test-equal "project load dependencies report content changes"
              (list (canonicalize-path load-dependency))
              (build-report-modified-sources updated)))))
      (test-equal "project load dependencies refresh captured configuration"
        "after" (call-with-input-file load-output get-string-all)))
    (call-with-loaded-project config
      (lambda (project)
        (test-assert "unchanged loaded projects reuse render cache"
          (build-report-render-cache-hit? (build-project/report project)))))
    (write-project "two")
    (call-with-loaded-project config build-project)
    (test-equal "configuration changes invalidate cached rendered contents" "two"
      (call-with-input-file (string-append root "/output/result.txt") get-string-all))
    (call-with-loaded-project config
      (lambda (project)
        (test-equal "output override is relative to caller directory"
          (string-append caller "/preview") (project-output-directory project)))
      #:output-directory "preview")
    (call-with-loaded-project pipeline-config
      (lambda (project)
        (set! first-pipeline-report (build-project/report project))))
    (test-equal "asset pipeline returns its initial output" "before"
      (call-with-input-file pipeline-output get-string-all))
    (test-assert "runtime Scheme inputs appear in build source provenance"
      (member (canonicalize-path pipeline-adapter)
              (build-report-sources first-pipeline-report)))
    (test-assert "runtime Scheme inputs appear in artifact provenance"
      (member (canonicalize-path pipeline-adapter)
              (artifact-sources
                (car (build-report-artifacts first-pipeline-report)))))
    (write-file pipeline-adapter
      "(set! pipeline-output \"before\") ; adapter revision two\n")
    (call-with-loaded-project pipeline-config
      (lambda (project)
        (let ((same-output (build-project/report project)))
          (test-equal "runtime adapter edits are listed as modified sources"
            (list (canonicalize-path pipeline-adapter))
            (build-report-modified-sources same-output))
          (test-equal "runtime adapter edits affect their generated artifact"
            '("generated.txt")
            (build-report-affected-artifacts same-output)))))
    (test-equal "an unchanged pipeline byte survives an adapter edit" "before"
      (call-with-input-file pipeline-output get-string-all))
    (write-file pipeline-adapter "(set! pipeline-output \"after\")\n")
    (call-with-loaded-project pipeline-config
      (lambda (project)
        (let ((changed-output (build-project/report project)))
          (test-equal "changed pipeline inputs remain visible after a cache miss"
            (list (canonicalize-path pipeline-adapter))
            (build-report-modified-sources changed-output))
          (test-equal "changed pipeline bytes affect the generated artifact"
            '("generated.txt")
            (build-report-affected-artifacts changed-output)))))
    (test-equal "editing an imported pipeline adapter invalidates cached output" "after"
      (call-with-input-file pipeline-output get-string-all))
    (test-error "callback failures propagate" #t
      (call-with-loaded-project config (lambda (_) (error "callback failure"))))
    (test-equal "callback failure restores working directory" caller (getcwd))
    (write-file config "(list 'not-a-project)\n")
    (test-equal "invalid return has loading context" 'load
      (error-stage (lambda () (call-with-loaded-project config check-project))))
    (write-file config "private-project-binding\n")
    (test-equal "project loads have independent top-level environments" 'load
      (error-stage (lambda () (call-with-loaded-project config check-project))))
    (write-file config "(broken syntax\n")
    (test-equal "syntax failures have loading context" 'load
      (error-stage (lambda () (call-with-loaded-project config check-project))))
    (test-equal "missing files have loading context" 'load
      (error-stage
        (lambda () (call-with-loaded-project (string-append root "/missing.scm") check-project))))
    (test-equal "failed loading restores working directory" caller (getcwd))
    (let ((cause (string-copy "original cause")))
      (catch 'grove-project-error
        (lambda ()
          (call-with-project-stage 'read "input.plant"
            (lambda () (throw 'unbound-variable cause))))
        (lambda (_ error)
          (string-set! cause 0 #\X)
          (test-equal "diagnostics preserve original exception key" 'unbound-variable
            (project-error-key error))
          (test-equal "diagnostics snapshot original exception arguments" '("original cause")
            (project-error-arguments error))
          (test-equal "diagnostics preserve the source path" "input.plant"
            (project-error-path error))
          (let ((arguments (project-error-arguments error))
                (path (project-error-path error)))
            (string-set! (car arguments) 0 #\X)
            (string-set! path 0 #\X))
          (test-equal "diagnostic accessors return independent arguments" '("original cause")
            (project-error-arguments error))
          (test-equal "diagnostic accessors return independent paths" "input.plant"
            (project-error-path error))))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "project loading")
