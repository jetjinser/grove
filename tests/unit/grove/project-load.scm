;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project-load)
  #:use-module (grove project)
  #:use-module (grove project load)
  #:use-module (grove project diagnostic)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))

(define root (format #f "/tmp/grove-load-test-~a" (getpid)))
(define config (string-append root "/project.scm"))
(define caller (getcwd))
(define environment (current-module))

(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))

(define (write-project result)
  (write-file config
    (format #f "(use-modules (grove project) (grove build) (grove output) (ice-9 textual-ports))
(define private-project-binding 'private)
(make-project
  #:source-directory \"sources\"
  #:read-source (lambda (path) (call-with-input-file path get-string-all))
  #:renderers (list (make-cached-renderer
                     (lambda (_) (list (make-artifact \"result.txt\" ~s)))
                     #:cache-key \"renderer\"))
  #:cache-key \"project\"
  #:output-directory \"output\")\n" result)))

(define (error-stage thunk)
  (catch 'grove-project-error thunk
    (lambda (_ error) (project-error-stage error))))

(test-begin "project loading")
(dynamic-wind
  (lambda ()
    (mkdir root)
    (mkdir (string-append root "/sources"))
    (write-file (string-append root "/sources/input.txt") "input")
    (write-project "one"))
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
    (call-with-loaded-project config
      (lambda (project)
        (test-assert "unchanged loaded projects reuse render cache"
          (not (build-report-rendered? (build-project/report project))))))
    (write-project "two")
    (call-with-loaded-project config build-project)
    (test-equal "configuration changes invalidate cached rendered contents" "two"
      (call-with-input-file (string-append root "/output/result.txt") get-string-all))
    (call-with-loaded-project config
      (lambda (project)
        (test-equal "output override is relative to caller directory"
          (string-append caller "/preview") (project-output-directory project)))
      #:output-directory "preview")
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
    (catch 'grove-project-error
      (lambda ()
        (call-with-project-stage 'read "input.plant"
          (lambda () (throw 'unbound-variable "original cause"))))
      (lambda (_ error)
        (test-equal "diagnostics preserve original exception key" 'unbound-variable
          (project-error-key error))
        (test-equal "diagnostics preserve original arguments" '("original cause")
          (project-error-arguments error))
        (test-equal "diagnostics preserve the source path" "input.plant"
          (project-error-path error)))))
  (lambda () (system* "rm" "-rf" root)))
(test-end "project loading")
