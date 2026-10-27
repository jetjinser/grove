;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project-check)
  #:use-module (grove build)
  #:use-module (grove private build-state)
  #:use-module (grove output)
  #:use-module (grove private cache-dir)
  #:use-module (grove pipeline)
  #:use-module (grove project)
  #:use-module (grove project diagnostic)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))

(define root (format #f "/tmp/grove-check-test-~a" (getpid)))
(define output (string-append root "/output"))
(define source (string-append root "/source.txt"))
(define renders 0)
(define pipeline-runs 0)
(define broken? #f)
(define artifacts (list (make-artifact "page.txt" "success")))
(define project
  (make-project
    #:collect-sources (lambda () (list source))
    #:read-source (lambda (path) (call-with-input-file path get-string-all))
    #:renderers
    (list (make-cached-renderer
            (lambda (_)
              (set! renders (+ renders 1))
              (when broken? (error "renderer failure"))
              artifacts)))
    #:output-directory output))

(define (snapshot)
  (map (lambda (pair)
         (let* ((path (string-append (car pair) "/" (cdr pair)))
                (info (stat path)))
           (list path (file-fingerprint path)
                 (stat:ino info) (stat:mtime info))))
       (list (cons output "page.txt")
             (cons (grove-cache-directory output) ".grove-artifacts")
             (cons (grove-cache-directory output) ".grove-build-state"))))

(test-begin "project checks")
(dynamic-wind
  (lambda ()
    (setenv "XDG_CACHE_HOME" (string-append root "/.cache"))
    (mkdir root)
    (call-with-output-file source (lambda (port) (display "source" port))))
  (lambda ()
    (test-assert "check returns the complete validated artifact set"
      (equal? artifacts (build-report-artifacts (check-project project))))
    (test-assert "check does not create an output directory"
      (not (file-exists? output)))
    (build-project project)
    (build-project project)
    (test-equal "ordinary build still reuses renderer cache" 2 renders)
    (let ((before (snapshot)))
      (check-project project)
      (test-equal "check forces rendering despite a valid cache" 3 renders)
      (test-equal "successful check preserves outputs and state" before (snapshot))
      (set! broken? #t)
      (test-error "check detects a renderer failure hidden by build cache" #t
        (check-project project))
      (test-equal "failed check preserves outputs and state" before (snapshot))
      (set! broken? #f)
      (set! artifacts (list (make-artifact "a" "file")
                            (make-artifact "a/page.txt" "nested")))
      (test-error "check rejects file/directory conflicts" #t
        (check-project project))
      (call-with-output-file source
        (lambda (port) (display "changed source" port)))
      (test-error "build rejects conflicts before writing any output" #t
        (build-project project))
      (test-equal "invalid builds preserve existing outputs" before (snapshot))
      (test-assert "invalid builds create no partial output"
        (not (file-exists? (string-append output "/a")))))
    (test-error "artifact validation rejects invalid renderer values" #t
      (validate-artifacts (list "not an artifact")))
    (test-error "artifact validation rejects duplicate paths" #t
      (validate-artifacts (list (make-artifact "a" "1")
                                (make-artifact "a" "2"))))
    (test-equal "artifact validation accepts Grove-named output paths"
      '(".grove-build-state/data")
      (map artifact-path
        (validate-artifacts
          (list (make-artifact ".grove-build-state/data" "x")))))
    (call-with-output-file
      (string-append (grove-cache-directory output) "/.grove-build-state")
      (lambda (port) (display "invalid state" port)))
    (set! artifacts '())
    (test-assert "check does not read malformed output cache state"
      (null? (build-report-artifacts (check-project project))))
    (let* ((pipeline-name (format #f "check-pipeline-~a" (getpid)))
           (pipeline
            (make-asset-pipeline
              #:name pipeline-name
              #:procedure
              (lambda (_ emit)
                (set! pipeline-runs (+ pipeline-runs 1))
                (emit "derived.txt" "pipeline output"))))
           (pipeline-project
            (make-project
              #:collect-sources (lambda () '())
              #:read-source (lambda (_) #f)
              #:pipelines (list pipeline)
              #:output-directory (string-append root "/pipeline-output"))))
      (build-project pipeline-project)
      (build-project pipeline-project)
      (test-equal "ordinary project builds reuse pipeline output" 1 pipeline-runs)
      (check-project pipeline-project)
      (check-project pipeline-project)
      (test-equal "project checks bypass pipeline cache reads and writes"
        3 pipeline-runs)
      (build-project pipeline-project)
      (test-equal "checks leave the previous pipeline cache entry intact"
        3 pipeline-runs)
      (let ((uncached-project
              (project-with-options pipeline-project #:cache? #f)))
        (build-project uncached-project)
        (build-project uncached-project)
        (test-equal "disabling project caching also disables pipeline reuse"
          5 pipeline-runs))))
  (lambda ()
    (system* "rm" "-rf" root)))
(let* ((message (string-append (make-string 900 #\x) "END-OF-DIAGNOSTIC"))
       (error (catch 'grove-project-error
                (lambda () (call-with-project-stage 'read "broken.plant"
                             (lambda () (throw 'misc-error #f "~a" (list message) #f))))
                (lambda (key error) error))))
  (define (printed debug?)
    (call-with-output-string
      (lambda (port)
        (parameterize ((current-error-port port)) (print-project-error error debug?)))))
  (test-assert "ordinary diagnostics keep the source path and truncate long errors"
    (let ((text (printed #f)))
      (and (string-contains text "broken.plant")
           (string-contains text "--debug")
           (not (string-contains text "END-OF-DIAGNOSTIC")))))
  (test-assert "debug diagnostics retain the complete original message"
    (string-contains (printed #t) "END-OF-DIAGNOSTIC")))
(test-end "project checks")
