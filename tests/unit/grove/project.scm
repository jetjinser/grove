;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (grove build)
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
(define cache-output-directory (string-append project-root "/cache-output"))
(define cache-output-path
  (string-append cache-output-directory "/summary.txt"))

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
                     (make-site-page id id (make-content-document '()) path)))
                 #:analyze
                 (lambda (site)
                   (set! observed-site-title (site-title site))
                   (set! events (append events '(analyze)))
                   site)
                 #:renderers
                 (list
                   (lambda (site)
                     (set! events (append events '(render)))
                     (append
                       (list
                         (make-artifact
                           "text/summary.txt"
                           (number->string (length (site-pages site)))
                           (site-sources site)))
                       (map (lambda (page)
                              (make-artifact
                                (string-append "pages/"
                                               (site-page-id page) ".txt")
                                (site-page-id page)
                                (list (site-page-source page))))
                            (site-pages site))
                       (list (make-artifact "opaque.txt" "opaque")))))
                 #:output-directory output-directory))
             (report (build-project/report project))
             (artifacts (build-report-artifacts report)))
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
          '("text/summary.txt" "pages/first.txt" "pages/second.txt"
            "opaque.txt")
          (map artifact-path artifacts))
        (test-assert "project writes renderer artifacts to its output directory"
          (file-exists? output-path))
        (test-equal "project output contains renderer content"
          "2"
          (call-with-input-file output-path get-string-all))
        (test-equal "first build reports every input as added"
          (list (string-append module-directory "/shared.grv")
                (string-append source-directory "/first.grv")
                (string-append source-directory "/second.grv"))
          (build-report-added-sources report))
        (test-equal "first build reports its output as affected"
          '("text/summary.txt" "pages/first.txt" "pages/second.txt"
            "opaque.txt")
          (build-report-affected-artifacts report))
        (let ((unchanged (build-project/report project)))
          (test-equal "unchanged build reports no input changes"
            '()
            (append (build-report-added-sources unchanged)
                    (build-report-modified-sources unchanged)
                    (build-report-removed-sources unchanged)))
          (test-equal "unknown renderer dependencies remain conservatively affected"
            '("opaque.txt")
            (build-report-affected-artifacts unchanged)))
        (write-test-file (string-append source-directory "/first.grv")
                         "first modified")
        (delete-file (string-append source-directory "/second.grv"))
        (write-test-file (string-append source-directory "/third.grv")
                         "third")
        (let ((changed (build-project/report project)))
          (test-equal "build report detects added inputs"
            (list (string-append source-directory "/third.grv"))
            (build-report-added-sources changed))
          (test-equal "build report detects modified inputs"
            (list (string-append source-directory "/first.grv"))
            (build-report-modified-sources changed))
          (test-equal "build report detects removed inputs"
            (list (string-append source-directory "/second.grv"))
            (build-report-removed-sources changed))
          (test-equal "input changes affect declared dependents and unknown outputs"
            '("text/summary.txt" "pages/first.txt" "pages/third.txt"
              "opaque.txt" "pages/second.txt")
            (build-report-affected-artifacts changed))
          (test-equal "project output refreshes after source changes"
            "2"
            (call-with-input-file output-path get-string-all)))
        (let* ((render-count 0)
               (analysis-count 0)
               (render
                 (lambda (site)
                   (set! render-count (+ render-count 1))
                   (list (make-artifact
                           "summary.txt"
                           (number->string (length (site-pages site)))
                           (site-sources site)))))
               (cached-renderer
                 (make-cached-renderer
                   render #:cache-key "summary-renderer-v1"))
               (cached-project
                 (make-project
                   #:source-directory source-directory
                   #:read-page
                   (lambda (path)
                     (let ((id (string-drop-right (basename path) 4)))
                       (make-site-page id id (make-content-document '()) path)))
                   #:analyze
                   (lambda (site)
                     (set! analysis-count (+ analysis-count 1))
                     site)
                   #:renderers (list cached-renderer)
                   #:output-directory cache-output-directory
                   #:cache-key "test-analysis-v1")))
          (let ((first (build-project/report cached-project)))
            (test-equal "cached renderer runs on the first build"
              1 render-count)
            (test-assert "first cacheable build reports rendering"
              (build-report-rendered? first)))
          (let ((second (build-project/report cached-project)))
            (test-equal "unchanged inputs reuse cached renderer output"
              1 render-count)
            (test-assert "cache hit reports that rendering was skipped"
              (not (build-report-rendered? second)))
            (test-equal "cache hit has no affected artifacts"
              '()
              (build-report-affected-artifacts second)))
          (test-equal "cache hits still run site analysis"
            2 analysis-count)
          (delete-file cache-output-path)
          (let ((missing-output (build-project/report cached-project)))
            (test-equal "missing cached outputs are rendered again"
              2 render-count)
            (test-assert "missing cached output is restored"
              (file-exists? cache-output-path))
            (test-assert "missing cached output reports rendering"
              (build-report-rendered? missing-output)))
          (write-test-file cache-output-path "tampered")
          (let ((tampered-output (build-project/report cached-project)))
            (test-equal "modified cached outputs are rendered again"
              3 render-count)
            (test-equal "modified cached outputs are restored"
              "2"
              (call-with-input-file cache-output-path get-string-all))
            (test-assert "modified cached output reports rendering"
              (build-report-rendered? tampered-output)))
          (write-test-file (string-append source-directory "/first.grv")
                           "cache source changed")
          (let ((changed-source (build-project/report cached-project)))
            (test-equal "changed source inputs invalidate renderer output"
              4 render-count)
            (test-equal "cached build report includes changed source paths"
              (list (string-append source-directory "/first.grv"))
              (build-report-modified-sources changed-source)))
          (let* ((changed-renderer
                   (make-cached-renderer
                     render #:cache-key "summary-renderer-v2"))
                 (changed-project
                   (make-project
                     #:source-directory source-directory
                     #:read-page
                     (lambda (path)
                       (let ((id (string-drop-right (basename path) 4)))
                         (make-site-page id id (make-content-document '()) path)))
                     #:analyze
                     (lambda (site)
                       (set! analysis-count (+ analysis-count 1))
                       site)
                     #:renderers (list changed-renderer)
                     #:output-directory cache-output-directory
                     #:cache-key "test-analysis-v1"))
                 (key-change (build-project/report changed-project)))
            (test-equal "renderer cache-key changes force rendering"
              5 render-count)
            (test-equal "renderer cache-key changes invalidate its output"
              '("summary.txt")
              (build-report-affected-artifacts key-change))
            (let* ((changed-project-key
                     (make-project
                       #:source-directory source-directory
                       #:read-page
                       (lambda (path)
                         (let ((id (string-drop-right (basename path) 4)))
                           (make-site-page id id (make-content-document '())
                                           path)))
                       #:analyze
                       (lambda (site)
                         (set! analysis-count (+ analysis-count 1))
                         site)
                       #:renderers (list changed-renderer)
                       #:output-directory cache-output-directory
                       #:cache-key "test-analysis-v2"))
                   (project-key-change
                     (build-project/report changed-project-key)))
              (test-equal "project cache-key changes force rendering"
                6 render-count)
              (test-equal "project cache-key changes invalidate its outputs"
                '("summary.txt")
                (build-report-affected-artifacts project-key-change))))))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list output-path
            cache-output-path
            (string-append cache-output-directory "/.grove-artifacts")
            (string-append cache-output-directory "/.grove-artifacts.tmp")
            (string-append cache-output-directory "/.grove-build-state")
            (string-append cache-output-directory "/.grove-build-state.tmp")
            (string-append output-directory "/pages/first.txt")
            (string-append output-directory "/pages/second.txt")
            (string-append output-directory "/pages/third.txt")
            (string-append output-directory "/opaque.txt")
            (string-append output-directory "/.grove-artifacts")
            (string-append output-directory "/.grove-artifacts.tmp")
            (string-append output-directory "/.grove-build-state")
            (string-append output-directory "/.grove-build-state.tmp")))
    (let ((text-directory (string-append output-directory "/text")))
      (when (file-exists? text-directory)
        (rmdir text-directory)))
    (let ((pages-directory (string-append output-directory "/pages")))
      (when (file-exists? pages-directory)
        (rmdir pages-directory)))
    (when (file-exists? output-directory)
      (rmdir output-directory))
    (when (file-exists? cache-output-directory)
      (rmdir cache-output-directory))
    (for-each (lambda (path)
                (when (file-exists? path)
                  (delete-file path)))
      (list (string-append module-directory "/shared.grv")
            (string-append source-directory "/first.grv")
            (string-append source-directory "/second.grv")
            (string-append source-directory "/third.grv")
            (string-append source-directory "/ignore.txt")))
    (rmdir module-directory)
    (rmdir source-directory)
    (rmdir project-root)))

(test-end "grove project")
