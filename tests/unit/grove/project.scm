;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove project)
  #:use-module (grove project site)
  #:use-module (grove pipeline)
  #:use-module (grove site))

(define project-root
  (format #f "/tmp/grove-project-test-~a" (getpid)))
(define source-directory (string-append project-root "/source"))
(define module-directory (string-append project-root "/modules"))
(define output-directory (string-append project-root "/output"))
(define generic-output-directory
  (string-append project-root "/generic-output"))
(define output-path (string-append output-directory "/text/summary.txt"))
(define cache-output-directory (string-append project-root "/cache-output"))
(define cache-output-path
  (string-append cache-output-directory "/summary.txt"))
(define incremental-output-directory
  (string-append project-root "/incremental-output"))
(define external-source-path (string-append project-root "/theme.css"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(test-begin "grove project")

(test-error "site pages require a non-empty string ID" #t
  (make-site-page "" "Page" (make-content-document '())))
(test-error "site pages require a non-empty string title" #t
  (make-site-page "page" #f (make-content-document '())))
(test-error "site page source paths cannot be empty" #t
  (make-site-page "page" "Page" (make-content-document '()) ""))
(test-error "site page metadata keys are unique" #t
  (make-site-page "page" "Page" (make-content-document '())
    #:metadata '((summary . "first") (summary . "second"))))

(test-error "site projects validate cache flags at construction" #t
  (make-site-project #:source-directory "sources"
                     #:read-page (lambda (_) #f)
                     #:output-directory "output"
                     #:cache? 'yes))
(test-error "site projects validate transforms at construction" #t
  (make-site-project #:source-directory "sources"
                     #:read-page (lambda (_) #f)
                     #:output-directory "output"
                     #:transforms '(not-a-procedure)))
(test-error "site projects require a page reader at construction" #t
  (make-site-project #:source-directory "sources"
                     #:output-directory "output"))

(define duplicate-pipelines
  (list (make-asset-pipeline
          #:name "math-assets" #:inputs '("math.typ")
          #:procedure (lambda (_input emit) (emit "math/a.svg" "a")))
        (make-asset-pipeline
          #:name "math-assets" #:inputs '("math.typ")
          #:procedure (lambda (_input emit) (emit "math/b.svg" "b")))))

(test-error "site projects reject duplicate asset pipeline names" #t
  (make-site-project #:source-directory "sources"
                     #:read-page (lambda (_) #f)
                     #:output-directory "output"
                     #:pipelines duplicate-pipelines))

(test-error "generic projects reject duplicate asset pipeline names" #t
  (make-project #:source-directory "sources"
                #:read-source (lambda (_) #f)
                #:output-directory "output"
                #:pipelines duplicate-pipelines))

(dynamic-wind
  (lambda ()
    (setenv "XDG_CACHE_HOME" (string-append project-root "/.cache"))
    (mkdir project-root)
    (mkdir source-directory)
    (mkdir module-directory)
    (write-test-file external-source-path "body { color: black; }")
    (write-test-file (string-append module-directory "/shared.plant") "module")
    (write-test-file (string-append source-directory "/first.plant") "first")
    (write-test-file (string-append source-directory "/second.plant") "second")
    (write-test-file (string-append source-directory "/ignore.txt") "ignored"))
  (lambda ()
    (let* ((generic-events '())
           (generic-project
             (make-project
               #:collect-sources
               (lambda ()
                 (set! generic-events (append generic-events '(collect)))
                 (list (string-append source-directory "/first.plant")))
               #:prepare
               (lambda ()
                 (set! generic-events (append generic-events '(prepare)))
                 (list (string-append module-directory "/shared.plant")))
               #:extra-sources (list external-source-path)
               #:read-source
               (lambda (path)
                 (set! generic-events (append generic-events '(read)))
                 (call-with-input-file path get-string-all))
               #:analyze
               (lambda (context)
                 (set! generic-events (append generic-events '(analyze)))
                 (list 'inventory
                       (map project-input-value
                            (project-context-inputs context))))
               #:renderers
               (list
                 (lambda (model)
                   (set! generic-events (append generic-events '(render-csv)))
                   (list (make-artifact
                           "summary.csv"
                           (format #f "items,~a\n"
                                   (length (cadr model)))
                           (list (string-append source-directory "/first.plant")))))
                 (lambda (model)
                   (set! generic-events (append generic-events '(render-data)))
                   (list (make-artifact
                           "inventory.scm"
                           (call-with-output-string
                             (lambda (port) (write model port) (newline port)))
                           (list (string-append module-directory "/shared.plant")
                                 external-source-path)))))
               #:output-directory generic-output-directory))
           (generic-report (build-project/report generic-project)))
      (test-equal "generic projects run independent collection, preparation, reading, analysis and renderers"
        '(collect prepare read analyze render-csv render-data)
        generic-events)
      (test-equal "generic project passes an opaque model to unrelated outputs"
        "items,1\n"
        (call-with-input-file
          (string-append generic-output-directory "/summary.csv")
          get-string-all))
      (test-equal "generic project tracks prepared, collected and extra files"
        (list (string-append module-directory "/shared.plant")
              (string-append source-directory "/first.plant")
              external-source-path)
        (filter (lambda (path) (string-prefix? project-root path))
                (build-report-added-sources generic-report))))
    (let ((events '())
          (observed-site-title #f)
          (observed-site-description #f))
      (let* ((project
               (make-site-project
                   #:title "Configured"
                   #:description "A configured site description."
                   #:source-directory source-directory
                 #:prepare
                 (lambda ()
                   (let ((paths (list (string-append module-directory
                                                     "/shared.plant"))))
                   (set! events
                     (append events
                       (list (list 'prepare (map basename paths)))))
                   paths))
                 #:read-page
                 (lambda (path)
                   (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                     (set! events (append events (list (list 'read id))))
                     (make-site-page id id (make-content-document '()) path)))
                 #:analyze
                 (lambda (site)
                   (set! observed-site-title (site-title site))
                   (set! observed-site-description (site-description site))
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
          '((prepare ("shared.plant"))
            (read "first")
            (read "second")
            analyze
            render)
          events)
        (test-equal "project configuration selects the site title"
          "Configured"
          observed-site-title)
        (test-equal "project configuration passes the site description"
          "A configured site description."
          observed-site-description)
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
          (list (string-append module-directory "/shared.plant")
                (string-append source-directory "/first.plant")
                (string-append source-directory "/second.plant"))
          (filter (lambda (path) (string-prefix? project-root path))
                (build-report-added-sources report)))
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
          (test-equal "unchanged inputs allow reuse without per-artifact dependencies"
            '()
            (build-report-affected-artifacts unchanged)))
        (write-test-file (string-append source-directory "/first.plant")
                         "first modified")
        (delete-file (string-append source-directory "/second.plant"))
        (write-test-file (string-append source-directory "/third.plant")
                         "third")
        (let ((changed (build-project/report project)))
          (test-equal "build report detects added inputs"
            (list (string-append source-directory "/third.plant"))
            (build-report-added-sources changed))
          (test-equal "build report detects modified inputs"
            (list (string-append source-directory "/first.plant"))
            (build-report-modified-sources changed))
          (test-equal "build report detects removed inputs"
            (list (string-append source-directory "/second.plant"))
            (build-report-removed-sources changed))
          (test-equal "input changes affect declared dependents and unknown outputs"
            '("text/summary.txt" "pages/first.txt" "pages/third.txt"
              "opaque.txt" "pages/second.txt")
            (build-report-affected-artifacts changed))
          (test-equal "project output refreshes after source changes"
            "2"
            (call-with-input-file output-path get-string-all)))
        (let* ((render-count 0)
               (reader-count 0)
               (analysis-count 0)
               (render
                 (lambda (site)
                   (set! render-count (+ render-count 1))
                   (list (make-artifact
                           "summary.txt"
                           (string-append "平方：" (number->string (length (site-pages site))))
                           (append (site-sources site)
                                   (list external-source-path))))))
               (cached-renderer
                 (make-cached-renderer
                   render #:configuration '(summary (precision . 1))))
               (cached-project
               (make-site-project
                     #:source-directory source-directory
                     #:extra-sources (list external-source-path)
                   #:read-page
                   (lambda (path)
                     (set! reader-count (+ reader-count 1))
                     (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                       (make-site-page id id (make-content-document '()) path)))
                   #:analyze
                   (lambda (site)
                     (set! analysis-count (+ analysis-count 1))
                     site)
                   #:renderers (list cached-renderer)
                   #:output-directory cache-output-directory)))
          (let ((first (build-project/report cached-project)))
            (test-equal "cached renderer runs on the first build"
              1 render-count)
            (test-equal "first build still reads every page"
              2 reader-count)
            (test-assert "first cacheable build reports rendering"
              (build-report-rendered? first))
            (test-assert "extra source files appear in initial source report"
              (member external-source-path
                      (build-report-added-sources first)
                      string=?)))
          (with-fluids ((%default-port-encoding "ISO-8859-1"))
            (let ((second (build-project/report cached-project)))
              (test-equal "UTF-8 cache reuse is independent of default port encoding"
                1 render-count)
              (test-equal "cache hit still reads every page"
                4 reader-count)
              (test-assert "cache hit reports that rendering was skipped"
                (not (build-report-rendered? second)))
              (test-equal "cache hit has no affected artifacts"
                '()
                (build-report-affected-artifacts second))))
          (test-equal "cache hit still analyzes the complete site"
            2 analysis-count)
          (delete-file cache-output-path)
          (let ((missing-output (build-project/report cached-project)))
            (test-equal "missing cached outputs are restored without rendering"
              1 render-count)
            (test-assert "missing cached output is restored"
              (file-exists? cache-output-path))
            (test-equal "the restored output holds the rendered bytes"
              "平方：2"
              (call-with-input-file cache-output-path get-string-all))
            (test-assert "a restored cache hit reports that rendering was skipped"
              (not (build-report-rendered? missing-output))))
          (write-test-file cache-output-path "tampered")
          (let ((tampered-output (build-project/report cached-project)))
            (test-equal "modified cached outputs are rendered again"
              2 render-count)
            (test-equal "modified cached outputs are restored"
              "平方：2"
              (call-with-input-file cache-output-path get-string-all))
            (test-assert "modified cached output reports rendering"
              (build-report-rendered? tampered-output)))
          (write-test-file (string-append source-directory "/first.plant")
                           "cache source changed")
          (let ((changed-source (build-project/report cached-project)))
            (test-equal "changed source inputs invalidate renderer output"
              3 render-count)
            (test-equal "cached build report includes changed source paths"
              (list (string-append source-directory "/first.plant"))
              (build-report-modified-sources changed-source)))
          (write-test-file external-source-path "body { color: rebeccapurple; }")
          (let ((changed-external (build-project/report cached-project)))
            (test-equal "extra source modifications are reported"
              (list external-source-path)
              (build-report-modified-sources changed-external))
            (test-equal "extra source modifications invalidate dependent outputs"
              '("summary.txt")
              (build-report-affected-artifacts changed-external))
            (test-equal "extra source modifications rerun cached renderers"
              4 render-count))
          (let* ((changed-renderer
                   (make-cached-renderer
                     render #:configuration '(summary (precision . 2))))
                 (changed-project
                 (make-site-project
                       #:source-directory source-directory
                       #:extra-sources (list external-source-path)
                     #:read-page
                     (lambda (path)
                       (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                         (make-site-page id id (make-content-document '()) path)))
                     #:analyze
                     (lambda (site)
                       (set! analysis-count (+ analysis-count 1))
                       site)
                     #:renderers (list changed-renderer)
                     #:output-directory cache-output-directory))
                 (key-change (build-project/report changed-project)))
            (test-equal "renderer configuration changes force rendering"
              5 render-count)
            (test-equal "renderer configuration changes invalidate its output"
              '("summary.txt")
              (build-report-affected-artifacts key-change))
            (let* ((changed-project-key
                     (make-site-project
                         #:source-directory source-directory
                         #:extra-sources (list external-source-path)
                       #:read-page
                       (lambda (path)
                         (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                           (make-site-page id id (make-content-document '())
                                           path)))
                       #:analyze
                       (lambda (site)
                         (set! analysis-count (+ analysis-count 1))
                         site)
                       #:renderers (list changed-renderer)
                       #:output-directory cache-output-directory
                       #:cache? #f))
                   (project-key-change
                     (build-project/report changed-project-key)))
              (test-equal "disabling project caching force rendering"
                6 render-count)
              (test-equal "disabling project caching invalidate its outputs"
                '("summary.txt")
              (build-report-affected-artifacts project-key-change))))
        (let* ((full-count 0)
               (partial-count 0)
               (incremental-renderer
                 (make-cached-renderer
                   (lambda (site)
                     (set! full-count (+ full-count 1))
                     (append
                       (list (make-artifact
                               "index.txt"
                               (number->string (length (site-pages site)))
                               (site-sources site)))
                       (map (lambda (page)
                              (make-artifact
                                (string-append "pages/"
                                               (site-page-id page) ".txt")
                                (site-page-id page)
                                (list (site-page-source page))))
                            (site-pages site))))
                   #:render-changed
                   (lambda (site changed-paths dirty-paths old-artifacts)
                     (set! partial-count (+ partial-count 1))
                     (let ((current-ids
                             (map site-page-id (site-pages site))))
                       (append
                         (list (make-artifact
                                 "index.txt"
                                 (number->string (length current-ids))
                                 (site-sources site)))
                         (map
                           (lambda (page)
                             (let* ((path (string-append
                                            "pages/" (site-page-id page)
                                            ".txt"))
                                    (old (find
                                           (lambda (artifact)
                                             (string=? path
                                                       (artifact-path artifact)))
                                           old-artifacts)))
                               (if (and old
                                        (not (member
                                               (site-page-source page)
                                               changed-paths string=?)))
                                   old
                                   (make-artifact
                                     path (site-page-id page)
                                     (list (site-page-source page))))))
                           (site-pages site)))))))
               (incremental-project
                 (make-site-project
                     #:source-directory source-directory
                     #:read-page
                   (lambda (path)
                     (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                       (make-site-page id id (make-content-document '()) path)))
                   #:renderers (list incremental-renderer)
                   #:output-directory incremental-output-directory)))
          (build-project/report incremental-project)
          (test-equal "incremental renderer performs initial full render"
            1 full-count)
          (write-test-file (string-append source-directory "/first.plant")
                           "incremental first")
          (delete-file (string-append source-directory "/third.plant"))
          (let ((partial (build-project/report incremental-project)))
            (test-equal "source changes use opted-in partial rendering"
              1 partial-count)
            (test-equal "source changes do not rerun full renderer"
              1 full-count)
            (test-assert "partial render is reported as rendering"
              (build-report-rendered? partial))
            (test-equal "partial renderer preserves current output set"
              '("index.txt" "pages/first.txt")
              (map artifact-path (build-report-artifacts partial)))
            (test-assert "partial renderer removes output for deleted page"
              (not (file-exists?
                    (string-append incremental-output-directory
                                   "/pages/third.txt")))))
          (write-test-file (string-append source-directory "/fourth.plant")
                           "new page")
          (let ((added-page (build-project/report incremental-project)))
            (test-equal "new inputs also use partial rendering"
              2 partial-count)
            (test-equal "new page appears in the complete renderer output set"
              '("index.txt" "pages/first.txt" "pages/fourth.txt")
              (map artifact-path (build-report-artifacts added-page))))))))
    (let* ((view-events '())
           (view-output-directory (string-append project-root "/view-output"))
           (view-project
             (make-project
               #:collect-sources (lambda () '())
               #:read-source
               (lambda (path) (call-with-input-file path get-string-all))
               #:analyze
               (lambda (context)
                 (list 'model (length (project-context-inputs context))))
               #:renderers
               (list (lambda (model)
                       (set! view-events (append view-events '(render)))
                       '()))
               #:browse
               (lambda* (model #:key input output)
                 (set! view-events (append view-events (list 'browse model)))
                 (format output "browsed ~a\n" (cadr model)))
               #:output-directory view-output-directory)))
      (let ((analysis (analyze-project view-project)))
        (test-equal "analyze-project runs the analysis without renderers"
          '(model 0)
          (project-analysis-model analysis))
        (test-equal "analyze-project invokes no renderer" '() view-events)
        (test-assert "analyze-project writes no output directory"
          (not (file-exists? view-output-directory))))
      (let ((output (open-output-string)))
        (browse-project view-project
          #:input (open-input-string "")
          #:output output)
        (test-equal "browse-project runs the browser on the model"
          '(browse (model 0)) view-events)
        (test-equal "browse-project writes through the provided port"
          "browsed 0\n" (get-output-string output))
        (test-assert "browse-project writes no output directory"
          (not (file-exists? view-output-directory))))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list output-path
            (string-append generic-output-directory "/summary.csv")
            (string-append generic-output-directory "/inventory.scm")
            cache-output-path
            (string-append incremental-output-directory "/index.txt")
            (string-append incremental-output-directory "/pages/first.txt")
            (string-append incremental-output-directory "/pages/third.txt")
            (string-append incremental-output-directory "/pages/fourth.txt")
            (string-append output-directory "/pages/first.txt")
            (string-append output-directory "/pages/second.txt")
            (string-append output-directory "/pages/third.txt")
            (string-append output-directory "/opaque.txt")))
    (let ((text-directory (string-append output-directory "/text")))
      (when (file-exists? text-directory)
        (rmdir text-directory)))
    (let ((pages-directory (string-append output-directory "/pages")))
      (when (file-exists? pages-directory)
        (rmdir pages-directory)))
    (let ((pages-directory
            (string-append incremental-output-directory "/pages")))
      (when (file-exists? pages-directory)
        (rmdir pages-directory)))
    (when (file-exists? output-directory)
      (rmdir output-directory))
    (when (file-exists? generic-output-directory)
      (rmdir generic-output-directory))
    (when (file-exists? cache-output-directory)
      (rmdir cache-output-directory))
    (when (file-exists? incremental-output-directory)
      (rmdir incremental-output-directory))
    (for-each (lambda (path)
                (when (file-exists? path)
                  (delete-file path)))
      (list (string-append module-directory "/shared.plant")
            (string-append source-directory "/first.plant")
            (string-append source-directory "/second.plant")
            (string-append source-directory "/third.plant")
            (string-append source-directory "/fourth.plant")
            (string-append source-directory "/ignore.txt")))
    (when (file-exists? external-source-path)
      (delete-file external-source-path))
    (system* "rm" "-rf" (string-append project-root "/.cache"))
    (rmdir module-directory)
    (rmdir source-directory)
    (rmdir project-root)))

(test-end "grove project")
