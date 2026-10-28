;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove project)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove build)
  #:use-module (grove output)
  #:use-module (grove project)
  #:use-module (grove private project model)
  #:use-module (grove project diagnostic)
  #:use-module (grove project site)
  #:use-module (grove input)
  #:use-module (grove pipeline)
  #:use-module (grove site)
  #:use-module (grove private build-state)
  #:use-module (grove private cache-dir)
  #:use-module (grove private preview rebuild))

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
(define drift-source-directory (string-append project-root "/drift-source"))
(define drift-output-directory (string-append project-root "/drift-output"))
(define drift-source-path (string-append drift-source-directory "/drift.plant"))
(define preview-output-directory (string-append project-root "/preview-output"))
(define preview-render-input (string-append project-root "/preview-render-input"))
(define preview-irrelevant-input (string-append project-root "/preview-irrelevant-input"))
(define preview-pipeline-selector (string-append project-root "/preview-pipeline-selector"))
(define preview-pipeline-a (string-append project-root "/preview-pipeline-a"))
(define preview-pipeline-b (string-append project-root "/preview-pipeline-b"))
(define external-source-path (string-append project-root "/theme.css"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(define (plant-source? filename)
  (and (not (string-prefix? "." filename))
       (string-suffix? ".plant" filename)))

(test-begin "grove project")

(test-assert "project execution details stay out of the public project API"
  (let ((interface (module-public-interface (resolve-module '(grove project)))))
    (every (lambda (name) (not (module-variable interface name)))
      '(project-source-directory project-renderers project-extra-sources
        project-output-directory project-cache? project-browse))))

(test-error "project constructors reject collectors with the wrong arity" #t
  (make-project #:collect-sources (lambda (_) '())
                #:read-source (lambda (_) #f)
                #:output-directory (string-append project-root "/bad-collector")))
(test-error "project constructors reject readers with the wrong arity" #t
  (make-project #:collect-sources (lambda () '())
                #:read-source (lambda () #f)
                #:output-directory (string-append project-root "/bad-reader")))
(test-error "project constructors reject analyzers with the wrong arity" #t
  (make-project #:collect-sources (lambda () '())
                #:read-source (lambda (_) #f)
                #:analyze (lambda () #f)
                #:output-directory (string-append project-root "/bad-analyzer")))
(test-error "project constructors reject renderers with the wrong arity" #t
  (make-project #:collect-sources (lambda () '())
                #:read-source (lambda (_) #f)
                #:renderers (list (lambda () '()))
                #:output-directory (string-append project-root "/bad-renderer")))
(test-assert "project callbacks may accept optional or rest arguments"
  (project?
    (make-project #:collect-sources (lambda () '())
                  #:read-source (lambda* (path #:optional (context #f)) #f)
                  #:analyze (lambda (model . options) model)
                  #:renderers (list (lambda* (model #:optional (context #f)) '()))
                  #:output-directory (string-append project-root "/flexible-callbacks"))))
(test-error "project constructors reject browsers with insufficient arity" #t
  (make-project #:collect-sources (lambda () '())
                #:read-source (lambda (_) #f)
                #:browse (lambda (_) #t)
                #:output-directory (string-append project-root "/bad-browser")))
(test-assert "project constructors accept browsers with three positional arguments"
  (project?
    (make-project #:collect-sources (lambda () '())
                  #:read-source (lambda (_) #f)
                  #:browse (lambda (model input output) #t)
                  #:output-directory (string-append project-root "/browser-positional"))))
(test-assert "project constructors accept optional browser port arguments"
  (project?
    (make-project #:collect-sources (lambda () '())
                  #:read-source (lambda (_) #f)
                  #:browse (lambda* (model #:optional (input #f) (output #f)) #t)
                  #:output-directory (string-append project-root "/browser-optional"))))
(test-assert "project constructors accept rest-argument browsers"
  (project?
    (make-project #:collect-sources (lambda () '())
                  #:read-source (lambda (_) #f)
                  #:browse (lambda arguments #t)
                  #:output-directory (string-append project-root "/browser-rest"))))
(test-error "site projects reject transforms with the wrong arity" #t
  (make-site-project #:collect-sources (lambda () '())
                     #:read-page (lambda (_) #f)
                     #:transforms (list (lambda () #f))
                     #:output-directory (string-append project-root "/bad-transform")))

(test-error "source collection rejects non-project values" #t
  (collect-project-sources #f))
(test-error "source collectors reject duplicate paths" #t
  (collect-project-sources
    (make-project #:collect-sources (lambda () '("same" "same"))
                  #:read-source (lambda (_) #f)
                  #:output-directory
                  (string-append project-root "/duplicate-source-output"))))
(test-error "source collectors reject filesystem NUL characters" #t
  (collect-project-sources
    (make-project #:collect-sources
                  (lambda () (list (string-append "source" (string #\nul))))
                  #:read-source (lambda (_) #f)
                  #:output-directory
                  (string-append project-root "/nul-source-output"))))
(test-error "project preparation rejects duplicate paths" #t
  (check-project
    (make-project #:collect-sources (lambda () '())
                  #:prepare (lambda () '("same" "same"))
                  #:read-source (lambda (_) #f)
                  #:output-directory
                  (string-append project-root "/duplicate-prepare-output"))))
(test-error "analysis rejects non-project values" #t
  (analyze-project #f))
(test-error "browsing rejects non-project values before reading project fields" #t
  (browse-project #f))
(test-error "building rejects non-project values before acquiring a lock" #t
  (build-project/report #f))
(test-error "build-project rejects non-project values" #t
  (build-project #f))
(test-error "check-project rejects non-project values" #t
  (check-project #f))

(define progress-test-project
  (make-project #:collect-sources (lambda () '())
                #:read-source (lambda (_) #f)
                #:output-directory
                (string-append project-root "/progress-output")
                #:browse (lambda (_ input output) #t)))
(define (progress-type-error? thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . _) (eq? key 'misc-error))))
(test-assert "analysis validates progress before wrapping project errors"
  (progress-type-error? (lambda () (analyze-project progress-test-project
                                                     #:progress #f))))
(test-assert "browsing validates progress before collecting project inputs"
  (progress-type-error? (lambda () (browse-project progress-test-project
                                                     #:progress #f))))
(test-assert "build validates progress before acquiring its output lock"
  (progress-type-error? (lambda () (build-project/report progress-test-project
                                                          #:progress #f))))
(test-assert "check validates progress before collecting project inputs"
  (progress-type-error? (lambda () (check-project progress-test-project
                                                   #:progress #f))))
(test-assert "project progress callbacks must accept stage and value"
  (progress-type-error? (lambda () (analyze-project progress-test-project
                                                     #:progress (lambda () #t)))))

(test-error "site pages require a non-empty string ID" #t
  (make-site-page "" "Page" (make-content-document '())))
(test-error "site pages require a non-empty string title" #t
  (make-site-page "page" #f (make-content-document '())))
(test-error "site page source paths cannot be empty" #t
  (make-site-page "page" "Page" (make-content-document '()) #:source ""))
(test-error "site page source uses an explicit keyword" #t
  (make-site-page "page" "Page" (make-content-document '()) "page.plant"))
(test-error "site page metadata keys are unique" #t
  (make-site-page "page" "Page" (make-content-document '())
    #:metadata '((summary . "first") (summary . "second"))))

(let* ((id (string-copy "page"))
       (title (string-copy "Title"))
       (source (string-copy "page.plant"))
       (summary (string-copy "Summary"))
       (metadata (list (cons 'summary summary)))
       (page (make-site-page id title (make-content-document '()) #:source source
                             #:metadata metadata)))
  (string-set! id 0 #\x)
  (string-set! title 0 #\x)
  (string-set! source 0 #\x)
  (string-set! summary 0 #\x)
  (set-car! metadata '(summary . "changed"))
  (test-equal "site pages snapshot caller-owned fields and metadata"
    '("page" "Title" "page.plant" "Summary")
    (list (site-page-id page)
          (site-page-title page)
          (site-page-source page)
          (assq-ref (site-page-metadata page) 'summary)))
  (let ((exposed-id (site-page-id page))
        (exposed-metadata (site-page-metadata page)))
    (string-set! exposed-id 0 #\x)
    (set-cdr! (assq 'summary exposed-metadata) "changed again"))
  (test-equal "site page accessors return independent value snapshots"
    '("page" "Summary")
    (list (site-page-id page)
          (assq-ref (site-page-metadata page) 'summary))))

(test-error "site sources reject empty paths" #t
  (make-site '() #:sources '("")))
(test-error "site sources reject filesystem NUL characters" #t
  (make-site '() #:sources (list (string-append "source" (string #\nul)))))

(test-equal "site source dependencies keep their first occurrence"
  '("theme.scm" "locale.scm")
  (site-sources
    (make-site '() #:sources '("theme.scm" "locale.scm" "theme.scm"))))

(let* ((first (make-site-page "first" "First" (make-content-document '())))
       (second (make-site-page "second" "Second" (make-content-document '())))
       (site (make-site (list first second) #:home "second")))
  (test-equal "site preserves caller page order"
    '("first" "second")
    (map site-page-id (site-pages site)))
  (test-equal "site accepts a home page named by its ID"
    "second"
    (site-home site)))

(test-error "site rejects duplicate page IDs" #t
  (make-site
    (list (make-site-page "same" "First" (make-content-document '()))
          (make-site-page "same" "Second" (make-content-document '())))))
(test-error "site rejects a home ID without a matching page" #t
  (make-site
    (list (make-site-page "page" "Page" (make-content-document '())))
    #:home "missing"))

(let* ((page (make-site-page "home" "Home" (make-content-document '())))
       (pages (list page))
       (title (string-copy "Site"))
       (source (string-copy "home.plant"))
       (sources (list source))
       (description (string-copy "Description"))
       (home (string-copy "home"))
       (site (make-site pages #:title title #:sources sources
                              #:description description #:home home)))
  (set-car! pages #f)
  (string-set! title 0 #\x)
  (string-set! source 0 #\x)
  (string-set! description 0 #\x)
  (string-set! home 0 #\x)
  (set-car! sources "changed.plant")
  (test-equal "sites snapshot caller-owned page, path, and descriptive values"
    '(#t "Site" ("home.plant") "Description" "home")
    (list (site-page? (car (site-pages site)))
          (site-title site)
          (site-sources site)
          (site-description site)
          (site-home site)))
  (let ((exposed-pages (site-pages site))
        (exposed-sources (site-sources site))
        (exposed-title (site-title site)))
    (set-car! exposed-pages #f)
    (set-car! exposed-sources "changed-again.plant")
    (string-set! exposed-title 0 #\x))
  (test-equal "site accessors return independent collection snapshots"
    '(#t "Site" ("home.plant"))
    (list (site-page? (car (site-pages site)))
          (site-title site)
          (site-sources site))))

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
(test-error "site projects reject empty extra source paths at construction" #t
  (make-site-project #:source-directory "sources"
                     #:read-page (lambda (_) #f)
                     #:output-directory "output"
                     #:extra-sources '("")))
(test-error "site projects require a page reader at construction" #t
  (make-site-project #:source-directory "sources"
                     #:output-directory "output"))

(let ((project (make-site-project
                 #:source-directory "sources"
                 #:read-page (lambda (_) #f)
                 #:output-directory "output")))
  (test-equal "generic site projects require an explicit renderer set"
    '() (project-renderers project)))

(define duplicate-pipelines
  (list (make-asset-pipeline
          #:name "math-assets" #:inputs '("math.typ")
          #:procedure (lambda (_) (list (make-artifact "math/a.svg" "a"))))
        (make-asset-pipeline
          #:name "math-assets" #:inputs '("math.typ")
          #:procedure (lambda (_) (list (make-artifact "math/b.svg" "b"))))))

(test-error "site projects reject duplicate asset pipeline names" #t
  (make-site-project #:source-directory "sources"
                     #:read-page (lambda (_) #f)
                     #:output-directory "output"
                     #:pipelines duplicate-pipelines))

(test-error "generic projects reject empty source paths" #t
  (make-project #:source-directory ""
                #:read-source (lambda (_) #f)
                #:output-directory "output"))
(test-error "generic projects reject empty extra source paths" #t
  (make-project #:source-directory "sources"
                #:extra-sources '("")
                #:read-source (lambda (_) #f)
                #:output-directory "output"))
(test-error "generic projects reject filesystem NUL characters in paths" #t
  (make-project #:source-directory "sources"
                #:extra-sources (list (string-append "theme" (string #\nul)))
                #:read-source (lambda (_) #f)
                #:output-directory "output"))
(test-error "generic projects reject duplicate extra source paths" #t
  (make-project #:source-directory "sources"
                #:extra-sources '("theme.css" "theme.css")
                #:read-source (lambda (_) #f)
                #:output-directory "output"))
(test-error "generic projects reject empty output paths" #t
  (make-project #:source-directory "sources"
                #:read-source (lambda (_) #f)
                #:output-directory ""))
(test-equal "explicit source collector takes precedence over source directory"
  '("selected.plant")
  (collect-project-sources
    (make-project #:source-directory "unused-directory"
                  #:source-filter #f
                  #:collect-sources (lambda () '("selected.plant"))
                  #:read-source (lambda (_) #f)
                  #:output-directory "output")))
(test-error "directory projects require a source filter procedure" #t
  (make-project #:source-directory "sources"
                #:source-filter #f
                #:read-source (lambda (_) #f)
                #:output-directory "output"))
(test-equal "project options reject non-project values before reading defaults"
  #t
  (catch 'misc-error
    (lambda () (project-with-options #f) #f)
    (lambda (_key _module template arguments _rest)
      (string-prefix? "project options require a Grove project"
                      (apply format #f template arguments)))))
(let* ((path (string-copy "selected.plant"))
       (collector-paths (list path))
       (project
         (make-project #:collect-sources (lambda () collector-paths)
                       #:read-source (lambda (_) #f)
                       #:output-directory "output"))
       (collected (collect-project-sources project)))
  (string-set! path 0 #\x)
  (test-equal "collected paths do not alias collector-owned strings"
    "selected.plant"
    (car collected))
  (string-set! (car collected) 0 #\y)
  (set-car! collected "other.plant")
  (test-equal "collected paths do not alias the collector-owned list"
    '("xelected.plant")
    collector-paths))
(test-equal "project options diagnose malformed additional sources early"
  #t
  (catch 'misc-error
    (lambda ()
      (project-with-options
        (make-project #:source-directory "sources"
                      #:read-source (lambda (_) #f)
                      #:output-directory "output")
        #:extra-sources '(not-a-path))
      #f)
    (lambda (_key _module template arguments _rest)
      (string-prefix? "project additional sources must be a list of non-empty paths"
                      (apply format #f template arguments)))))

(let* ((body (string-copy "before"))
       (_ (set-source-properties! body '((filename . "page.plant") (line . 4))))
       (source-value (list (cons 'body body)))
       (prepared-source (list (string-copy "prepared.scm")))
       (extra-source (list (string-copy "theme.css")))
       (captured-context #f)
       (project
        (make-project
          #:collect-sources (lambda () '("page.plant"))
          #:prepare (lambda () prepared-source)
          #:extra-sources extra-source
          #:read-source (lambda (_) source-value)
          #:analyze
          (lambda (context)
            (set! captured-context context)
            (set-cdr! (car source-value) "changed reader value")
            (set-source-properties! body '((filename . "changed.plant") (line . 9)))
            (set-car! prepared-source "changed-prepared.scm")
            (set-car! extra-source "changed-theme.css")
            (set-car! (project-context-inputs context) #f)
            (string-set! (project-input-path
                           (car (project-context-inputs context)))
                         0 #\x)
            (set-cdr! (car (project-input-value
                             (car (project-context-inputs context))))
                      "changed accessor value")
            (set-car! (project-context-prepared-sources context)
                      "changed-accessor-prepared.scm")
            (set-car! (project-context-extra-sources context)
                      "changed-accessor-theme.css")
            'model)
          #:output-directory "unused")))
  (test-equal "analyzer input contexts expose independent snapshots"
    'model (analyze-project project))
  (test-equal "analyzer mutations cannot alter project context values"
    '("page.plant" ((body . "before"))
      ("prepared.scm") ("theme.css"))
       (list (project-input-path
               (car (project-context-inputs captured-context)))
             (project-input-value
               (car (project-context-inputs captured-context)))
             (project-context-prepared-sources captured-context)
             (project-context-extra-sources captured-context)))
  (test-equal "source value snapshots preserve independent source properties"
    '((filename . "page.plant") (line . 4))
    (source-properties
      (cdr (car (project-input-value
                  (car (project-context-inputs captured-context))))))))

(let* ((source-directory (string-copy "sources"))
       (output-directory (string-copy "output"))
       (source-creator (lambda (prefix destination)
                         (string-append prefix ":" destination)))
       (extra-sources (list (string-copy "theme.css")))
       (renderers (list (lambda (_) '())))
       (project (make-project
                  #:source-directory source-directory
                  #:extra-sources extra-sources
                  #:read-source (lambda (_) #f)
                  #:renderers renderers
                  #:output-directory output-directory
                  #:create-source source-creator)))
  (string-set! source-directory 0 #\x)
  (string-set! output-directory 0 #\x)
  (set-car! extra-sources "changed.css")
  (set-car! renderers #f)
  (test-equal "projects snapshot caller-owned path and source values"
    '("sources" "output" ("theme.css") "note:destination")
    (list (project-source-directory project)
          (project-output-directory project)
          (project-extra-sources project)
          (create-project-source project
            #:prefix "note" #:destination "destination")))
  (test-equal "source creation defaults to the project source directory"
    "note:sources"
    (create-project-source project #:prefix "note"))
  (let ((exposed-sources (project-extra-sources project))
        (exposed-renderers (project-renderers project)))
    (set-car! exposed-sources "changed again.css")
    (set-car! exposed-renderers #f))
  (test-equal "project accessors return independent configuration containers"
    '("theme.css" 1)
    (list (car (project-extra-sources project))
          (length (project-renderers project)))))

(test-error "source creators must return a non-empty path" #t
  (create-project-source
    (make-project #:collect-sources (lambda () '())
                  #:read-source (lambda (_) #f)
                  #:output-directory "output"
                  #:create-source (lambda (_prefix _destination) 'not-a-path))))
(test-error "source creation rejects non-project values" #t
  (create-project-source #f))

(let* ((extra-sources (list (string-copy "theme.css")))
       (project (make-site-project
                  #:source-directory "sources"
                  #:extra-sources extra-sources
                  #:read-page (lambda (_) #f)
                  #:output-directory "output")))
  (set-car! extra-sources "changed.css")
  (test-equal "site projects snapshot inputs captured by project callbacks"
    '("theme.css")
    (project-extra-sources project)))

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
    (mkdir drift-source-directory)
    (write-test-file external-source-path "body { color: black; }")
    (write-test-file (string-append module-directory "/shared.plant") "module")
    (write-test-file (string-append source-directory "/first.plant") "first")
    (write-test-file (string-append source-directory "/second.plant") "second")
    (write-test-file (string-append source-directory "/.draft.plant") "hidden")
    (write-test-file (string-append source-directory "/ignore.txt") "ignored"))
  (lambda ()
    (let ((project
            (make-site-project
              #:source-directory source-directory
              #:read-page (lambda (_) #f)
              #:output-directory "unused-output")))
      (test-equal "generic site collection accepts non-hidden source formats"
        '("first.plant" "ignore.txt" "second.plant")
        (map basename (collect-project-sources project))))
    (let* ((path (string-append source-directory "/first.plant"))
           (project
             (make-site-project
               #:collect-sources (lambda () (list path))
               #:read-page (lambda (_) #f)
               #:output-directory "invalid-page-output"))
           (error
             (catch 'grove-project-error
               (lambda () (build-project project) #f)
               (lambda (_ error) error))))
      (test-equal "site readers fail at the read stage with their source path"
        (list 'read path)
        (and error
             (list (project-error-stage error)
                   (project-error-path error)))))
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
                           #:sources (list (string-append source-directory "/first.plant")))))
                 (lambda (model)
                   (set! generic-events (append generic-events '(render-data)))
                   (list (make-artifact
                           "inventory.scm"
                           (call-with-output-string
                             (lambda (port) (write model port) (newline port)))
                           #:sources
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
    (let* ((source drift-source-path)
           (output drift-output-directory)
           (renders 0)
           (project
             (begin
               (write-test-file source "before render")
               (make-project
                 #:collect-sources (lambda () (list source))
                 #:read-source
                 (lambda (path)
                   (call-with-input-file path get-string-all))
                 #:analyze
                 (lambda (context)
                   (project-input-value
                     (car (project-context-inputs context))))
                 #:renderers
                 (list
                   (lambda (model)
                     (set! renders (+ renders 1))
                     (when (= renders 1)
                       (write-test-file source "changed during render"))
                     (list (make-artifact "drift.txt" model #:sources (list source)))))
                 #:output-directory output)))
           (first (build-project/report project)))
      (test-equal "first render observes its original source snapshot" 1 renders)
      (test-assert "a source drift during render is not reported as a cache hit"
        (not (build-report-render-cache-hit? first)))
      (let ((second (build-project/report project)))
        (test-equal "source drift invalidates the just-written build state" 2 renders)
        (test-equal "the next render reads the changed source"
          "changed during render"
          (artifact-content (car (build-report-artifacts second))))))
    (let ((events '())
          (observed-site-title #f)
          (observed-site-description #f))
      (let* ((project
               (make-site-project
                   #:title "Configured"
                   #:description "A configured site description."
                   #:source-directory source-directory
                 #:source-filter plant-source?
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
                     (make-site-page id id (make-content-document '()) #:source path)))
                 #:analyze
                 (lambda (site)
                   (set! observed-site-title (site-title site))
                   (set! observed-site-description (site-description site))
                   (set! events (append events '(analyze)))
                   site)
                 #:renderers
                 (list
                   (make-cached-renderer
                     (lambda (site)
                       (set! events (append events '(render)))
                       (append
                         (list
                           (make-artifact
                             "text/summary.txt"
                             (number->string (length (site-pages site)))
                             #:sources (site-sources site)))
                         (map (lambda (page)
                                (make-artifact
                                  (string-append "pages/"
                                                 (site-page-id page) ".txt")
                                  (site-page-id page)
                                  #:sources (list (site-page-source page))))
                              (site-pages site))
                         (list (make-artifact "opaque.txt" "opaque"))))
                     #:configuration 'configured-site-renderer))
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
        (test-assert "configured collection excludes hidden .plant files"
          (not (member '(read ".draft") events)))
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
        (let ((reported-artifacts (build-report-artifacts report))
              (reported-sources (build-report-sources report)))
          (set-car! reported-artifacts #f)
          (string-set! (car reported-sources) 0 #\X)
          (test-assert "build reports isolate their artifact collection"
            (artifact? (car (build-report-artifacts report))))
          (test-equal "build reports isolate their source path strings"
            (string-append module-directory "/shared.plant")
            (car (build-report-sources report))))
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
               (observed-site-sources '())
               (render
                 (lambda (site)
                   (set! render-count (+ render-count 1))
                   (list (make-artifact
                           "summary.txt"
                           (string-append "平方：" (number->string (length (site-pages site))))
                           #:sources (append (site-sources site)
                                             (list external-source-path))))))
               (cached-renderer
                 (make-cached-renderer
                   render #:configuration '(summary (precision . 1))))
               (cached-project
               (make-site-project
                     #:source-directory source-directory
                     #:source-filter plant-source?
                     #:extra-sources (list external-source-path)
                   #:read-page
                   (lambda (path)
                     (set! reader-count (+ reader-count 1))
                     (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                       (make-site-page id id (make-content-document '()) #:source path)))
                   #:analyze
                   (lambda (site)
                     (set! analysis-count (+ analysis-count 1))
                     (set! observed-site-sources (site-sources site))
                     site)
                   #:renderers (list cached-renderer)
                   #:output-directory cache-output-directory)))
          (let ((first (build-project/report cached-project)))
            (test-equal "cached renderer runs on the first build"
              1 render-count)
            (test-equal "first build still reads every page"
              2 reader-count)
            (test-assert "first cacheable build is a renderer cache miss"
              (not (build-report-render-cache-hit? first)))
            (test-assert "extra source files appear in initial source report"
              (member external-source-path
                      (build-report-added-sources first)
                      string=?))
            (test-assert "site sources include collected page inputs"
              (every (lambda (path)
                       (member path observed-site-sources string=?))
                     (list (string-append source-directory "/first.plant")
                           (string-append source-directory "/third.plant"))))
            (test-assert "extra sources stay outside the site source list"
              (not (member external-source-path
                           observed-site-sources string=?))))
          (with-fluids ((%default-port-encoding "ISO-8859-1"))
            (let ((second (build-project/report cached-project)))
              (test-equal "UTF-8 cache reuse is independent of default port encoding"
                1 render-count)
              (test-equal "cache hit still reads every page"
                4 reader-count)
              (test-assert "cache hit is reported explicitly"
                (build-report-render-cache-hit? second))
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
            (test-assert "restored artifacts preserve the renderer cache hit"
              (build-report-render-cache-hit? missing-output)))
          (write-test-file cache-output-path "tampered")
          (let ((tampered-output (build-project/report cached-project)))
            (test-equal "modified cached outputs are rendered again"
              2 render-count)
            (test-equal "modified cached outputs are restored"
              "平方：2"
              (call-with-input-file cache-output-path get-string-all))
            (test-assert "modified cached output is a renderer cache miss"
              (not (build-report-render-cache-hit? tampered-output))))
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
                       #:source-filter plant-source?
                       #:extra-sources (list external-source-path)
                     #:read-page
                     (lambda (path)
                       (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                         (make-site-page id id (make-content-document '()) #:source path)))
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
                         #:source-filter plant-source?
                         #:extra-sources (list external-source-path)
                       #:read-page
                       (lambda (path)
                         (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                           (make-site-page id id (make-content-document '())
                                           #:source path)))
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
               (partial-details #f)
               (incremental-renderer
                 (make-cached-renderer
                   (lambda (site)
                     (set! full-count (+ full-count 1))
                     (append
                       (list (make-artifact
                               "index.txt"
                               (number->string (length (site-pages site)))
                               #:sources (site-sources site))
                             (make-artifact "independent.txt" "constant" #:sources '())
                             (make-artifact "unknown.txt" "opaque"))
                       (map (lambda (page)
                              (make-artifact
                                (string-append "pages/"
                                               (site-page-id page) ".txt")
                                (site-page-id page)
                                #:sources (list (site-page-source page))))
                            (site-pages site))))
                   #:render-changed
                   (lambda (site changed-paths dirty-paths old-artifacts)
                     (set! partial-count (+ partial-count 1))
                     (set! partial-details
                       (list changed-paths dirty-paths
                             (map artifact-path old-artifacts)))
                     (let ((current-ids
                             (map site-page-id (site-pages site)))
                           (independent
                             (find (lambda (artifact)
                                     (string=? "independent.txt"
                                               (artifact-path artifact)))
                                   old-artifacts)))
                       (append
                         (list (make-artifact
                                 "index.txt"
                                 (number->string (length current-ids))
                                 #:sources (site-sources site))
                               (or independent
                                   (make-artifact "independent.txt"
                                                  "constant" #:sources '()))
                               (make-artifact "unknown.txt" "opaque"))
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
                                     #:sources (list (site-page-source page))))))
                           (site-pages site)))))))
               (incremental-project
                 (make-site-project
                     #:source-directory source-directory
                     #:source-filter plant-source?
                     #:read-page
                   (lambda (path)
                     (let ((id (string-drop-right (basename path) (string-length ".plant"))))
                       (make-site-page id id (make-content-document '()) #:source path)))
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
            (test-assert "partial renderer receives source changes and reusable artifacts"
              (let ((changed-paths (car partial-details))
                    (dirty-paths (cadr partial-details))
                    (old-paths (caddr partial-details)))
                (and (member (string-append source-directory "/first.plant")
                             changed-paths string=?)
                     (member (string-append source-directory "/third.plant")
                             changed-paths string=?)
                     (member "index.txt" dirty-paths string=?)
                     (member "unknown.txt" dirty-paths string=?)
                     (not (member "independent.txt" dirty-paths string=?))
                     (member "pages/first.txt" old-paths string=?)
                     (member "pages/third.txt" old-paths string=?))))
            (test-assert "partial render is a renderer cache miss"
              (not (build-report-render-cache-hit? partial)))
            (test-equal "partial renderer preserves current output set"
              '("index.txt" "independent.txt" "unknown.txt"
                "pages/first.txt")
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
              '("index.txt" "independent.txt" "unknown.txt"
                "pages/first.txt" "pages/fourth.txt")
              (map artifact-path (build-report-artifacts added-page))))))))
    (let ((render-count 0)
          (pipeline-count 0)
          (state-path
            (string-append (grove-cache-directory preview-output-directory)
                           "/" build-state-name)))
      (write-test-file preview-render-input "render-1")
      (write-test-file preview-irrelevant-input "unrelated-1")
      (write-test-file preview-pipeline-selector "a")
      (write-test-file preview-pipeline-a "pipeline-a")
      (write-test-file preview-pipeline-b "pipeline-b")
      (let ((project
              (make-project
                #:collect-sources (lambda () '())
                #:extra-sources
                (list preview-render-input preview-irrelevant-input)
                #:read-source (lambda (_) #f)
                #:analyze (lambda (_) 'retained-model)
                #:pipelines
                (list
                  (make-asset-pipeline
                    #:name "preview-empty-dynamic-input"
                    #:procedure
                    (lambda (_input)
                      (set! pipeline-count (1+ pipeline-count))
                      (let* ((selector
                               (call-with-input-file
                                 (depend-on! preview-pipeline-selector)
                                 get-string-all))
                             (path (depend-on!
                                     (if (string=? selector "a")
                                         preview-pipeline-a
                                         preview-pipeline-b))))
                        (call-with-input-file path get-string-all))
                      '())))
                #:renderers
                (list
                  (make-cached-renderer
                    (lambda (_model)
                      (set! render-count (1+ render-count))
                      (list
                        (make-artifact "preview.txt"
                          (call-with-input-file preview-render-input
                            get-string-all)
                          #:sources (list preview-render-input))))
                    #:configuration 'stable))
                #:output-directory preview-output-directory)))
        (let ((report (build-project/report project)))
          (test-equal "empty pipeline runtime inputs execute on the first build"
            1 pipeline-count)
          (test-assert "build state records an empty pipeline's active dependency"
            (member (canonicalize-path preview-pipeline-a)
                    (build-report-sources report) string=?))
          (test-assert "build state omits the empty pipeline's inactive dependency"
            (not (member (canonicalize-path preview-pipeline-b)
                         (build-report-sources report) string=?))))
        (let ((state (read-build-state state-path)))
          (write-test-file preview-render-input "render-2")
          (let ((report
                  (rebuild-from-model project 'retained-model state
                                      (list preview-render-input) '())))
            (test-equal "model reuse reruns a renderer for its changed artifact input"
              2 render-count)
            (test-assert "model reuse reports a miss when a renderer runs"
              (not (build-report-render-cache-hit? report)))
            (test-equal "model reuse publishes the regenerated artifact"
              "render-2"
              (artifact-content (car (build-report-artifacts report)))))
          (let ((updated-state (read-build-state state-path)))
            (write-test-file preview-pipeline-selector "b")
            (let ((report
                    (rebuild-from-model project 'retained-model updated-state
                                        (list preview-pipeline-selector) '()
                                        #:analysis-inputs '())))
              (test-equal "model reuse reruns a pipeline whose runtime selector changed"
                2 pipeline-count)
              (test-assert "model reuse tracks the newly selected empty-pipeline input"
                (member (canonicalize-path preview-pipeline-b)
                        (build-report-sources report) string=?))
              (test-assert "model reuse removes the pipeline's stale runtime input"
                (not (member (canonicalize-path preview-pipeline-a)
                             (build-report-sources report) string=?))))
            (let ((updated-state (read-build-state state-path)))
            (write-test-file preview-irrelevant-input "unrelated-2")
            (let ((report
                    (rebuild-from-model project 'retained-model updated-state
                                        (list preview-irrelevant-input) '())))
              (test-equal "model reuse retains renderers independent of the changed input"
                2 render-count)
              (test-assert "model reuse reports a hit when all renderer outputs are reused"
                (build-report-render-cache-hit? report))))))))
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
               (lambda (model input output)
                 (set! view-events (append view-events (list 'browse model)))
                 (format output "browsed ~a\n" (cadr model)))
               #:output-directory view-output-directory)))
      (let ((model (analyze-project view-project)))
        (test-equal "analyze-project runs the analysis without renderers"
          '(model 0)
          model)
        (test-equal "analyze-project invokes no renderer" '() view-events)
        (test-assert "analyze-project writes no output directory"
          (not (file-exists? view-output-directory))))
      (test-error "browse-project requires an input port" #t
        (browse-project view-project #:input #f))
      (test-equal "invalid browser input is rejected before project analysis"
        '() view-events)
      (test-error "browse-project requires an output port" #t
        (browse-project view-project #:output #f))
      (test-equal "invalid browser output is rejected before project analysis"
        '() view-events)
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
            (string-append incremental-output-directory "/independent.txt")
            (string-append incremental-output-directory "/unknown.txt")
            (string-append incremental-output-directory "/pages/first.txt")
            (string-append incremental-output-directory "/pages/third.txt")
            (string-append incremental-output-directory "/pages/fourth.txt")
            (string-append output-directory "/pages/first.txt")
            (string-append output-directory "/pages/second.txt")
            (string-append output-directory "/pages/third.txt")
            (string-append output-directory "/opaque.txt")
            (string-append drift-output-directory "/drift.txt")
            (string-append preview-output-directory "/preview.txt")
            preview-render-input preview-irrelevant-input
            drift-source-path))
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
    (when (file-exists? drift-output-directory)
      (rmdir drift-output-directory))
    (when (file-exists? preview-output-directory)
      (rmdir preview-output-directory))
    (for-each (lambda (path)
                (when (file-exists? path)
                  (delete-file path)))
      (list (string-append module-directory "/shared.plant")
            preview-pipeline-selector
            preview-pipeline-a
            preview-pipeline-b
            (string-append source-directory "/first.plant")
            (string-append source-directory "/second.plant")
            (string-append source-directory "/.draft.plant")
            (string-append source-directory "/third.plant")
            (string-append source-directory "/fourth.plant")
            (string-append source-directory "/ignore.txt")))
    (when (file-exists? external-source-path)
      (delete-file external-source-path))
    (system* "rm" "-rf" (string-append project-root "/.cache"))
    (rmdir module-directory)
    (rmdir source-directory)
    (rmdir drift-source-directory)
    (rmdir project-root)))

(test-end "grove project")
