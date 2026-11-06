;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove pipeline)
  #:declarative? #f
  #:use-module (grove cache)
  #:use-module (grove input)
  #:use-module (grove output)
  #:use-module (grove project)
  #:use-module (grove pipeline)
  #:use-module (grove private input cache)
  #:use-module (grove private pipeline)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;     Asset pipelines are the third content-addressed boundary: the
;;     procedure runs only when its identity - name, version, project
;;     fingerprint, input fingerprints - misses the shared store, and
;;     everything it returns rides the ordinary artifact machinery.

(define root (mkdtemp "/tmp/grove-pipeline-test-XXXXXX"))
(define store (string-append root "/store"))
(define inputs-directory (string-append root "/inputs"))
(define input-a (string-append inputs-directory "/a.txt"))
(define input-b (string-append inputs-directory "/b.txt"))
(define input-c (string-append inputs-directory "/c.txt"))
(define path-root (string-append inputs-directory "/path-sensitive"))
(define path-directory-a (string-append path-root "/a"))
(define path-directory-b (string-append path-root "/b"))
(define runtime-script (string-append inputs-directory "/runtime.scm"))
(define runs 0)
(define runtime-runs 0)
(define constant-runs 0)
(define runtime-marker #f)

(setenv "GROVE_STORE_DIR" store)
(setenv "GROVE_CACHE_DIR" (string-append root "/cache"))
(current-project-key "project-v1")
(mkdir inputs-directory)
(mkdir path-root)
(mkdir path-directory-a)
(mkdir path-directory-b)
(call-with-output-file input-a
  (lambda (port) (display "alpha" port)))
(call-with-output-file input-b
  (lambda (port) (display "beta" port)))
(call-with-output-file input-c
  (lambda (port) (display "alpha" port)))
(for-each
  (lambda (directory)
    (call-with-output-file (string-append directory "/input.txt")
      (lambda (port) (display "same bytes" port))))
  (list path-directory-a path-directory-b))
(call-with-output-file runtime-script
  (lambda (port) (display "(set! runtime-marker \"one\")\n" port)))

(define (make-figures-pipeline version)
  (make-asset-pipeline
   #:name "figures"
   #:version version
   #:inputs (list input-a input-b)
   #:procedure
   (lambda (in)
     (set! runs (+ runs 1))
     (list
       (make-artifact "fig/one.txt"
         (string-append (call-with-input-file (in input-a)
                          get-string-all)
                        "\n"))
       (make-byte-artifact "fig/two.bin"
         (string->utf8 (call-with-input-file (in input-b)
                          get-string-all)))))))

(define (run-pipelines version)
  (run-asset-pipelines (list (make-figures-pipeline version))
                       (current-project-key)))

(test-begin "grove asset pipelines")

(test-error "asset pipeline constructors reject callbacks with the wrong arity" #t
  (make-asset-pipeline #:name "bad-callback"
                       #:procedure (lambda () '())))

(let* ((declared-inputs (list (string-copy input-a)))
       (pipeline
        (make-asset-pipeline
          #:name "pipeline-snapshot"
          #:inputs declared-inputs
          #:procedure
          (lambda (in)
            (list (make-artifact "snapshot.txt"
                    (call-with-input-file (in input-a) get-string-all)))))))
  (set-car! declared-inputs input-b)
  (let ((artifacts
         (car (run-asset-pipelines (list pipeline) "snapshot-project"))))
    (test-equal "asset pipelines snapshot declared input paths"
      (canonicalize-path input-a)
      (car (artifact-sources (car artifacts))))))

(define (blob-files)
  (scandir (string-append store "/artifacts")
           (lambda (name)
             (not (member name '("." ".."))))))

(define (blob->index? name)
  (let ((sexp (false-if-exception
               (call-with-input-file
                   (string-append store "/artifacts/" name)
                 read))))
    (and (pair? sexp)
         (or (eq? (car sexp) 'grove-pipeline-index)
             (and (pair? (car sexp))
                  (string? (caar sexp)))))))

(define (index-blobs)
  (filter blob->index? (blob-files)))

(define (content-blobs)
  (lset-difference string=? (blob-files) (index-blobs)))

(define (overwrite-blob name text)
  (call-with-output-file (string-append store "/artifacts/" name)
    (lambda (port) (display text port))))

(test-assert "asset pipeline representation stays private"
  (not (module-variable
         (module-public-interface (resolve-module '(grove pipeline)))
         'asset-pipeline?)))

(test-error "asset pipelines reject duplicate input paths" #t
  (make-asset-pipeline
    #:name "duplicate-input"
    #:inputs (list input-a input-a)
    #:procedure (lambda (_input) '())))

(test-error "asset pipelines require a non-empty name" #t
  (make-asset-pipeline
    #:name ""
    #:procedure (lambda (_input) '())))

(test-error "asset pipelines require non-empty input paths" #t
  (make-asset-pipeline
    #:name "empty-input"
    #:inputs (list "")
    #:procedure (lambda (_input) '())))
(test-error "asset pipelines reject filesystem NUL characters in inputs" #t
  (make-asset-pipeline
    #:name "nul-input"
    #:inputs (list (string-append "input" (string #\nul)))
    #:procedure (lambda (_input) '())))

(let ((runs 0))
  (define (path-sensitive-pipeline input)
    (make-asset-pipeline
      #:name "path-sensitive-input"
      #:inputs (list input)
      #:procedure
      (lambda (in)
        (set! runs (1+ runs))
        (list (make-artifact "path-sensitive.txt" (basename (in input)))))))
  (define (run-path-sensitive input)
    (car (run-asset-pipelines (list (path-sensitive-pipeline input))
                              "path-sensitive-project")))
  (test-equal "pipeline output may use the declared input path"
    "a.txt" (artifact-content (car (run-path-sensitive input-a))))
  (test-equal "same bytes at a different path do not reuse pipeline output"
    "c.txt" (artifact-content (car (run-path-sensitive input-c))))
  (test-equal "different declared input paths have distinct cache identities"
    2 runs))

(let ((runs 0)
      (original-directory (getcwd)))
  (define (run-materialized-path directory)
    (dynamic-wind
      (lambda () (chdir directory))
      (lambda ()
        (car
          (run-asset-pipelines
            (list
              (make-asset-pipeline
                #:name "materialized-path-input"
                #:inputs '("input.txt")
                #:procedure
                (lambda (in)
                  (set! runs (1+ runs))
                  (list (make-artifact "materialized-path.txt" (in "input.txt"))))))
            "same-project-key")))
      (lambda () (chdir original-directory))))
  (let ((path-a (string-append (canonicalize-path path-directory-a)
                               "/input.txt"))
        (path-b (string-append (canonicalize-path path-directory-b)
                               "/input.txt")))
    (test-equal "same declared path and bytes produce the materialized path"
      path-a
      (artifact-content (car (run-materialized-path path-directory-a))))
    (test-equal "different materialized paths do not share cached output"
      path-b
      (artifact-content (car (run-materialized-path path-directory-b))))
    (test-equal "materialized input paths have distinct cache identities"
      2 runs)))

(let ((merged
       (merge-pipeline-artifacts
        (list (make-artifact "shared.css" "pipeline"))
        (list (make-artifact "shared.css" "renderer")
              (make-artifact "page.html" "page"))
        '())))
  (test-equal "pipeline artifacts take precedence over renderer paths"
    '("shared.css" "page.html")
    (map artifact-path merged))
  (test-equal "pipeline precedence keeps the returned content"
    "pipeline"
    (artifact-content (car merged))))

(test-error "pipeline artifacts cannot share an output path" #t
  (validate-artifacts
    (list (make-artifact "shared.css" "first pipeline")
          (make-artifact "shared.css" "second pipeline"))))

(let ((cache (make-content-cache "pipeline-test")))
  (test-equal "content cache starts empty" #f
    (content-cache-ref cache "font-key"))
  (test-assert "content cache stores byte values"
    (content-cache-store! cache "font-key" (string->utf8 "font bytes")))
  (test-equal "content cache restores verified byte values" "font bytes"
    (utf8->string (content-cache-ref cache "font-key")))
  (test-equal "content cache namespaces isolate keys" #f
    (content-cache-ref (make-content-cache "other-pipeline") "font-key")))

(let ((left (make-content-cache (string-append "a" (string #\nul) "b")))
      (right (make-content-cache "a")))
  (content-cache-store! left "c" (string->utf8 "left"))
  (test-equal "content cache identities distinguish embedded NUL boundaries"
    #f
    (content-cache-ref right (string-append "b" (string #\nul) "c"))))

(let* ((namespace (string-copy "snapshot-namespace"))
       (cache (make-content-cache namespace))
       (bytes (string->utf8 "stable bytes")))
  (content-cache-store! cache "key" bytes)
  (string-set! namespace 0 (integer->char 88))
  (test-equal "content cache snapshots its namespace"
    bytes
    (content-cache-ref cache "key")))

(let* ((bytes (string->utf8 "content whose cache blob path is blocked"))
       (artifact-directory (string-append store "/artifacts"))
       (blob-path (string-append artifact-directory "/"
                                 (content-fingerprint bytes)))
       (cache (make-content-cache "blocked-content-cache")))
  (unless (file-exists? artifact-directory)
    (mkdir artifact-directory))
  (mkdir blob-path)
  (test-equal "content cache reports a failed blob write"
    #f
    (content-cache-store! cache "key" bytes))
  (test-equal "content cache does not index a missing blob"
    #f
    (content-cache-ref cache "key"))
  (rmdir blob-path))

(let* ((content "pipeline output whose cache blob path is blocked")
       (artifact-directory (string-append store "/artifacts"))
       (blob-path (string-append artifact-directory "/"
                                 (content-fingerprint content)))
       (index-count (length (index-blobs)))
       (runs 0)
       (pipeline
        (make-asset-pipeline
          #:name "blocked-output-cache"
          #:procedure
          (lambda (_input)
            (set! runs (1+ runs))
            (list (make-artifact "blocked-output.txt" content)))))
       (run (lambda ()
              (car (run-asset-pipelines
                    (list pipeline) "blocked-output-project")))))
  (mkdir blob-path)
  (test-equal "a pipeline still returns outputs when cache storage fails"
    content
    (artifact-content (car (run))))
  (test-equal "a pipeline does not index unavailable output blobs"
    index-count
    (length (index-blobs)))
  (rmdir blob-path)
  (run)
  (test-equal "a pipeline reruns after its output could not be cached"
    2 runs)
  (run)
  (test-equal "a pipeline caches outputs after storage recovers"
    2 runs))

(test-error "content cache rejects empty keys" #t
  (content-cache-ref (make-content-cache "pipeline-test") ""))

(let* ((result (run-pipelines "v1"))
       (artifacts (car result))
       (owners (cadr result)))
  (test-equal "the first run executes the procedure" 1 runs)
(test-equal "every returned output becomes an artifact"
    '("fig/one.txt" "fig/two.bin")
    (map artifact-path artifacts))
  (test-equal "string outputs keep their bytes"
    "alpha\n" (artifact-content (car artifacts)))
  (test-equal "bytevector outputs keep their bytes"
    (string->utf8 "beta")
    (artifact-content (cadr artifacts)))
  (test-assert "outputs declare the pipeline inputs as sources"
    (every (lambda (artifact)
             (lset= string=?
                    (list (canonicalize-path input-a)
                          (canonicalize-path input-b))
                    (artifact-sources artifact)))
           artifacts))
  (test-equal "owners name the pipeline"
    '("figures" "figures")
    (map cdr owners)))

(let* ((result
         (run-asset-pipelines
           (list (make-asset-pipeline
                   #:name "empty-dynamic-dependency"
                   #:procedure
                   (lambda (_input)
                     (call-with-input-file (depend-on! input-c) get-string-all)
                     '())))
           "empty-dynamic-dependency-project")))
  (test-equal "a pipeline may return no artifacts after reading an input"
    '() (car result))
  (test-assert "empty pipeline results still report runtime dependencies"
    (member (canonicalize-path input-c) (caddr result) string=?)))

(define (run-runtime-loaded-pipeline)
  (car (run-asset-pipelines
         (list (make-asset-pipeline
                #:name "runtime-load"
                #:inputs (list input-a)
                #:procedure
                (lambda (in)
                  (set! runtime-runs (+ runtime-runs 1))
                  (load runtime-script)
                  (list (make-artifact "runtime.txt" runtime-marker)))))
         (current-project-key))))

(define (artifact-text artifact)
  (let ((content (artifact-content artifact)))
    (if (string? content) content (utf8->string content))))

(define first-runtime-artifact (car (run-runtime-loaded-pipeline)))
(test-equal "the first execution loads its runtime Scheme dependency"
  "one" (artifact-text first-runtime-artifact))
(test-assert "newly executed artifacts retain runtime Scheme provenance"
  (member (canonicalize-path runtime-script)
          (artifact-sources first-runtime-artifact)
          string=?))
(define cached-runtime-artifact (car (run-runtime-loaded-pipeline)))
(test-equal "unchanged runtime Scheme does not rerun the procedure"
  "one" (artifact-text cached-runtime-artifact))
(test-assert "restored artifacts retain runtime Scheme provenance"
  (member (canonicalize-path runtime-script)
          (artifact-sources cached-runtime-artifact)
          string=?))
(test-equal "unchanged runtime Scheme loads reuse cached output"
  1 runtime-runs)
(call-with-output-file runtime-script
  (lambda (port) (display "(set! runtime-marker \"two\")\n" port)))
(test-equal "editing a runtime-loaded Scheme dependency invalidates the cache"
  "two" (artifact-text (car (run-runtime-loaded-pipeline))))
(test-equal "the changed runtime dependency reruns its pipeline" 2 runtime-runs)

(let ((runs 0))
  (define (run-dynamic-input-pipeline)
    (car (run-asset-pipelines
           (list (make-asset-pipeline
                   #:name "runtime-input"
                   #:procedure
                   (lambda (_input)
                     (set! runs (1+ runs))
                     (let ((path (depend-on! input-c)))
                       (list (make-artifact
                               "runtime-input.txt"
                               (call-with-input-file path get-string-all)))))))
           (current-project-key))))
  (let ((first (car (run-dynamic-input-pipeline))))
    (test-equal "a pipeline can register a discovered input at runtime"
      "alpha" (artifact-text first))
    (test-assert "runtime inputs become artifact source dependencies"
      (member (canonicalize-path input-c) (artifact-sources first) string=?)))
  (run-dynamic-input-pipeline)
  (test-equal "unchanged runtime inputs reuse pipeline output" 1 runs)
  (call-with-output-file input-c
    (lambda (port) (display "gamma" port)))
  (test-equal "a changed runtime input invalidates pipeline output"
    "gamma" (artifact-text (car (run-dynamic-input-pipeline))))
  (test-equal "a changed runtime input reruns its pipeline" 2 runs))

(define (run-constant-pipeline)
  (car (run-asset-pipelines
         (list (make-asset-pipeline
                #:name "constant-output"
                #:procedure
                (lambda (_)
                  (set! constant-runs (+ constant-runs 1))
                  (list (make-artifact "constant.txt" "fixed")))))
         (current-project-key))))

(test-equal "a self-contained pipeline needs no declared file inputs"
  "fixed" (artifact-text (car (run-constant-pipeline))))
(test-equal "self-contained pipelines cache against their project identity"
  "fixed" (artifact-text (car (run-constant-pipeline))))
(test-equal "a cache hit skips a self-contained pipeline" 1 constant-runs)

(let ((runs 0))
  (define (run-empty-pipeline)
    (run-asset-pipelines
      (list (make-asset-pipeline
              #:name "empty-output"
              #:procedure
              (lambda (_)
                (set! runs (1+ runs))
                '())))
      (current-project-key)))
  (test-equal "a pipeline may produce no artifacts" '()
    (car (run-empty-pipeline)))
  (test-equal "empty pipeline outputs are cached" '()
    (car (run-empty-pipeline)))
  (test-equal "a cache hit skips an empty pipeline" 1 runs))

(let* ((result (run-pipelines "v1"))
       (artifacts (car result)))
  (test-equal "an unchanged identity skips the procedure" 1 runs)
  (test-equal "the skip still materializes the outputs"
    '("fig/one.txt" "fig/two.bin")
    (map artifact-path artifacts))
  (test-equal "restored bytes are exact"
    (string->utf8 "beta")
    (artifact-content (cadr artifacts))))

(call-with-output-file input-b
  (lambda (port) (display "beta-2" port)))
(let ((artifacts (car (run-pipelines "v1"))))
  (test-equal "an input change re-runs the procedure" 2 runs)
  (test-equal "the re-run reflects the new input"
    (string->utf8 "beta-2")
    (artifact-content (cadr artifacts))))

(let ((artifacts (car (run-pipelines "v2"))))
  (test-equal "a version bump re-runs the procedure" 3 runs)
  (test-equal "the bump does not change the bytes"
    (string->utf8 "beta-2")
    (artifact-content (cadr artifacts))))

(let* ((stage-one
        (make-asset-pipeline
         #:name "independent-stage-one"
         #:inputs (list input-a)
         #:procedure
         (lambda (in)
           (list (make-artifact "assets/intermediate.txt"
                   (call-with-input-file (in input-a) get-string-all))))))
       (stage-two
        (make-asset-pipeline
         #:name "independent-stage-two"
         #:inputs '("assets/intermediate.txt")
         #:procedure
         (lambda (in)
           (list (make-artifact "assets/final.txt"
                   (call-with-input-file (in "assets/intermediate.txt")
                     get-string-all)))))))
  (test-assert "one pipeline cannot consume another pipeline's output artifact"
    (catch #t
      (lambda ()
        (run-asset-pipelines (list stage-one stage-two)
                             (current-project-key))
        #f)
      (lambda args #t))))

(let ((result
       (run-asset-pipelines
        (list
         (make-asset-pipeline
          #:name "composed-stages"
          #:version "stage-one-v1-stage-two-v1"
          #:inputs (list input-a)
          #:procedure
          (lambda (in)
            (let* ((source (call-with-input-file (in input-a) get-string-all))
                   (intermediate (string-append source "\nstage-one"))
                   (final (string-append intermediate "\nstage-two")))
              (list (make-artifact "assets/composed-intermediate.txt" intermediate)
                    (make-artifact "assets/composed-final.txt" final))))))
        (current-project-key))))
  (test-equal "one Guile pipeline can compose and return multiple stages"
    '("assets/composed-intermediate.txt" "assets/composed-final.txt")
    (map artifact-path (car result)))
  (test-equal "the composed final artifact includes both stages"
    "alpha\nstage-one\nstage-two"
    (artifact-content (cadr (car result)))))

(parameterize ((current-project-key "project-v2"))
  (let ((artifacts (car (run-pipelines "v2"))))
    (test-equal "a project fingerprint change re-runs" 4 runs)
    (test-assert "the re-run still materializes"
      (string=? "alpha\n" (artifact-content (car artifacts))))))

(let ((indexes (index-blobs)))
  (test-assert "the pipeline wrote index blobs" (pair? indexes))
  (for-each
   (lambda (name) (overwrite-blob name "garbage"))
   indexes)
  (let ((artifacts (car (run-pipelines "v2"))))
    (test-equal "a corrupted index re-runs the procedure" 5 runs)
    (test-assert "the re-run rewrites a valid index"
      (pair? (index-blobs)))))

(let ((before (index-blobs))
      (runs 0))
  (define pipeline
    (make-asset-pipeline
      #:name "semantic-index-validation"
      #:inputs (list input-a)
      #:procedure
      (lambda (_input)
        (call-with-input-file (depend-on! input-a) get-string-all)
        (set! runs (1+ runs))
        (list (make-artifact "semantic-index.txt" "valid")))))
  (define (run)
    (run-asset-pipelines (list pipeline) (current-project-key)))
  (run)
  (let* ((new-indexes (lset-difference string=? (index-blobs) before))
         (name (car new-indexes))
         (path (string-append store "/artifacts/" name))
         (index (call-with-input-file path read))
         (entries (cadddr index)))
    (test-equal "the semantic-corruption fixture creates one pipeline index"
      1 (length new-indexes))
    (overwrite-blob
      name
      (call-with-output-string
        (lambda (port)
          (write (list (car index) (cadr index) (caddr index)
                       (append entries (list (car entries))))
                 port))))
    (run)
    (test-equal "a semantically invalid index re-runs the pipeline" 2 runs)
    (let ((valid-index (call-with-input-file path read)))
      (overwrite-blob
        name
        (call-with-output-string
          (lambda (port)
            (write valid-index port)
            (newline port)
            (write 'trailing-datum port)))))
    (run)
    (test-equal "a pipeline index with trailing data is a cache miss" 3 runs)
    (let* ((valid-index (call-with-input-file path read))
           (dependencies (caddr valid-index)))
      (test-assert "the pipeline index records runtime dependencies"
        (pair? dependencies))
      (let* ((dependency (car dependencies))
             (invalid-dependencies
               (cons (list (string-append "bad-path" (string #\nul))
                           (cadr dependency))
                     (cdr dependencies))))
        (overwrite-blob
          name
          (call-with-output-string
            (lambda (port)
              (write (list (car valid-index) (cadr valid-index)
                           invalid-dependencies (cadddr valid-index))
                     port))))
        (run)
        (test-equal "a pipeline index with an invalid dependency path is a cache miss"
          4 runs))
      (let* ((valid-index (call-with-input-file path read))
             (dependencies (caddr valid-index)))
        (overwrite-blob
          name
          (call-with-output-string
            (lambda (port)
              (write (list (car valid-index) (cadr valid-index)
                           (append dependencies (list (car dependencies)))
                           (cadddr valid-index))
                     port))))
        (run)
        (test-equal "a pipeline index with duplicate dependencies is a cache miss"
          5 runs)))))

(let ((contents (content-blobs)))
  (test-assert "content blobs exist" (pair? contents))
  (for-each
   (lambda (name) (overwrite-blob name "corrupted"))
   contents)
  (let ((artifacts (car (run-pipelines "v2"))))
    (test-equal "corrupted content re-runs the procedure" 6 runs)
    (test-equal "recovered bytes are exact"
      "alpha\n" (artifact-content (car artifacts)))))

(test-assert "an invalid pipeline artifact path fails"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "bad-path"
              #:inputs (list input-a)
              #:procedure (lambda (_) (list (make-artifact "../evil.png" "x")))))
       (current-project-key))
      #f)
    (lambda args #t)))

(test-assert "an undeclared input request fails the pipeline"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "bad-input"
              #:inputs (list input-a)
              #:procedure (lambda (in)
                            (list (make-artifact "ok.txt"
                                    (call-with-input-file (in "/etc/passwd")
                                      get-string-all))))))
       (current-project-key))
      #f)
    (lambda args #t)))

(let* ((producer-runs 0)
       (producer
        (make-asset-pipeline
          #:name "fanout-producer"
          #:procedure
          (lambda (_)
            (set! producer-runs (1+ producer-runs))
            (list (make-byte-artifact "private/shared.bin"
                    (string->utf8 "shared bytes"))))))
       (reference (pipeline-output producer "private/shared.bin"))
       (make-consumer
        (lambda (name output)
          (make-asset-pipeline
            #:name name
            #:inputs (list reference)
            #:procedure
            (lambda (in)
              (list (make-artifact output
                      (call-with-input-file (in reference) get-string-all)))))))
       (left (make-consumer "fanout-left" "site/left.txt"))
       (right (make-consumer "fanout-right" "site/right.txt"))
       (result (run-asset-pipelines (list right producer left)
                                    "pipeline-fanout-project")))
  (test-equal "one producer can feed multiple consumer pipelines"
    '("site/left.txt" "site/right.txt")
    (sort (map artifact-path (car result)) string<?))
  (test-equal "fan-out consumers receive identical producer bytes"
    '("shared bytes" "shared bytes")
    (map artifact-content (car result)))
  (test-equal "a shared producer runs once for all consumers" 1 producer-runs))

(let* ((events '())
       (runs 0)
       (pipeline
        (make-asset-pipeline
          #:name "progress-observable"
          #:procedure
          (lambda (_)
            (set! runs (1+ runs))
            (list (make-artifact "progress-observable.txt" "done")))))
      (project-key "pipeline-progress-project"))
  (define* (run #:key (cache? #t))
    (run-asset-pipelines
      (list pipeline) project-key
      #:cache? cache?
      #:progress (lambda (stage detail)
                   (set! events (append events (list (cons stage detail)))))))
  (run)
  (test-equal "pipeline progress reports start and completion in order"
    '(pipeline-start pipeline) (map car events))
  (test-equal "pipeline start identifies the named step"
    "progress-observable" (cdr (car events)))
  (let ((detail (cdr (cadr events))))
    (test-equal "pipeline completion reports a cache miss" 'miss (cadr detail))
    (test-assert "pipeline completion reports elapsed seconds"
      (and (number? (caddr detail)) (>= (caddr detail) 0)))
    (test-equal "pipeline completion reports output count" 1 (cadddr detail)))
  (set! events '())
  (run)
  (test-equal "pipeline progress reports cache hits" 'hit
    (cadr (cdr (cadr events))))
  (test-equal "pipeline progress does not rerun a cached step" 1 runs)
  (set! events '())
  (run #:cache? #f)
  (test-equal "pipeline progress identifies disabled caching" 'disabled
    (cadr (cdr (cadr events))))
  (test-equal "uncached pipeline progress reflects the procedure run" 2 runs))

(let* ((producer
        (make-asset-pipeline
          #:name "missing-output-producer"
          #:procedure (lambda (_) '())))
       (reference (pipeline-output producer "missing.txt"))
       (consumer
        (make-asset-pipeline
          #:name "missing-output-consumer"
          #:inputs (list reference)
          #:procedure (lambda (_) '()))))
  (test-assert "a consumer fails clearly when its producer omits a referenced output"
    (catch #t
      (lambda ()
        (run-asset-pipelines (list consumer producer)
                             "missing-pipeline-output-project")
        #f)
      (lambda args #t))))

(test-assert "a pipeline must return artifacts rather than arbitrary values"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "invalid-result"
              #:procedure (lambda (_) #t)))
       (current-project-key))
      #f)
    (lambda args #t)))

(let* ((source (string-append inputs-directory "/graph-source.txt"))
       (producer-runs 0)
       (producer-content "one")
       (consumer-runs 0)
       (producer
        (make-asset-pipeline
          #:name "graph-source"
          #:inputs (list source)
          #:procedure
          (lambda (in)
            (set! producer-runs (1+ producer-runs))
            (call-with-input-file (in source) get-string-all)
            (list (make-artifact "private/source.txt"
                    producer-content)))))
       (source-ref (pipeline-output producer "private/source.txt"))
       (consumer #f)
       (run (lambda ()
              (run-asset-pipelines (list consumer producer)
                                   "pipeline-graph-project"))))
  (define (make-consumer reference)
    (make-asset-pipeline
      #:name "graph-consumer"
      #:inputs (list reference)
      #:procedure
      (lambda (in)
        (set! consumer-runs (1+ consumer-runs))
        (list (make-artifact "site/final.txt"
                (string-append "consumed: "
                  (call-with-input-file (in reference) get-string-all)))))))
  (call-with-output-file source (lambda (port) (display "initial input" port)))
  (set! consumer (make-consumer source-ref))
  (let ((result (run)))
    (test-equal "pipeline output reference schedules producer before consumer"
      '("site/final.txt")
      (map artifact-path (car result)))
    (test-equal "consumed producer output is private by default"
      "consumed: one"
      (artifact-content (car (car result))))
    (test-equal "the first graph build runs each stage once"
      '(1 1) (list producer-runs consumer-runs)))
  (run)
  (test-equal "warm graph build reuses both stages"
    '(1 1) (list producer-runs consumer-runs))
  (call-with-output-file source
    (lambda (port) (display "changed input, same output" port)))
  (run)
  (test-equal "same upstream bytes preserve consumer cache entries"
    '(2 1) (list producer-runs consumer-runs))
  (set! producer-content "two")
  (call-with-output-file source
    (lambda (port) (display "changed input and output" port)))
  (let ((result (run)))
    (test-equal "source content changes propagate through graph edges"
      '(3 2) (list producer-runs consumer-runs))
    (test-equal "consumer receives changed producer bytes"
      "consumed: two"
      (artifact-content (car (car result)))))
  (let* ((output-directory (string-append root "/graph-project-output"))
         (project
          (make-project
            #:collect-sources (lambda () '())
            #:read-source (lambda (_) #f)
            #:pipelines (list consumer producer)
            #:output-directory output-directory))
         (artifacts (build-project project)))
    (test-equal "project builds publish only the graph's final artifact"
      '("site/final.txt") (map artifact-path artifacts))
    (test-equal "project builds publish graph artifacts to disk"
      "consumed: two"
      (call-with-input-file (string-append output-directory "/site/final.txt")
        get-string-all))))

(let* ((producer
        (make-asset-pipeline
          #:name "publish-consumed-producer"
          #:publish-consumed? #t
          #:procedure
          (lambda (_)
            (list (make-artifact "shared/intermediate.txt" "shared")))))
       (reference (pipeline-output producer "shared/intermediate.txt"))
       (consumer
        (make-asset-pipeline
          #:name "publish-consumed-consumer"
          #:inputs (list reference)
          #:procedure
          (lambda (in)
            (list (make-artifact "site/also-final.txt"
                    (call-with-input-file (in reference) get-string-all))))))
       (artifacts
        (car (run-asset-pipelines (list consumer producer)
                                  "pipeline-publish-project"))))
  (test-equal "a producer can publish an output that is also consumed"
    '("shared/intermediate.txt" "site/also-final.txt")
    (sort (map artifact-path artifacts) string<?)))

(test-error "pipeline outputs must name a real producer" #t
  (pipeline-output #f "not-an-artifact.txt"))

(let* ((producer
        (make-asset-pipeline
          #:name "omitted-graph-producer"
          #:procedure (lambda (_) (list (make-artifact "x.txt" "x")))))
       (reference (pipeline-output producer "x.txt"))
       (consumer
        (make-asset-pipeline
          #:name "omitted-graph-consumer"
          #:inputs (list reference)
          #:procedure (lambda (_) '()))))
  (test-assert "a graph rejects a producer omitted from the project"
    (catch #t
      (lambda ()
        (run-asset-pipelines (list consumer) "omitted-producer-project")
        #f)
      (lambda args #t)))
  (test-error "project construction rejects a producer omitted from its graph"
    #t
    (make-project
      #:collect-sources (lambda () '())
      #:read-source (lambda (_) #f)
      #:pipelines (list consumer))))

(delete-file input-b)
(test-assert "a missing input fails the pipeline loudly"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "missing-input"
              #:inputs (list input-b)
              #:procedure
              (lambda (in)
                (list (make-artifact "x.txt"
                        (call-with-input-file (in input-b)
                          get-string-all))))))
       (current-project-key))
      #f)
    (lambda args #t)))

(test-end "grove asset pipelines")
(system* "rm" "-rf" root)
