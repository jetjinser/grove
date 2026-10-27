;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove pipeline)
  #:declarative? #f
  #:use-module (grove cache)
  #:use-module (grove output)
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

(define root (string-append "/tmp/grove-pipeline-test-"
                            (number->string (getpid))))
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
(current-project-key "project-v1")
(mkdir root)
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

(let ((cache (make-content-cache "pipeline-test")))
  (test-equal "content cache starts empty" #f
    (content-cache-ref cache "font-key"))
  (test-assert "content cache stores byte values"
    (content-cache-store! cache "font-key" (string->utf8 "font bytes")))
  (test-equal "content cache restores verified byte values" "font bytes"
    (utf8->string (content-cache-ref cache "font-key")))
  (test-equal "content cache namespaces isolate keys" #f
    (content-cache-ref (make-content-cache "other-pipeline") "font-key")))

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

(test-error "content cache rejects empty keys" #t
  (content-cache-ref (make-content-cache "pipeline-test") ""))

(let* ((result (run-pipelines "v1"))
       (artifacts (car result))
       (owners (cdr result)))
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

(test-assert "a pipeline must return at least one artifact"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "no-outputs"
              #:inputs (list input-a)
              #:procedure (lambda (_) '())))
       (current-project-key))
      #f)
    (lambda args #t)))

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
