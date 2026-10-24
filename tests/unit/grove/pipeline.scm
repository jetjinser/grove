;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove pipeline)
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
;;     everything it emits rides the ordinary artifact machinery.

(define root (string-append "/tmp/grove-pipeline-test-"
                            (number->string (getpid))))
(define store (string-append root "/store"))
(define inputs-directory (string-append root "/inputs"))
(define input-a (string-append inputs-directory "/a.txt"))
(define input-b (string-append inputs-directory "/b.txt"))
(define runs 0)

(setenv "GROVE_STORE_DIR" store)
(current-project-key "project-v1")
(mkdir root)
(mkdir inputs-directory)
(call-with-output-file input-a
  (lambda (port) (display "alpha" port)))
(call-with-output-file input-b
  (lambda (port) (display "beta" port)))

(define (make-figures-pipeline version)
  (make-asset-pipeline
   #:name "figures"
   #:version version
   #:inputs (list input-a input-b)
   #:procedure
   (lambda (in emit)
     (set! runs (+ runs 1))
     (emit "fig/one.txt"
           (string-append (call-with-input-file (in input-a)
                            get-string-all)
                          "\n"))
     (emit "fig/two.bin"
           (string->utf8 (call-with-input-file (in input-b)
                            get-string-all))))))

(define (run-pipelines version)
  (run-asset-pipelines (list (make-figures-pipeline version))
                       (current-project-key)))

(define (blob-files)
  (scandir (string-append store "/artifacts")
           (lambda (name)
             (not (member name '("." ".."))))))

(define (blob->index? name)
  (let ((sexp (false-if-exception
               (call-with-input-file
                   (string-append store "/artifacts/" name)
                 read))))
    (and (list? sexp)
         (pair? sexp)
         (pair? (car sexp))
         (string? (caar sexp)))))

(define (index-blobs)
  (filter blob->index? (blob-files)))

(define (content-blobs)
  (lset-difference string=? (blob-files) (index-blobs)))

(define (overwrite-blob name text)
  (call-with-output-file (string-append store "/artifacts/" name)
    (lambda (port) (display text port))))

(test-begin "grove asset pipelines")

(let ((cache (make-content-cache "pipeline-test")))
  (test-equal "content cache starts empty" #f
    (content-cache-ref cache "font-key"))
  (test-assert "content cache stores byte values"
    (content-cache-store! cache "font-key" (string->utf8 "font bytes")))
  (test-equal "content cache restores verified byte values" "font bytes"
    (utf8->string (content-cache-ref cache "font-key")))
  (test-equal "content cache namespaces isolate keys" #f
    (content-cache-ref (make-content-cache "other-pipeline") "font-key")))

(test-error "content cache rejects empty keys" #t
  (content-cache-ref (make-content-cache "pipeline-test") ""))

(let* ((result (run-pipelines "v1"))
       (artifacts (car result))
       (owners (cdr result)))
  (test-equal "the first run executes the procedure" 1 runs)
  (test-equal "every emitted output becomes an artifact"
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

(test-assert "an invalid emitted path fails the pipeline"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "bad-path"
              #:inputs (list input-a)
              #:procedure (lambda (in emit) (emit "../evil.png" "x"))))
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
              #:procedure (lambda (in emit)
                            (emit "ok.txt"
                                  (call-with-input-file (in "/etc/passwd")
                                    get-string-all)))))
       (current-project-key))
      #f)
    (lambda args #t)))

(test-assert "an empty emission fails the pipeline"
  (catch #t
    (lambda ()
      (run-asset-pipelines
       (list (make-asset-pipeline
              #:name "no-outputs"
              #:inputs (list input-a)
              #:procedure (lambda (in emit) #t)))
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
              (lambda (in emit)
                (emit "x.txt"
                      (call-with-input-file (in input-b)
                        get-string-all)))))
       (current-project-key))
      #f)
    (lambda args #t)))

(test-end "grove asset pipelines")
(system* "rm" "-rf" root)
