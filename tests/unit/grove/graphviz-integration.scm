;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove graphviz-integration)
  #:use-module (examples hackable-assets graphviz)
  #:use-module (grove output)
  #:use-module (grove private input cache)
  #:use-module (grove private pipeline)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define root (string-append "/tmp/grove-graphviz-"
                            (number->string (getpid))))
(define source (string-append root "/diagram.dot"))

(define (render version)
  (car (run-asset-pipelines
         (list (make-graphviz-pipeline source "assets/diagram.svg"
                                       #:version version))
         (current-project-key))))

(define (write-diagram text)
  (call-with-output-file source
    (lambda (port) (display text port))))

(define (store-blob-count)
  (length (scandir (string-append root "/store/artifacts")
                   (lambda (name)
                     (not (member name '("." "..")))))))

(setenv "GROVE_STORE_DIR" (string-append root "/store"))
(current-project-key "graphviz-integration-v1")
(mkdir root)
(write-diagram "digraph Grove { source -> artifact -> renderer; }\n")

(test-begin "external tool asset pipeline")

(let* ((first (render "graphviz-14.1.1-svg-v1"))
       (svg (artifact-content (car first))))
  (test-equal "Graphviz emits a binary SVG artifact"
    "assets/diagram.svg" (artifact-path (car first)))
  (test-assert "Graphviz output is valid SVG"
    (string-contains (utf8->string svg) "<svg"))
  (test-assert "pipeline output tracks the declared source file"
    (member (canonicalize-path source)
            (artifact-sources (car first)) string=?))
  (test-equal "an unchanged input restores identical SVG bytes"
    svg (artifact-content (car (render "graphviz-14.1.1-svg-v1"))))
  (let ((before (store-blob-count)))
    (test-equal "changing the declared tool version preserves SVG bytes"
      svg (artifact-content (car (render "graphviz-14.1.1-svg-v2"))))
    (test-assert "changing the declared tool version creates a new cache entry"
      (> (store-blob-count) before)))
  (parameterize ((current-project-key "graphviz-integration-v2"))
    (test-assert "a fresh pipeline execution is reproducible"
      (bytevector=? svg
                    (artifact-content
                      (car (render "graphviz-14.1.1-svg-v1"))))))

(write-diagram "digraph Grove { source -> tool -> artifact -> renderer; }\n")
(test-assert "changing the standalone source changes the rendered asset"
  (not (bytevector=?
         (artifact-content (car first))
         (artifact-content (car (render "graphviz-14.1.1-svg-v1")))))))

(test-end "external tool asset pipeline")
(system* "rm" "-rf" root)
