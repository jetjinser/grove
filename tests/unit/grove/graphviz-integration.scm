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
(define fake-bin (string-append root "/fake-bin"))
(define fake-dot (string-append fake-bin "/dot"))
(define fake-log (string-append root "/dot-runs"))

(define (render)
  (car (run-asset-pipelines
         (list (make-graphviz-pipeline source "assets/diagram.svg"))
         (current-project-key))))

(define (write-diagram text)
  (call-with-output-file source
    (lambda (port) (display text port))))

(define (write-fake-dot version marker)
  (call-with-output-file fake-dot
    (lambda (port)
      (format port
              "#!/bin/sh\nif [ \"$1\" = \"-V\" ]; then\n  echo 'dot - graphviz version ~a' >&2\n  exit 0\nfi\nprintf '%s\\n' >> '~a' render\nprintf '%s\\n' '<svg>~a</svg>'\n"
              version fake-log marker)))
  (chmod fake-dot #o755))

(define (render-run-count)
  (if (file-exists? fake-log)
      (call-with-input-file fake-log
        (lambda (port)
          (let loop ((count 0))
            (if (eof-object? (get-line port)) count
                (loop (+ count 1))))))
      0))

(setenv "GROVE_STORE_DIR" (string-append root "/store"))
(current-project-key "graphviz-integration-v1")
(mkdir root)
(mkdir fake-bin)
(write-fake-dot "1.0" "tool-one")
(write-diagram "digraph Grove { source -> artifact -> renderer; }\n")

(test-begin "external tool asset pipeline")

(let* ((old-path (getenv "PATH"))
       (first #f)
       (svg #f))
  (dynamic-wind
    (lambda ()
      (setenv "PATH" (string-append fake-bin ":" old-path)))
    (lambda ()
      (set! first (render))
      (set! svg (artifact-content (car first)))
      (test-equal "Graphviz emits the requested SVG artifact"
        "assets/diagram.svg" (artifact-path (car first)))
      (test-assert "Graphviz output is valid SVG"
        (string-contains (utf8->string svg) "<svg"))
      (test-assert "pipeline output tracks the declared source file"
        (member (canonicalize-path source)
                (artifact-sources (car first)) string=?))
      (let ((runs (render-run-count)))
        (test-equal "an unchanged input and tool restore cached bytes"
          svg (artifact-content (car (render))))
        (test-equal "a cache hit does not run Graphviz again"
          runs (render-run-count)))
      (parameterize ((current-project-key "graphviz-integration-v2"))
        (test-assert "a fresh pipeline execution is reproducible"
          (bytevector=? svg (artifact-content (car (render))))))
      (write-diagram "digraph Grove { source -> tool -> artifact -> renderer; }\n")
      (let ((runs (render-run-count)))
        (render)
        (test-equal "changing a declared input reruns Graphviz"
          (+ runs 1) (render-run-count)))
      (parameterize ((current-project-key "graphviz-replaced-tool"))
        (write-fake-dot "1.0" "tool-one")
        (let ((same-tool (artifact-content (car (render))))
              (runs (render-run-count)))
          (test-equal "same tool identity reuses its output"
            runs (begin (render) (render-run-count)))
          (write-fake-dot "2.0" "tool-two")
          (test-equal "a changed tool version invalidates cache automatically"
            (string->utf8 "<svg>tool-two</svg>\n")
            (artifact-content (car (render))))
          (test-assert "the new version produced different output"
            (not (bytevector=? same-tool
                               (artifact-content (car (render)))))))))
    (lambda ()
      (if old-path
          (setenv "PATH" old-path)
          (unsetenv "PATH")))))

(test-end "external tool asset pipeline")
(system* "rm" "-rf" root)
