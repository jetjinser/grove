;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (examples hackable-assets graphviz)
  #:use-module (grove pipeline)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:export (make-graphviz-pipeline))

(define (graphviz-version)
  ;; Graphviz writes its version to stderr; route it through stdout so the
  ;; extension can make the discovered tool identity part of Grove's cache key.
  (let* ((pipe (open-pipe* OPEN_READ "sh" "-c" "dot -V 2>&1"))
         (version (string-trim-both (get-string-all pipe)))
         (status (close-pipe pipe)))
    (unless (and (zero? (status:exit-val status))
                 (not (string-null? version)))
      (error "could not determine Graphviz version" status version))
    version))

(define (make-graphviz-pipeline input output)
  "Build a Grove asset pipeline that turns Graphviz INPUT into SVG OUTPUT.

The adapter probes the Graphviz executable on PATH and includes its reported
version in the pipeline identity. The input file is fingerprinted and passed
to the pipeline through Grove's declared-input accessor."
  (make-asset-pipeline
    #:name "graphviz"
    #:version (string-append "dot-svg-v1:" (graphviz-version))
    #:inputs (list input)
    #:procedure
    (lambda (in emit)
      (let* ((pipe (open-pipe* OPEN_READ "dot" "-Tsvg" (in input)))
             (svg (get-bytevector-all pipe))
             (status (close-pipe pipe)))
        (unless (zero? (status:exit-val status))
          (error "Graphviz failed to render diagram" input status))
        (emit output svg)))))
