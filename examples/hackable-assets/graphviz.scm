;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (examples hackable-assets graphviz)
  #:use-module (grove pipeline)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-13)
  #:export (make-graphviz-pipeline))

(define* (make-graphviz-pipeline input output #:key version)
  "Build a Grove asset pipeline that turns Graphviz INPUT into SVG OUTPUT.

VERSION identifies the Graphviz version and any rendering options that affect
the output. The input file is fingerprinted and passed to the pipeline through
Grove's declared-input accessor."
  (unless (and (string? version) (not (string-null? version)))
    (error "Graphviz pipeline requires a version identifier" version))
  (make-asset-pipeline
    #:name "graphviz"
    #:version version
    #:inputs (list input)
    #:procedure
    (lambda (in emit)
      (let* ((pipe (open-pipe* OPEN_READ "dot" "-Tsvg" (in input)))
             (svg (get-bytevector-all pipe))
             (status (close-pipe pipe)))
        (unless (zero? (status:exit-val status))
          (error "Graphviz failed to render diagram" input status))
        (emit output svg)))))
