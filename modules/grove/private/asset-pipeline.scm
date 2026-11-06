;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private asset-pipeline)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (%make-asset-pipeline
            asset-pipeline?
            asset-pipeline-name
            asset-pipeline-version
            asset-pipeline-inputs
            asset-pipeline-procedure
            asset-pipeline-publish-consumed?
            %make-pipeline-output
            pipeline-output?
            pipeline-output-pipeline
            pipeline-output-path
            pipeline-input?
            pipeline-input-key=?
            pipeline-references-contained?))

(define-record-type <asset-pipeline>
  (%make-asset-pipeline name version inputs procedure publish-consumed?)
  asset-pipeline?
  (name asset-pipeline-name)
  (version asset-pipeline-version)
  (inputs asset-pipeline-inputs)
  (procedure asset-pipeline-procedure)
  (publish-consumed? asset-pipeline-publish-consumed?))

(define-record-type <pipeline-output>
  (%make-pipeline-output pipeline path)
  pipeline-output?
  (pipeline pipeline-output-pipeline)
  (path pipeline-output-path))

(define (pipeline-input? input)
  (or (string? input) (pipeline-output? input)))

(define (pipeline-input-key=? left right)
  (cond
   ((and (string? left) (string? right)) (string=? left right))
   ((and (pipeline-output? left) (pipeline-output? right))
    (and (eq? (pipeline-output-pipeline left)
              (pipeline-output-pipeline right))
         (string=? (pipeline-output-path left) (pipeline-output-path right))))
   (else #f)))

(define (pipeline-references-contained? pipeline pipelines)
  (every (lambda (input)
           (or (not (pipeline-output? input))
               (memq (pipeline-output-pipeline input) pipelines)))
         (asset-pipeline-inputs pipeline)))
