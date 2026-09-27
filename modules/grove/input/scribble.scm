;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove input scribble)
  #:use-module (system base language)
  #:use-module (grove ir)
  #:use-module ((language scribble spec)
                #:select (scribble scribble-evaluate-document))
  #:export (load-scribble-modules!
            make-scribble-document-reader))

(define* (load-scribble-modules! paths
                                #:key
                                (read-document
                                 (make-scribble-document-reader)))
  "Evaluate PATHS as Scribble sources before reading documents that import
their named modules.  Each source selects its module through Guile's normal
`define-module' form."
  (for-each read-document paths)
  (values))

(define* (make-scribble-document-reader
           #:optional (environment #f)
           #:key (environment-factory
                   (lambda () (default-environment scribble))))
  (lambda (path)
    (call-with-input-file path
      (lambda (port)
        (let ((document
                (scribble-evaluate-document port
                                            (or environment
                                                (environment-factory))
                                            path)))
          (unless (content-document? document)
            (error "Scribble source requires a content document" path))
          document)))))
