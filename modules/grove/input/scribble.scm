;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove input scribble)
  #:use-module (system base language)
  #:use-module (grove ir)
  #:use-module (language scribble spec)
  #:export (make-scribble-document-reader))

(define* (make-scribble-document-reader
           #:optional (environment (default-environment scribble)))
  (lambda (path)
    (let ((document
            (call-with-input-file path
              (lambda (port)
                (eval ((language-reader scribble) port environment)
                      environment)))))
      (unless (content-document? document)
        (error "Scribble source requires a content document" path))
      document)))
