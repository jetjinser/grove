;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project diagnostic)
  #:use-module (srfi srfi-9)
  #:export (call-with-project-stage
            project-error? project-error-stage project-error-path
            project-error-key project-error-arguments project-error-stack))

(define-record-type <project-error>
  (make-project-error stage path key arguments stack)
  project-error?
  (stage project-error-stage)
  (path project-error-path)
  (key project-error-key)
  (arguments project-error-arguments)
  (stack project-error-stack))

(define (call-with-project-stage stage path thunk)
  "Attach execution context without flattening the original exception."
  (let ((stack #f))
    (catch #t thunk
      (lambda (key . arguments)
        (if (eq? key 'grove-project-error)
            (apply throw key arguments)
            (throw 'grove-project-error
                   (make-project-error stage path key arguments stack))))
      (lambda (key . arguments)
        (unless (eq? key 'grove-project-error)
          (set! stack (make-stack #t)))))))
