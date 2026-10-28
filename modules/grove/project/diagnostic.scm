;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project diagnostic)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (print-project-error
            call-with-project-stage
            project-error? project-error-stage project-error-path
            project-error-key project-error-arguments project-error-stack))

(define-record-type <project-error>
  (%make-project-error stage path key arguments stack)
  project-error?
  (stage project-error-stage)
  (path project-error-path/raw)
  (key project-error-key/raw)
  (arguments project-error-arguments/raw)
  (stack project-error-stack))

(define (make-project-error stage path key arguments stack)
  (%make-project-error stage (snapshot-data path) (snapshot-data key)
                       (snapshot-data arguments) stack))

(define (project-error-path error)
  (snapshot-data (project-error-path/raw error)))

(define (project-error-key error)
  (snapshot-data (project-error-key/raw error)))

(define (project-error-arguments error)
  (snapshot-data (project-error-arguments/raw error)))

(define (call-with-project-stage stage path thunk)
  "Attach execution context without flattening the original exception.
STAGE is a symbol, PATH is a non-empty path string or #f, and THUNK is a
procedure. Invalid context is rejected before THUNK runs."
  (unless (symbol? stage)
    (error "project stage must be a symbol" stage))
  (unless (or (not path) (non-empty-string? path))
    (error "project stage path must be a non-empty string or #f" path))
  (unless (procedure? thunk)
    (error "project stage requires a thunk" thunk))
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

(define (print-project-error error debug?)
  (let* ((port (current-error-port))
         (message
           (call-with-output-string
             (lambda (output)
               (print-exception output #f
                                (project-error-key error)
                                (project-error-arguments error))))))
    (format port "grove: ~a failed~a~%"
            (project-error-stage error)
            (if (project-error-path error)
                (format #f " (~a)" (project-error-path error)) ""))
    (if debug?
        (begin
          (when (project-error-stack error)
            (display-backtrace (project-error-stack error) port))
          (display message port))
        (begin
          (display (string-take message (min 700 (string-length message))) port)
          (when (> (string-length message) 700)
            (display "\n… use --debug for the full diagnostic.\n" port))))))
