;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project diagnostic)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (print-project-error
            call-with-project-stage
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
