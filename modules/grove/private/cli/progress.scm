;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private cli progress)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:export (call-with-cli-progress))

(define (display-path path)
  (let ((prefix (string-append (getcwd) "/")))
    (if (string-prefix? prefix path)
        (substring path (string-length prefix)) path)))

(define (call-with-cli-progress action filename procedure)
  "Run PROCEDURE with a progress callback; keep terminal updates on one line."
  (let* ((port (current-error-port))
         (terminal? (isatty? port))
         (report
           (lambda (stage path)
             (let ((label (case stage
                            ((load read) action)
                            ((analyze) "Analyzing")
                            ((render) "Rendering")
                            ((validate) "Validating")
                            ((write) "Writing")
                            (else #f))))
               (when label
                 (when terminal? (display "\r\x1b[2K" port))
                 (format port "~a~a~a" label (if path " " "")
                         (if path (display-path path) ""))
                 (unless terminal? (newline port))
                 (force-output port))))))
    (dynamic-wind
      (lambda () (report 'load filename))
      (lambda () (procedure report))
      (lambda ()
        (when terminal?
          (display "\r\x1b[2K" port)
          (force-output port))))))
