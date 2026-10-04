;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove cli)
  #:use-module (grove project)
  #:use-module (grove project load)
  #:use-module (grove project diagnostic)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-13)
  #:export (main))

(define (usage-error message . arguments)
  (throw 'grove-usage-error (apply format #f message arguments)))

(define (parse-options arguments)
  (let loop ((arguments arguments) (command #f) (options '()))
    (match arguments
      (()
       (unless (or command (assq 'help options))
         (usage-error "expected a command; use 'grove --help'"))
       (when (and (equal? command "check") (assq 'output options))
         (usage-error "check does not accept --output"))
       (cons command options))
      ((argument . rest)
       (cond
         ((member argument '("build" "check"))
          (when command (usage-error "unexpected command: ~a" argument))
          (loop rest argument options))
         ((member argument '("--help" "-h" "--debug"))
          (loop rest command
                (acons (if (string=? argument "--debug") 'debug 'help) #t options)))
         ((member argument '("--project" "--output"))
          (let ((key (if (string=? argument "--project") 'project 'output)))
            (when (assq key options)
              (usage-error "duplicate option: ~a" argument))
            (match rest
              (((? (lambda (value)
                     (and (not (string-null? value))
                          (not (string-prefix? "-" value)))) value) . tail)
               (loop tail command (acons key value options)))
              (_ (usage-error "~a requires a path" argument)))))
         ((and (string-prefix? "--" argument) (string-index argument #\=))
          (let ((index (string-index argument #\=)))
            (loop (cons (substring argument 0 index)
                        (cons (substring argument (+ index 1)) rest))
                  command options)))
         (else (usage-error "unknown command or option: ~a" argument)))))))

(define (print-help command)
  (display
    (cond
      ((equal? command "build")
       "Usage: grove build [OPTIONS]\n\nBuild the project and update its outputs.\n")
      ((equal? command "check")
       "Usage: grove check [OPTIONS]\n\nRead, analyze, render, and validate without writing outputs.\n")
      (else
       "Usage: grove COMMAND [OPTIONS]\n\nCommands:\n  build  Build the project and update its outputs.\n  check  Validate the complete pipeline without writing outputs.\n")))
  (display "\nOptions:\n  --project PATH  Project file (default: ./project.scm).\n")
  (unless (equal? command "check")
    (display "  --output PATH   Override the output directory (build only).\n"))
  (display "  --debug         Show the original exception and backtrace.\n  -h, --help      Show help.\n\nOption paths are relative to the invocation directory.\nProject paths are relative to the project file's directory.\n"))

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

(define (run-command command options)
  (let ((filename (or (assq-ref options 'project) "project.scm")))
    (call-with-loaded-project filename
      (lambda (project)
        (if (string=? command "check")
            (let ((report (check-project project)))
              (format #t "Checked ~d artifact~:p.\n"
                      (length (build-report-artifacts report))))
            (let ((report (build-project/report project)))
              (format #t "Built ~d artifact~:p (~d affected).~%Output: ~a~%"
                      (length (build-report-artifacts report))
                      (length (build-report-affected-artifacts report))
                      (canonicalize-path (project-output-directory project)))
              (unless (build-report-rendered? report)
                (display "Renderer cache hit.\n")))))
      #:output-directory (assq-ref options 'output))))

(define (main arguments)
  "Run Grove with ARGUMENTS (excluding the program name), returning an exit code."
  (catch 'grove-usage-error
    (lambda ()
      (let* ((parsed (parse-options arguments))
             (command (car parsed))
             (options (cdr parsed)))
        (if (assq-ref options 'help)
            (begin (print-help command) 0)
            (catch 'grove-project-error
              (lambda ()
                (call-with-project-stage 'execute #f
                  (lambda () (run-command command options)))
                0)
              (lambda (_ error)
                (print-project-error error (assq-ref options 'debug))
                1)))))
    (lambda (_ message)
      (format (current-error-port) "grove: ~a~%" message)
      2)))
