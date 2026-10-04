;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove cli)
  #:use-module (grove project)
  #:use-module (grove serve)
  #:use-module (grove private cli progress)
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
       (when (and (not (equal? command "serve"))
                  (or (assq 'port options) (assq 'no-watch options)))
         (usage-error "--port and --no-watch are serve options"))
       (cons command options))
      ((argument . rest)
       (cond
         ((member argument '("build" "check" "serve"))
          (when command (usage-error "unexpected command: ~a" argument))
          (loop rest argument options))
         ((member argument '("--help" "-h" "--debug" "--no-watch"))
          (loop rest command
                (acons (cond ((string=? argument "--debug") 'debug)
                             ((string=? argument "--no-watch") 'no-watch)
                             (else 'help)) #t options)))
         ((member argument '("--project" "--output" "--port"))
          (let ((key (cond ((string=? argument "--project") 'project)
                           ((string=? argument "--output") 'output)
                           (else 'port))))
            (when (assq key options)
              (usage-error "duplicate option: ~a" argument))
            (match rest
              (((? (lambda (value)
                     (and (not (string-null? value))
                          (not (string-prefix? "-" value)))) value) . tail)
               (let ((value (if (eq? key 'port) (string->number value) value)))
                 (when (and (eq? key 'port)
                            (not (and (exact-integer? value) (<= 1 value 65535))))
                   (usage-error "--port requires an integer from 1 to 65535"))
                 (loop tail command (acons key value options))))
              (_ (usage-error "~a requires a value" argument)))))
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
      ((equal? command "serve")
       "Usage: grove serve [OPTIONS]\n\nServe successful build outputs on localhost and watch for changes.\n")
      (else
       "Usage: grove COMMAND [OPTIONS]\n\nCommands:\n  build  Build the project and update its outputs.\n  check  Validate the complete pipeline without writing outputs.\n  serve  Preview on localhost and rebuild when inputs change.\n")))
  (display "\nOptions:\n  --project PATH  Project file (default: ./project.scm).\n")
  (unless (equal? command "check")
    (display "  --output PATH   Override the output directory (build and serve).\n"))
  (when (or (not command) (equal? command "serve"))
    (display "  --port NUMBER   Listen on this port (serve only; default: 8080).\n  --no-watch      Serve without automatic rebuilding (serve only).\n"))
  (display "  --debug         Show the original exception and backtrace.\n  -h, --help      Show help.\n\nOption paths are relative to the invocation directory.\nProject paths are relative to the project file's directory.\n"))

(define (run-command command options)
  (let ((filename (or (assq-ref options 'project) "project.scm")))
    (if (string=? command "serve")
        (serve-project filename
          #:output-directory (assq-ref options 'output)
          #:port (or (assq-ref options 'port) 8080)
          #:watch? (not (assq-ref options 'no-watch))
          #:debug? (assq-ref options 'debug))
        (let* ((started (cli-seconds))
               (checking? (string=? command "check"))
               (result
                 (call-with-cli-progress (if checking? "Checking" "Building")
                   filename
                   (lambda (progress)
                     (call-with-loaded-project filename
                       (lambda (project)
                         (let ((report
                                 (if checking?
                                     (check-project project #:progress progress)
                                     (build-project/report project #:progress progress))))
                           (cons (and (not checking?)
                                      (canonicalize-path (project-output-directory project)))
                                 report)))
                       #:output-directory (assq-ref options 'output)))))
               (output (car result))
               (report (cdr result)))
          (print-cli-summary report (- (cli-seconds) started)
            #:checked? checking? #:output output)
          0))))

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
                  (lambda () (run-command command options))))
              (lambda (_ error)
                (print-project-error error (assq-ref options 'debug))
                1)))))
    (lambda (_ message)
      (format (current-error-port) "grove: ~a~%" message)
      2)))
