;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove cli)
  #:use-module (grove project)
  #:use-module (grove project new)
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

(define (ssh-support)
  "Return the (grove ssh) module, or raise a clear error when it is unavailable."
  (or (catch #t
        (lambda () (resolve-module '(grove ssh) #:ensure #f))
        (lambda _ #f))
      (error "SSH support requires guile-ssh; this Grove was built without it")))

(define (ssh-ref name)
  (module-ref (ssh-support) name))

(define numeric-option-keys
  '(port ssh ssh-workers ssh-login-timeout ssh-session-timeout ssh-memory-limit))

(define (parse-options arguments)
  (let loop ((arguments arguments) (command #f) (options '()))
    (match arguments
      (()
       (unless (or command (assq 'help options))
         (usage-error "expected a command; use 'grove --help'"))
       (when (and (member command '("check" "browse")) (assq 'output options))
         (usage-error "~a does not accept --output" command))
       (when (and (not (equal? command "browse")) (assq 'ssh options))
         (usage-error "--ssh is a browse option"))
       (when (and (not (equal? command "browse")) (assq 'authorized-keys options))
         (usage-error "--authorized-keys is a browse option"))
       (when (and (not (member command '("browse" "ssh-keygen")))
                  (assq 'host-key options))
         (usage-error "--host-key is a browse and ssh-keygen option"))
       (when (and (not (equal? command "ssh-keygen")) (assq 'force options))
         (usage-error "--force is an ssh-keygen option"))
       (when (and (not (equal? command "browse"))
                  (or (assq 'ssh-workers options) (assq 'ssh-login-timeout options)
                      (assq 'ssh-session-timeout options)
                      (assq 'ssh-memory-limit options)))
         (usage-error "--ssh-workers, --ssh-login-timeout, --ssh-session-timeout and --ssh-memory-limit are browse options"))
       (when (and (not (equal? command "serve"))
                  (or (assq 'port options) (assq 'no-watch options)))
         (usage-error "--port and --no-watch are serve options"))
       (when (and (not (equal? command "new")) (assq 'dest options))
         (usage-error "--dest is a new option"))
       (when (and (equal? command "new") (assq 'output options))
         (usage-error "new does not accept --output"))
       (cons command options))
      ((argument . rest)
       (cond
         ((and (not command)
               (member argument
                       '("build" "check" "serve" "new" "browse" "ssh-keygen")))
          (loop rest argument options))
         ((member argument '("--help" "-h" "--debug" "--no-watch" "--force"))
          (loop rest command
                (acons (cond ((string=? argument "--debug") 'debug)
                             ((string=? argument "--no-watch") 'no-watch)
                             ((string=? argument "--force") 'force)
                             (else 'help)) #t options)))
         ((member argument
                  '("--project" "--output" "--port" "--dest"
                    "--ssh" "--host-key" "--authorized-keys"
                    "--ssh-workers" "--ssh-login-timeout" "--ssh-session-timeout"
                    "--ssh-memory-limit"))
          (let ((key (cond ((string=? argument "--project") 'project)
                           ((string=? argument "--output") 'output)
                           ((string=? argument "--dest") 'dest)
                           ((string=? argument "--ssh") 'ssh)
                           ((string=? argument "--host-key") 'host-key)
                           ((string=? argument "--authorized-keys") 'authorized-keys)
                           ((string=? argument "--ssh-workers") 'ssh-workers)
                           ((string=? argument "--ssh-login-timeout") 'ssh-login-timeout)
                           ((string=? argument "--ssh-session-timeout") 'ssh-session-timeout)
                           ((string=? argument "--ssh-memory-limit") 'ssh-memory-limit)
                           (else 'port))))
            (when (assq key options)
              (usage-error "duplicate option: ~a" argument))
            (match rest
              (((? (lambda (value)
                     (and (not (string-null? value))
                          (not (string-prefix? "-" value)))) value) . tail)
               (let ((value (if (memq key numeric-option-keys)
                                (string->number value)
                                value)))
                 (when (memq key numeric-option-keys)
                   (unless (exact-integer? value)
                     (usage-error "~a requires an integer" argument))
                   (case key
                     ((port ssh)
                      (unless (<= 1 value 65535)
                        (usage-error "~a requires an integer from 1 to 65535"
                                     argument)))
                     ((ssh-workers)
                      (unless (<= 1 value 64)
                        (usage-error "--ssh-workers must be from 1 to 64")))
                     ((ssh-login-timeout ssh-session-timeout)
                      (unless (>= value 0)
                        (usage-error "~a must be a non-negative number of seconds"
                                     argument)))
                     ((ssh-memory-limit)
                      (unless (> value 0)
                        (usage-error "--ssh-memory-limit must be a positive number of bytes")))))
                 (loop tail command (acons key value options))))
              (_ (usage-error "~a requires a value" argument)))))
         ((and (string-prefix? "--" argument) (string-index argument #\=))
          (let ((index (string-index argument #\=)))
            (loop (cons (substring argument 0 index)
                        (cons (substring argument (+ index 1)) rest))
                  command options)))
         ((and (equal? command "new") (not (string-prefix? "-" argument)))
          (when (assq 'prefix options) (usage-error "new accepts one prefix"))
          (loop rest command (acons 'prefix argument options)))
         (else (usage-error "unknown command or option: ~a" argument)))))))

(define (print-help command)
  (display
    (cond
      ((equal? command "new")
       "Usage: grove new [PREFIX] [OPTIONS]\n\nCreate a tree using PREFIX or the project default, with an increasing base36 ID.\n")
      ((equal? command "build")
       "Usage: grove build [OPTIONS]\n\nBuild the project and update its outputs.\n")
      ((equal? command "check")
       "Usage: grove check [OPTIONS]\n\nRead, analyze, render, and validate without writing outputs.\n")
      ((equal? command "serve")
       "Usage: grove serve [OPTIONS]\n\nServe successful build outputs on localhost and watch for changes.\n")
      ((equal? command "browse")
       "Usage: grove browse [OPTIONS]\n\nRead and analyze the project, then browse its model in the terminal.\n")
      ((equal? command "ssh-keygen")
       "Usage: grove ssh-keygen --host-key PATH [OPTIONS]\n\nGenerate an RSA host key for 'grove browse --ssh'.\n")
      (else
       "Usage: grove COMMAND [OPTIONS]\n\nCommands:\n  build       Build the project and update its outputs.\n  check       Validate the complete pipeline without writing outputs.\n  serve       Preview on localhost and rebuild when inputs change.\n  browse      Analyze the project and browse its model in the terminal.\n  ssh-keygen  Generate an RSA host key for --ssh.\n  new         Create a tree with an increasing base36 ID.\n")))
  (when (or (not command) (member command '("build" "check" "serve" "browse" "new")))
    (display "\nOptions:\n  --project PATH  Project file (default: ./project.scm).\n"))
  (when (or (not command) (member command '("build" "serve")))
    (display "  --output PATH   Override the output directory (build and serve).\n"))
  (when (or (not command) (equal? command "serve"))
    (display "  --port NUMBER   Listen on this port (serve only; default: 8080).\n  --no-watch      Serve without automatic rebuilding (serve only).\n"))
  (when (or (not command) (equal? command "new"))
    (display "  --dest PATH     Existing destination directory (new only).\n"))
  (when (or (not command) (equal? command "browse"))
    (display "  --ssh PORT      Serve browsing over SSH (browse only).\n  --host-key PATH RSA host key for --ssh (browse only).\n  --authorized-keys PATH  Authorized client keys for --ssh (browse only).\n  --ssh-workers N         Concurrent sessions/processes (browse --ssh; default 4).\n  --ssh-login-timeout S   Drop clients that do not authenticate in S seconds (default 60).\n  --ssh-session-timeout S Drop a session after S seconds (0 disables; default 0).\n  --ssh-memory-limit BYTES Cap each worker's address space (browse --ssh).\n"))
  (when (or (not command) (equal? command "ssh-keygen"))
    (display "  --host-key PATH RSA host key to create (ssh-keygen).\n  --force         Overwrite an existing host key (ssh-keygen).\n"))
  (display "  --debug         Show the original exception and backtrace.\n  -h, --help      Show help.\n\nOption paths are relative to the invocation directory.\nProject paths are relative to the project file's directory.\n"))

(define (run-command command options)
  (let ((filename (or (assq-ref options 'project) "project.scm")))
    (cond
      ((string=? command "ssh-keygen")
       (let* ((invocation (getcwd))
              (path (assq-ref options 'host-key))
              (absolute (and path
                             (if (absolute-file-name? path) path
                                 (string-append invocation "/" path)))))
         (unless path
           (error "--host-key is required"))
         ((ssh-ref 'generate-host-key) absolute #:force (assq-ref options 'force))
         0))
      ((string=? command "new")
       (let* ((destination (assq-ref options 'dest))
              (destination (and destination
                                (if (absolute-file-name? destination) destination
                                    (string-append (getcwd) "/" destination)))))
         (call-with-loaded-project filename
           (lambda (project)
             (display (create-tree project
                        #:prefix (or (assq-ref options 'prefix)
                                     (project-default-prefix project))
                        #:destination (or destination
                                          (project-source-directory project))))
             (newline)))
         0))
      ((string=? command "serve")
        (serve-project filename
          #:output-directory (assq-ref options 'output)
          #:port (or (assq-ref options 'port) 8080)
          #:watch? (not (assq-ref options 'no-watch))
          #:debug? (assq-ref options 'debug)))
      ((string=? command "browse")
       (let* ((invocation (getcwd))
              (absolute (lambda (path)
                          (if (absolute-file-name? path) path
                              (string-append invocation "/" path))))
              (ssh-port (assq-ref options 'ssh)))
         (call-with-loaded-project filename
           (lambda (project)
             (call-with-project-stage 'browse #f
               (lambda ()
                 (unless (project-browse project)
                   (error "project does not define a #:browse callback"))
                 (if ssh-port
                     (let ((host-key (assq-ref options 'host-key))
                           (authorized (assq-ref options 'authorized-keys)))
                       (unless host-key
                         (error "--ssh requires --host-key"))
                       (unless authorized
                         (error "--ssh requires --authorized-keys"))
                       (format (current-error-port)
                               "grove: serving SSH browsing on port ~a~%" ssh-port)
                       ;; Analyze once; worker processes inherit the model.
                       (let ((analysis (analyze-project project)))
                         ((ssh-ref 'serve-ssh)
                           #:port ssh-port
                           #:host-key (absolute host-key)
                           #:authorized-keys
                           ((ssh-ref 'read-authorized-keys) (absolute authorized))
                           #:workers (or (assq-ref options 'ssh-workers) 4)
                           #:login-timeout (or (assq-ref options 'ssh-login-timeout) 60)
                           #:session-timeout (or (assq-ref options 'ssh-session-timeout) 0)
                           #:memory-limit (assq-ref options 'ssh-memory-limit)
                           #:handler
                           (lambda (input output)
                             ((project-browse project)
                              (project-analysis-model analysis)
                              #:input input #:output output)))
                         0))
                     (begin (browse-project project) 0))))))))
      (else
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
          0)))))

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
