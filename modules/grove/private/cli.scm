;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private cli)
  #:use-module (grove private project model)
  #:use-module (grove private project lifecycle)
  #:use-module (grove serve)
  #:use-module (grove private cache-dir)
  #:use-module (grove private store-gc)
  #:use-module (grove private cli progress)
  #:use-module (grove private project timing)
  #:use-module (grove project load)
  #:use-module (grove project diagnostic)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (main))

;; Reported by `grove --version'. The build system may substitute a real value
;; later; the source tree reports "dev".
(define grove-version "dev")

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

;;; Options are described once and drive parsing, validation, and help.

(define command-synopses
  '(("build" . "Usage: grove build [OPTIONS]\n\nBuild the project and update its outputs.\n")
    ("check" . "Usage: grove check [OPTIONS]\n\nRead, analyze, render, and validate without writing outputs.\n")
    ("serve" . "Usage: grove serve [OPTIONS]\n\nServe successful build outputs on localhost and watch for changes.\n")
    ("browse" . "Usage: grove browse [OPTIONS]\n\nRead and analyze the project, then browse its model in the terminal.\n")
    ("ssh-keygen" . "Usage: grove ssh-keygen --host-key PATH [OPTIONS]\n\nGenerate an RSA host key for 'grove browse --ssh'.\n")
    ("new" . "Usage: grove new [PREFIX] [OPTIONS]\n\nAsk the project to create a source, optionally using PREFIX.\n")
    ("store" . "Usage: grove store (size|gc) [OPTIONS]\n\nReport the size of the per-user shared store, or prune stale entries.\n")))

(define command-names (map car command-synopses))

(define (command-summary synopsis)
  (let* ((usage-end (string-index synopsis #\newline))
         (start (+ usage-end 2))
         (end (string-index synopsis #\newline start)))
    (substring synopsis start end)))

(define (validate-store-action action)
  (unless (member action '("size" "gc"))
    (usage-error "store requires an action: size or gc"))
  action)

(define* (option-spec flag #:key (key (string->symbol flag)) (arity 'flag)
                      (parse 'string) check (commands #f) (actions #f)
                      help helps)
  "Describe one option as an association list.

FLAG is the literal token (including dashes). ARITY is `flag' or `value'.
PARSE is `string' or `integer'. CHECK validates a parsed value. COMMANDS lists
the commands the option is accepted by, or #f for all. ACTIONS narrows an
option to subcommands of COMMANDS. HELP is the help line, with HELP overrides
keyed by command."
  (list (cons 'flag flag) (cons 'key key) (cons 'arity arity)
        (cons 'parse parse) (cons 'check check) (cons 'commands commands)
        (cons 'actions actions)
        (cons 'help help) (cons 'helps helps)))

(define (option-field spec field) (assq-ref spec field))

(define (option-by-flag flag)
  (find (lambda (spec) (string=? flag (option-field spec 'flag))) option-specs))

(define (option-by-key key)
  (find (lambda (spec) (eq? key (option-field spec 'key))) option-specs))

(define (port-check value flag)
  (unless (<= 1 value 65535)
    (usage-error "~a requires an integer from 1 to 65535" flag))
  value)

(define (workers-check value flag)
  (unless (<= 1 value 64)
    (usage-error "--ssh-workers must be from 1 to 64"))
  value)

(define (seconds-check value flag)
  (unless (>= value 0)
    (usage-error "~a must be a non-negative number of seconds" flag))
  value)

(define (bytes-check value flag)
  (unless (> value 0)
    (usage-error "--ssh-memory-limit must be a positive number of bytes"))
  value)

(define option-specs
  (list
    (option-spec "--project" #:key 'project #:arity 'value
                 #:commands '(build check serve new browse)
                 #:help "  --project PATH  Project file (default: the nearest project.scm upward).\n")
    (option-spec "--output" #:key 'output #:arity 'value
                 #:commands '(build serve)
                 #:help "  --output PATH   Override the output directory (build and serve).\n")
    (option-spec "--profile" #:key 'profile #:commands '(build check serve)
                 #:help "  --profile       Report input-cache timings and peak RSS.\n")
    (option-spec "--port" #:key 'port #:arity 'value #:parse 'integer
                 #:check port-check #:commands '(serve)
                 #:help "  --port NUMBER   Listen on this port (serve only; default: 8080).\n")
    (option-spec "--no-watch" #:key 'no-watch #:commands '(serve)
                 #:help "  --no-watch      Serve without automatic rebuilding (serve only).\n")
    (option-spec "--dest" #:key 'dest #:arity 'value #:commands '(new)
                 #:help "  --dest PATH     Existing destination directory (new only).\n")
    (option-spec "--ssh" #:key 'ssh #:arity 'value #:parse 'integer
                 #:check port-check #:commands '(browse)
                 #:help "  --ssh PORT      Serve browsing over SSH (browse only).\n")
    (option-spec "--host-key" #:key 'host-key #:arity 'value
                 #:commands '(browse ssh-keygen)
                 #:help "  --host-key PATH RSA host key for --ssh (browse only).\n"
                 #:helps '((ssh-keygen
                            . "  --host-key PATH RSA host key to create.\n")))
    (option-spec "--authorized-keys" #:key 'authorized-keys #:arity 'value
                 #:commands '(browse)
                 #:help "  --authorized-keys PATH  Authorized client keys for --ssh (browse only).\n")
    (option-spec "--ssh-generate-host-key" #:key 'ssh-generate-host-key
                 #:commands '(browse)
                 #:help "  --ssh-generate-host-key Create the host key if it is missing (browse --ssh).\n")
    (option-spec "--ssh-workers" #:key 'ssh-workers #:arity 'value
                 #:parse 'integer #:check workers-check #:commands '(browse)
                 #:help "  --ssh-workers N         Concurrent sessions/processes (browse --ssh; default 4).\n")
    (option-spec "--ssh-login-timeout" #:key 'ssh-login-timeout #:arity 'value
                 #:parse 'integer #:check seconds-check #:commands '(browse)
                 #:help "  --ssh-login-timeout S   Drop clients that do not authenticate in S seconds (default 60).\n")
    (option-spec "--ssh-session-timeout" #:key 'ssh-session-timeout #:arity 'value
                 #:parse 'integer #:check seconds-check #:commands '(browse)
                 #:help "  --ssh-session-timeout S Drop a session after S seconds (0 disables; default 0).\n")
    (option-spec "--ssh-memory-limit" #:key 'ssh-memory-limit #:arity 'value
                 #:parse 'integer #:check bytes-check #:commands '(browse)
                 #:help "  --ssh-memory-limit BYTES Cap each worker's address space (browse --ssh).\n")
    (option-spec "--force" #:key 'force #:commands '(ssh-keygen)
                 #:help "  --force         Overwrite an existing host key.\n")
    (option-spec "--dry-run" #:key 'dry-run #:commands '(store)
                 #:actions '(gc)
                 #:help "  --dry-run       Report what store gc would remove, deleting nothing.\n")
    (option-spec "--older-than" #:key 'older-than #:arity 'value
                 #:parse 'integer #:check seconds-check #:commands '(store)
                 #:actions '(gc)
                 #:help "  --older-than N  store gc prunes entries older than N days (default 30).\n")
    (option-spec "--debug" #:key 'debug)
    (option-spec "--help" #:key 'help)
    (option-spec "-h" #:key 'help)
    (option-spec "--version" #:key 'version)))

(define (coerce-option spec value flag)
  "Parse and validate VALUE for SPEC, returning the stored value."
  (case (option-field spec 'parse)
    ((integer)
     (let ((number (string->number value)))
       (unless (exact-integer? number)
         (usage-error "~a requires an integer" flag))
       (let ((check (option-field spec 'check)))
         (if check (check number flag) number))))
    (else value)))

(define (command-in? command commands)
  (and commands command (member command (map symbol->string commands))))

(define (validate-options command options)
  "Reject options used with a command they do not belong to, plus cross rules."
  (for-each
    (lambda (entry)
      (let ((spec (option-by-key (car entry))))
        (when spec
          (let ((commands (option-field spec 'commands)))
            (when (and commands command (not (command-in? command commands)))
              (usage-error "~a is a ~a option"
                           (option-field spec 'flag)
                           (string-join (map symbol->string commands) " and "))))
          (let* ((actions (option-field spec 'actions))
                 (action (assq-ref options 'store-action)))
            (when (and actions
                       (not (member action (map symbol->string actions))))
              (usage-error "~a requires 'store ~a'"
                           (option-field spec 'flag)
                           (string-join (map symbol->string actions) " or ")))))))
    options)
  (when (and (assq 'ssh-generate-host-key options) (not (assq 'ssh options)))
    (usage-error "--ssh-generate-host-key requires --ssh")))

(define (parse-options arguments)
  (let loop ((arguments arguments) (command #f) (options '()))
    (match arguments
      (()
       (unless (or command (assq 'help options) (assq 'version options))
         (usage-error "expected a command; use 'grove --help'"))
       (validate-options command options)
       (cons command options))
      ((argument . rest)
       (cond
         ((and (not command) (member argument command-names))
          (loop rest argument options))
         ((option-by-flag argument)
          => (lambda (spec)
               (let ((key (option-field spec 'key)))
                 (if (eq? (option-field spec 'arity) 'flag)
                     (loop rest command
                           (if (assq key options) options
                               (acons key #t options)))
                     (begin
                       (when (assq key options)
                         (usage-error "duplicate option: ~a" argument))
                       (match rest
                         (((? (lambda (value)
                                (and (not (string-null? value))
                                     (not (string-prefix? "-" value)))) value)
                           . tail)
                          (loop tail command
                                (acons key (coerce-option spec value argument)
                                       options)))
                         (_ (usage-error "~a requires a value" argument))))))))
         ((and (string-prefix? "--" argument) (string-index argument #\=))
          (let ((index (string-index argument #\=)))
            (loop (cons (substring argument 0 index)
                        (cons (substring argument (+ index 1)) rest))
                  command options)))
         ((and (equal? command "new") (not (string-prefix? "-" argument)))
          (when (assq 'prefix options) (usage-error "new accepts one prefix"))
          (loop rest command (acons 'prefix argument options)))
         ((and (equal? command "store") (not (string-prefix? "-" argument)))
          (when (assq 'store-action options)
            (usage-error "store accepts one action"))
          (loop rest command
                (acons 'store-action (validate-store-action argument) options)))
         (else (usage-error "unknown command or option: ~a" argument)))))))

(define (general-synopsis)
  (call-with-output-string
    (lambda (port)
      (display "Usage: grove COMMAND [OPTIONS]\n\nCommands:\n" port)
      (for-each
        (lambda (entry)
          (format port "  ~a  ~a\n"
                  (car entry) (command-summary (cdr entry))))
        command-synopses))))

(define (print-help command action)
  (display (or (and command (assq-ref command-synopses command))
               (general-synopsis)))
  (let ((lines
         (filter-map
           (lambda (spec)
             (let ((commands (option-field spec 'commands)))
               (let ((actions (option-field spec 'actions)))
                 (if (and (or (not commands) (not command)
                              (member command (map symbol->string commands)))
                          (or (not actions) (not action)
                              (member action (map symbol->string actions))))
                   (or (and command (assq-ref (or (option-field spec 'helps) '())
                                              command))
                       (option-field spec 'help))
                   #f))))
           option-specs)))
    (when (pair? lines)
      (display "\nOptions:\n")
      (for-each display lines)))
  (display "  --debug         Show the original exception and backtrace.\n  --version       Show version information.\n  -h, --help      Show help.\n\nOption paths are relative to the invocation directory.\nProject paths are relative to the project file's directory.\n"))

(define (discover-project-filename)
  ;;     Git-style upward search from the invocation directory.  A
  ;;     repository boundary (.git) or the filesystem root stops the
  ;;     walk; falling back to the bare relative name keeps the
  ;;     loader's "no project file at ..." direction intact for a
  ;;     directory that holds neither.
  (let loop ((directory (getcwd)))
    (let ((candidate (string-append directory "/project.scm")))
      (if (file-exists? candidate)
          candidate
          (let ((parent (dirname directory)))
            (if (or (file-exists? (string-append directory "/.git"))
                    (string=? parent directory))
                "project.scm"
                (loop parent)))))))

(define (run-command command options)
  (let ((filename (or (assq-ref options 'project)
                      (discover-project-filename))))
    (cond
      ((string=? command "store")
       (let* ((action (validate-store-action
                       (or (assq-ref options 'store-action) "")))
              (root (grove-store-directory))
              (dry-run (assq-ref options 'dry-run))
              (days (or (assq-ref options 'older-than) 30))
              (report (lambda (count bytes)
                        (format #t "~a ~a entries (~a)\n"
                                (if dry-run "Would remove" "Removed")
                                count (format-bytes bytes)))))
         (cond ((string=? action "size")
                (format #t "Store ~a\n" root)
                (for-each
                  (lambda (entry)
                    (format #t "  ~a ~a entries ~a\n"
                            (car entry) (cadr entry)
                            (format-bytes (caddr entry))))
                  (store-summary root)))
               (else
                (call-with-values
                    (lambda () (store-prune root days dry-run))
                  report)
                0))))
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
             (display (create-project-source project
                        #:prefix (assq-ref options 'prefix)
                        #:destination (or destination
                                          (project-source-directory project))))
             (newline)))
         0))
      ((string=? command "serve")
        (serve-project filename
          #:output-directory (assq-ref options 'output)
          #:port (or (assq-ref options 'port) 8080)
          #:watch? (not (assq-ref options 'no-watch))
          #:debug? (assq-ref options 'debug)
          #:profile? (assq-ref options 'profile)))
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
                        (let ((host-key-path (absolute host-key)))
                          (when (and (assq-ref options 'ssh-generate-host-key)
                                     (not (file-exists? host-key-path)))
                            (format (current-error-port)
                                    "grove: generating SSH host key ~a~%"
                                    host-key-path)
                            ((ssh-ref 'generate-host-key) host-key-path))
                          (format (current-error-port)
                                  "grove: serving SSH browsing on port ~a~%"
                                  ssh-port)
                          ;; Analyze once; worker processes inherit the model.
                          (let ((model (analyze-project project)))
                            ((ssh-ref 'serve-ssh)
                            #:port ssh-port
                            #:host-key host-key-path
                           #:authorized-keys
                           ((ssh-ref 'read-authorized-keys) (absolute authorized))
                           #:workers (or (assq-ref options 'ssh-workers) 4)
                           #:login-timeout (or (assq-ref options 'ssh-login-timeout) 60)
                           #:session-timeout (or (assq-ref options 'ssh-session-timeout) 0)
                           #:memory-limit (assq-ref options 'ssh-memory-limit)
                           #:handler
                           (lambda (input output)
                             ((project-browse project)
                              model
                               #:input input #:output output)))
                          0)))
                     (begin (browse-project project) 0))))))))
      (else
        (let* ((started (cli-seconds))
               (checking? (string=? command "check"))
               (timings (and (assq-ref options 'profile) (make-hash-table)))
               (result
                 (call-with-cli-progress (if checking? "Checking" "Building")
                   filename
                   (lambda (progress)
                     (parameterize ((current-analysis-timings
                                      (lambda (phase seconds)
                                        (progress 'analysis-phase
                                                  (cons phase seconds)))))
                       (parameterize ((current-analysis-phase-start
                                        (lambda (phase)
                                          (progress 'analysis-phase-start
                                                    phase))))
                         (parameterize ((current-input-cache-timings timings))
                           (call-with-loaded-project filename
                             (lambda (project)
                               (let ((report
                                       (if checking?
                                           (check-project project #:progress progress)
                                           (build-project/report project #:progress progress))))
                                 (cons (and (not checking?)
                                            (canonicalize-path (project-output-directory project)))
                                       report)))
                             #:output-directory (assq-ref options 'output))))))))
               (output (car result))
               (report (cdr result)))
          (print-cli-summary report (- (cli-seconds) started)
            #:checked? checking? #:output output)
          (when timings
            (report-profile (current-error-port) timings))
          0)))))

(define (main arguments)
  "Run Grove with ARGUMENTS (excluding the program name), returning an exit code."
  (catch 'grove-usage-error
    (lambda ()
      (let* ((parsed (parse-options arguments))
             (command (car parsed))
             (options (cdr parsed)))
        (cond
          ((assq-ref options 'version)
           (version-etc "grove" grove-version #:command-name "grove")
           0)
          ((assq-ref options 'help)
           (print-help command (assq-ref options 'store-action))
           0)
          (else
           (catch 'grove-project-error
             (lambda ()
               (call-with-project-stage 'execute #f
                 (lambda () (run-command command options))))
             (lambda (_ error)
               (print-project-error error (assq-ref options 'debug))
               1))))))
    (lambda (_ message)
      (format (current-error-port) "grove: ~a~%" message)
      2)))
