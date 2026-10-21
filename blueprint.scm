;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;; XXX: dunno how to do this in blueprint itself in a correct way
(add-to-load-path (string-append (dirname (current-filename)) "/modules"))

(use-modules (blue types blueprint)
             (blue types configuration)
             (blue types variable)
             (blue types command)
             (blue subprocess)
             (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (blueprint tests)
             (blueprint install)
             (blueprint buildables)
             (language scribble parser))

(define grove-variables
  (append
    (make-package-variables
        #:name "grove"
        #:version "dev")
    (list
      [variable
        [name "GIT_COMMIT"]
        [value (delay (or (getenv "GIT_COMMIT") "unknown"))]]
      [variable
        [name "testdir"]
        [value (string-append #%?srcdir "/tests")]]
      [variable
        [name "srcdir"]
        [value (string-append #%?srcdir "/modules")]]
      [variable
        [name "builddir"]
        [value (const "build")]])))

(define-command (parse-command args)
  ([invoke   "parse"]
   [category 'develop]
   [synopsis "Parse a Scribble expression into an S-expression"]
   [help     "Read a Scribble expression from the first command-line argument, parse it into an S-expression, and write the result to standard output."])

  (write (scribble-parse (open-input-string (car args))))
  (newline))

(define-command (grove-command arguments)
  ([invoke "grove"]
   [category 'develop]
   [synopsis "Run Grove from this checkout"]
   [help "Run Grove's CLI in an isolated Guile process using this checkout's module paths."]
   [load-configuration-policy 'no])
  (let* ((module-arguments
           (append-map (lambda (path) (list "-L" path)) %load-path))
         (compiled-arguments
           (append-map (lambda (path) (list "-C" path)) %load-compiled-path))
         (program
           (format #f "(use-modules (grove cli)) (exit (main '~s))"
             arguments))
         (status
           (popen "guile"
             (append '("--no-auto-compile")
                     module-arguments
                     compiled-arguments
                     (list "-c" program))
             #:input (current-input-port)
             #:output (current-output-port)
             #:error (current-error-port))))
    (and (status:exit-val status)
         (zero? (status:exit-val status)))))

(define (shell-quote text)
  (string-append
    "'"
    (string-join
      (map (lambda (character)
             (if (char=? character #\')
                 "'\\''"
                 (string character)))
           (string->list text))
      "")
    "'"))

(define (print-file path)
  (when (file-exists? path)
    (call-with-input-file path
      (lambda (port)
        (let loop ((line (read-line port)))
          (unless (eof-object? line)
            (display line (current-output-port))
            (newline (current-output-port))
            (loop (read-line port))))))))

(define (run-command program args)
  (let* ((log (string-append "/tmp/grove-command-" (number->string (getpid)) ".log"))
         (command (string-append (shell-quote program) " "
                                 (string-join (map shell-quote args) " ")
                                 " > " (shell-quote log) " 2>&1"))
         (status (system command)))
    (print-file log)
    (false-if-exception (delete-file log))
    (and status (zero? status))))

(define (run-blueprint-script name args)
  (let* ((root (dirname (current-filename)))
         (script (string-append root "/scripts/" name)))
    (run-command script args)))

(define-command (a11y-command args)
  ([invoke   "a11y"]
   [category 'test]
   [synopsis "Run the browser accessibility audit"]
   [help     "Run axe-core in headless Chromium over the built example pages, or over the HTML paths given as arguments."]
   [load-configuration-policy 'no])
  (run-blueprint-script "a11y-audit.sh" args))

(define-command (check-examples-command _)
  ([invoke   "check-examples"]
   [category 'test]
   [synopsis "Build the audited examples and run the accessibility audit"]
   [help     "Build the five examples covered by the audit, then run axe-core over their index pages."]
   [load-configuration-policy 'no])
  (run-blueprint-script "check-examples.sh" '()))

(define-command (lint-command _)
  ([invoke   "lint"]
   [category 'test]
   [synopsis "Check REUSE license compliance"]
   [help     "Run reuse --no-multiprocessing lint over the tree."]
   [load-configuration-policy 'no])
  (run-command "reuse" '("--no-multiprocessing" "lint")))

(define-command (check-all-command _)
  ([invoke   "check-all"]
   [category 'test]
   [synopsis "Run tests, license lint, example builds, and the accessibility audit"]
   [help     "Run blue check, blue lint, and blue check-examples in order."]
   [load-configuration-policy 'no])
  (and (run-command "blue" '("check"))
       (run-command "blue" '("lint"))
       (run-command "blue" '("check-examples"))))

(define grove-configuration
  (configuration (variables grove-variables)))

(blueprint
  (configuration grove-configuration)
  (buildables
    (append grove-modules forester-modules grove-programs))
  (commands
    (list install-command
          grove-command
          parse-command
          a11y-command
          check-examples-command
          lint-command
          check-all-command))
  (testables grove-tests))
