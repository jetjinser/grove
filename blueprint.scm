;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;; XXX: dunno how to do this in blueprint itself in a correct way
(add-to-load-path (string-append (dirname (current-filename)) "/modules"))

(use-modules (blue types blueprint)
             (blue types configuration)
             (blue types variable)
             (blue types command)
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

(define grove-configuration
  (configuration (variables grove-variables)))

(blueprint
  (configuration grove-configuration)
  (buildables
    (append grv-modules forester-modules))
  (commands
    (list install-command
          parse-command))
  (testables grv-tests))
