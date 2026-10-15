;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove subprocess)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module ((system vm coverage) #:prefix vm:)
  #:use-module (system vm vm)
  #:use-module (system base compile)
  #:use-module (blue debugging coverage)
  #:export (cli-entry prepare-subprocess test-subprocess-coverage))

(define (prepare-subprocess directory)
  (setenv "GROVE_TEST_COVERAGE_RUN" (number->string (getpid)))
  ;;     The compiled helper lives under the cache directory, outside
  ;;     the watched project tree: a compiled-path root at the project
  ;;     directory turns every lazily auto-compiled module load into a
  ;;     project edit for the serve watcher.
  (let* ((cache (string-append directory "/.cache"))
         (path cache))
    (unless (file-exists? cache) (mkdir cache))
    (for-each (lambda (name)
                (set! path (string-append path "/" name))
                (mkdir path)) '("tests" "unit" "grove"))
    (compile-file
      (string-append (dirname (getenv "GROVE_SRCDIR"))
                     "/tests/unit/grove/subprocess.scm")
      #:output-file (string-append path "/subprocess.go"))))

(define (coverage-enabled?)
  (equal? (getenv "GROVE_TESTS_ENABLE_COVERAGE") "1"))

(define (covered-call thunk)
  (if (not (coverage-enabled?)) (thunk)
      (let ((engine (vm-engine)))
        (dynamic-wind
          (lambda () (set-vm-engine! 'debug))
          (lambda ()
            (call-with-values (lambda () (vm:with-code-coverage thunk))
              (lambda (data . results)
                (call-with-output-file
                  (string-append (dirname (getenv "GROVE_SRCDIR"))
                                 "/build/coverage-child-"
                                 (getenv "GROVE_TEST_COVERAGE_RUN") "-"
                                 (number->string (getpid)) ".cov")
                  (lambda (port)
                    (coverage-data->lcov data port
                      #:match-modules?
                      (lambda (path) (string-prefix? "grove/" path)))))
                (apply values results))))
          (lambda () (set-vm-engine! engine))))))

(define (test-subprocess-coverage sources)
  (when (coverage-enabled?)
    (let* ((directory (string-append (dirname (getenv "GROVE_SRCDIR")) "/build"))
           (prefix (string-append "coverage-child-"
                                  (getenv "GROVE_TEST_COVERAGE_RUN") "-"))
           (reports
             (map (lambda (name)
                    (call-with-input-file (string-append directory "/" name)
                      get-string-all))
                  (scandir directory (lambda (name) (string-prefix? prefix name))))))
      (for-each
        (lambda (source)
          (test-assert (string-append "child coverage records " source)
            (any (lambda (report) (string-contains report source)) reports)))
        sources))))

(define (covered-expression code)
  (let ((forms (call-with-input-string code
                 (lambda (port)
                   (let loop ((forms '()))
                     (let ((form (read port)))
                       (if (eof-object? form) (reverse forms)
                           (loop (cons form forms)))))))))
    (call-with-output-string
      (lambda (port)
        (write `(begin
                  (use-modules (tests unit grove subprocess))
                  ,@(map (lambda (form)
                           (match form
                             (('exit expression)
                              `(exit ((@@ (tests unit grove subprocess) covered-call)
                                       (lambda () ,expression))))
                             (_ form))) forms)) port)))))

(define (cover-workers!)
  ;; Adapt the actual Guile launch arguments in the test process. Compiled
  ;; private entry constants cannot be changed through module-set!.
  (let* ((binding (module-variable (resolve-module '(grove serve)) 'spawn))
         (original (variable-ref binding)))
    (variable-set! binding
      (lambda (program arguments . options)
        (apply original program
          (let rewrite ((remaining arguments))
            (match remaining
              (("-c" code . rest)
               (if (string-contains code "(grove private preview worker)")
                   (cons "-c" (cons (covered-expression code) rest))
                   remaining))
              ((head . rest) (cons head (rewrite rest)))
              (() '())))
          options)))))

(define (cli-entry arguments)
  (resolve-interface '(grove cli))
  (when (coverage-enabled?) (cover-workers!))
  (covered-call
    (lambda ()
      ((module-ref (resolve-module '(grove cli)) 'main) arguments))))
