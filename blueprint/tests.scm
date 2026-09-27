;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint tests)
  #:use-module (blue build)
  #:use-module (blue computation)
  #:use-module (blue file-system directory)
  #:use-module (blue file-system path)
  #:use-module (blue states)
  #:use-module (blue stencils guile)
  #:use-module (blue store)
  #:use-module (blue subprocess)
  #:use-module (blue types buildable)
  #:use-module (blue types command)
  #:use-module (blue types testable)
  #:use-module (blue utils environ)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (grv-tests))

(define +test-resources+
  '("unit/scribble/read.scm"
    "unit/scribble/parse.scm"
    "unit/grove/build.scm"
    "unit/grove/state.scm"
    "unit/grove/content.scm"
    "unit/grove/output.scm"
    "unit/grove/project.scm"
    "unit/grove/platform.scm"
    "unit/forester/project.scm"
    "unit/forester/site.scm"
    "unit/forester/graph.scm"))

(define test-environment
  #%~(list (format #f "GRV_SRCDIR=~a" #%?srcdir)))

(define test-options
  #%~(list
       #:load-path (list #%?srcdir)
       #:load-compiled-path (list #%?builddir)
       #:enable-coverage? (->bool (getenv "GRV_TESTS_ENABLE_COVERAGE"))
       #:time-limit (or (and=> (getenv "GRV_TESTS_TIME_LIMIT") string->number)
                        20)))

(define grv-tests
  (map
    (λ (script-name)
      (guile-test-suite
        (inputs #%~(string-append #%?testdir "/" script-name))
        (environment test-environment)
        (options test-options)))
    +test-resources+))
