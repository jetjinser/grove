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
  #:export (grove-tests))

(define +test-resources+
  '("unit/scribble/read.scm"
    "unit/grove/math.scm"
    "unit/grove/differentiation.scm"
    "unit/grove/arithmetic.scm"
    "unit/grove/math-pass.scm"
    "unit/grove/math-rule.scm"
    "unit/grove/math-context.scm"
    "unit/grove/math-contracts.scm"
    "unit/grove/math-semantic-check.scm"
    "unit/grove/math-proof.scm"
    "unit/grove/math-provenance.scm"
    "unit/grove/math-oracle.scm"
    "unit/grove/math-search.scm"
    "unit/grove/math-writing.scm"
    "unit/grove/math-declarations.scm"
    "unit/grove/math-normalization.scm"
    "unit/grove/math-fields.scm"
    "unit/grove/semantic.scm"
    "unit/grove/math-interface.scm"
    "unit/grove/math-presentation.scm"
    "unit/grove/math-catalog.scm"
    "unit/grove/math-coefficients.scm"
    "unit/grove/math-components.scm"
    "unit/grove/math-partial-oracle.scm"
    "unit/grove/math-spatial.scm"
    "unit/grove/math-laplacian.scm"
    "unit/grove/math-component-algebra.scm"
    "unit/grove/math-cartesian-oracle.scm"
    "unit/grove/math-order.scm"
    "unit/grove/math-regularity.scm"
    "unit/grove/math-report.scm"
    "unit/grove/math-functions.scm"
    "unit/grove/math-function-calculus.scm"
    "unit/grove/math-parameters.scm"
    "unit/grove/math-coefficient-reduction.scm"
    "unit/grove/math-integration.scm"
    "unit/grove/math-poisson.scm"
    "unit/grove/math-equations.scm"
    "unit/grove/math-calculus-interface.scm"
    "unit/scribble/parse.scm"
    "unit/grove/build.scm"
    "unit/grove/collect.scm"
    "unit/grove/state.scm"
    "unit/grove/content.scm"
    "unit/grove/string.scm"
    "unit/grove/relation.scm"
    "unit/grove/relation-dsl.scm"
    "unit/grove/output.scm"
    "unit/grove/static.scm"
    "unit/grove/project.scm"
    "unit/grove/platform.scm"
    "unit/grove/test-registry.scm"
    "unit/forester/commands.scm"
    "unit/forester/composition.scm"
    "unit/forester/project.scm"
    "unit/forester/render.scm"
    "unit/forester/queries.scm"
    "unit/forester/relations.scm"
    "unit/forester/site.scm"
    "unit/forester/graph.scm"))

(define +fixture-resources+
  '("unit/grove/cartesian-fixtures.scm"
    "unit/grove/polynomial-oracle.scm"
    "unit/forester/fixtures.scm"))

(define test-environment
  #%~(list (format #f "GROVE_SRCDIR=~a" #%?srcdir)))

(define test-options
  #%~(list
       #:load-path (list #%?srcdir)
       #:load-compiled-path (list #%?builddir)
       #:enable-coverage? (->bool (getenv "GROVE_TESTS_ENABLE_COVERAGE"))
       #:time-limit (or (and=> (getenv "GROVE_TESTS_TIME_LIMIT") string->number)
                        60)))

(define grove-tests
  (map
    (λ (script-name)
      (guile-test-suite
        (inputs #%~(string-append #%?testdir "/" script-name))
        (environment test-environment)
        (options test-options)))
    +test-resources+))
