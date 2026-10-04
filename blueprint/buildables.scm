;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blue stencils template-file)
  #:use-module (blue stencils texinfo)
  #:export (grove-modules forester-modules grove-programs))

(define +grove-sources+
  '("grove/build.scm"
    "grove/build/state.scm"
    "grove/collect.scm"
    "grove/cli.scm"
    "grove/ir.scm"
    "grove/string-fun.scm"
    "grove/style.scm"
    "grove/math/audit.scm"
    "grove/math/syntax.scm"
    "grove/math/term.scm"
    "grove/math/operator.scm"
    "grove/math/assumption.scm"
    "grove/math/declaration.scm"
    "grove/math/domain.scm"
    "grove/math/context.scm"
    "grove/math/types.scm"
    "grove/math/rule-spec.scm"
    "grove/math/rule.scm"
    "grove/math/catalog.scm"
    "grove/math/identities.scm"
    "grove/math/vector.scm"
    "grove/math/differentiation.scm"
    "grove/math/partial.scm"
    "grove/math/function-derivative.scm"
    "grove/math/components.scm"
    "grove/math/spatial.scm"
    "grove/math/polynomial.scm"
    "grove/math/polynomial/contract.scm"
    "grove/math/exact-polynomial.scm"
    "grove/math/integration.scm"
    "grove/math/arithmetic.scm"
    "grove/math/number.scm"
    "grove/math/normalization.scm"
    "grove/math/scalar.scm"
    "grove/math/premise.scm"
    "grove/math/derivation.scm"
    "grove/math/proof.scm"
    "grove/math/diagnostic.scm"
    "grove/math/report.scm"
    "grove/math/source.scm"
    "grove/math/provenance.scm"
    "grove/math/expression.scm"
    "grove/math/semantic.scm"
    "grove/math/formula.scm"
    "grove/math/formula/reader.scm"
    "grove/math/notation.scm"
    "grove/math/notation/core.scm"
    "grove/math/presentation.scm"
    "grove/math/pass.scm"
    "grove/math/strategy.scm"
    "grove/math/verification.scm"
    "grove/math/semantic-check.scm"
    "grove/math/layout.scm"
    "grove/math/rewrite.scm"
    "grove/math/pipeline.scm"
    "grove/relation.scm"
    "grove/output.scm"
    "grove/project.scm"
    "grove/project/diagnostic.scm"
    "grove/project/load.scm"
    "grove/project/site.scm"
    "grove/private/minikanren.scm"
    "grove/relation/dsl.scm"
    "grove/site.scm"
    "grove/input/scribble.scm"
    "grove/render/html.scm"
    "grove/render/html/routes.scm"
    "grove/render/static.scm"
    "grove/render/text.scm"
    "grove/style/html.scm"
    "language/scribble/command.scm"
    "language/scribble/flow.scm"
    "language/scribble/commands.scm"
    "language/scribble/parser.scm"
    "language/scribble/spec.scm"))

(define +forester-sources+
  '("forester/backmatter.scm"
    "forester/commands.scm"
    "forester/composition.scm"
    "forester/private/date.scm"
    "forester/private/web.scm"
    "forester/site.scm"
    "forester/style/html.scm"
    "forester/input/scribble.scm"
    "forester/render/artifact.scm"
    "forester/render/html.scm"
    "forester/render/graph.scm"
    "forester/render/rss.scm"
    "forester/render/sitemap.scm"
    "forester/render/text.scm"
    "forester/render/text/routes.scm"
    "forester/relations.scm"))

(define grove-modules
  (map
    (λ (source)
      (guile-module
        (inputs source)
        (outputs (->.go source))
        (load-path #%~(list #%?srcdir))
        (optimizations
          '(#:seal-private-binddings? #t))))
    +grove-sources+))

(define forester-modules
  (append
    (map
      (λ (source)
        (guile-module
          (inputs source)
          (outputs (->.go source))
          (load-path #%~(list #%?srcdir))
          (optimizations
            '(#:seal-private-binddings? #t))))
      +forester-sources+)
    (list
      (template-file
        (inputs "forester/data/lipsum-la.txt")
        (outputs "forester/data/lipsum-la.txt")
        (install-location #%~(string-append #%?guile-module-dir "/forester/data")))
      (template-file
        (inputs "forester/style/default.css")
        (outputs "forester/style/default.css")
        (install-location #%~(string-append #%?guile-module-dir "/forester/style"))))))

(define grove-programs
  (list
    (template-file
      (inputs "../bin/grove.in")
      (outputs "bin/grove")
      (mode #o755)
      (install-permissions #o755)
      (local-variables
        #%~(list (cons 'GUILE_MODULE_DIR #%?guile-module-dir)
                 (cons 'GUILE_OBJECT_DIR #%?guile-object-dir)))
      (install-location #%~#%?bindir))))
