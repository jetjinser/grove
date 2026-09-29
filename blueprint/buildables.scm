;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blue stencils template-file)
  #:use-module (blue stencils texinfo)
  #:export (grove-modules forester-modules))

(define +grove-sources+
  '("grove/build.scm"
    "grove/build/state.scm"
    "grove/collect.scm"
    "grove/ir.scm"
    "grove/relation.scm"
    "grove/output.scm"
    "grove/project.scm"
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
    "language/scribble/commands.scm"
    "language/scribble/parser.scm"
    "language/scribble/spec.scm"))

(define +forester-sources+
  '("forester/commands.scm"
    "forester/composition.scm"
    "forester/private/date.scm"
    "forester/private/web.scm"
    "forester/site.scm"
    "forester/style/html.scm"
    "forester/input/scribble.scm"
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
  (map
    (λ (source)
      (guile-module
        (inputs source)
        (outputs (->.go source))
        (load-path #%~(list #%?srcdir))
        (optimizations
          '(#:seal-private-binddings? #t))))
    +forester-sources+))
