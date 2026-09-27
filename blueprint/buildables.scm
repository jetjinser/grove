;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blue stencils template-file)
  #:use-module (blue stencils texinfo)
  #:export (grv-modules forester-modules))

(define +grv-sources+
  '("modules/grove/build.scm"
    "modules/grove/collect.scm"
    "modules/grove/ir.scm"
    "modules/grove/output.scm"
    "modules/grove/project.scm"
    "modules/grove/site.scm"
    "modules/grove/input/scribble.scm"
    "modules/grove/render/html.scm"
    "modules/grove/render/html/routes.scm"
    "modules/grove/render/text.scm"
    "modules/grove/style/html.scm"
    "modules/language/scribble/commands.scm"
    "modules/language/scribble/parser.scm"
    "modules/language/scribble/spec.scm"))

(define +forester-sources+
  '("modules/forester/commands.scm"
    "modules/forester/site.scm"
    "modules/forester/input/scribble.scm"
    "modules/forester/render/html.scm"
    "modules/forester/render/text.scm"))

(define grv-modules
  (map
    (λ (source)
      (guile-module
        (inputs source)
        (outputs (->.go source))
        (load-path #%~(list #%?srcdir))
        (optimizations
          '(#:seal-private-binddings? #t))))
    +grv-sources+))

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
