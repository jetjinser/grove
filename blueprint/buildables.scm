;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blue stencils template-file)
  #:use-module (blue stencils texinfo)
  #:export (grv-modules))

(define +grv-sources+
  '("modules/grove/ir.scm"
    "modules/grove/site.scm"
    "modules/grove/input/scribble.scm"
    "modules/grove/render/html.scm"
    "modules/grove/render/text.scm"
    "modules/language/scribble/commands.scm"
    "modules/language/scribble/parser.scm"
    "modules/language/scribble/spec.scm"))

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
