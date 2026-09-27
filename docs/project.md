<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove projects

`(grove project)` runs the common project pipeline:

1. Collect optional module sources and pass them to the configured module loader.
2. Collect page sources and read each one into a Grove site.
3. Pass the site to an optional library analyzer.
4. Run each renderer over the analyzed value, combine the artifacts, and write
   them to the configured output directory.

The project API does not choose a document language or a site model. The
reader, analyzer, and renderers are procedures supplied by the project. For
example, a linked-notes project can use Forester as its analyzer and renderer
library:

```scheme
(use-modules (forester input scribble)
             (forester render html)
             (forester render text)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove render html routes)
             (grove style html))

(define read-document (make-scribble-document-reader))
(define source-directory "source")
(define style (make-html-style))
(define routes (make-html-routes))

(define project
  (make-project
    #:title "Linked Notes"
    #:source-directory source-directory
    #:module-directory "source/modules"
    #:load-modules load-scribble-modules!
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer #:style style #:routes routes)
          (lambda (forest)
            (prefix-artifacts "text" (render-forest-text forest))))
    #:output-directory "result"))

(build-project project)
```

The default collector selects `.grv` files directly inside each configured
directory. Supply `#:source-filter` or `#:module-filter` to change that
selection. Module sources are loaded before page sources; `.grv` module files
declare their module with Guile's ordinary `define-module`. Page reading,
analysis, and rendering remain independent phases. With no analyzer, renderers
receive the Grove site directly.

The complete runnable example, including custom routes and CSS, is in
`examples/linked-notes/build.scm`.

Forester's HTML renderer uses MathJax 4's combined TeX-to-CommonHTML component
on pages that contain math. It loads the component with `defer`; projects can
pass `#:mathjax-src` to `make-forester-html-renderer` to use a locally hosted
copy or another compatible MathJax build, and can pass `#f` to disable it.
