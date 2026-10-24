<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Hackable math output

This example is a user-side extension. Grove does not depend on Typst, LaTeX,
Racket, or SMathML. The extension defines one content constructor, one cached
renderer, and one HTML rule; a project supplies the compiler procedure.

```scheme
(use-modules (examples hackable-math extension))

(define math-renderer
  (make-math-asset-renderer
    #:compiler compile-to-svg
    #:compiler-id "typst-0.13; svg; font-profile-3"
    #:dependencies (list "math/macros.typ" "fonts/site-fonts.toml")))

(define formula
  (make-compiled-math "integral x^2 dx" "math/area.svg"
                      #:alt "The integral of x squared"))

(define html-options
  (make-html-options
    #:presentation
    (make-html-presentation
      #:rules math-html-rules)
    #:rules-id 'math-html-v1))

(make-site-options
  #:renderers
  (list math-renderer
        (make-html-renderer #:options html-options)))
```

`compile-to-svg` is ordinary project code with the signature
`(expression -> SVG string)`. It can call `typst`, invoke a LaTeX-to-SVG
pipeline, or delegate to another service. The compiler ID belongs to the
project and should change whenever tool versions or options can change output.
Files that affect compilation belong in `#:dependencies`; the build
fingerprints them and records them as sources of each generated SVG.

This asset path fits Typst and standalone LaTeX output. Grove also has a
native math content IR that renders to inline MathML, including a TeX-source
presentation path. SMathML is different: its Racket DSL builds HTML/MathML
structures for the browser rather than acting as a standalone typesetter. A
project using it should bridge its structured output into a custom Grove HTML
render rule, instead of routing it through this SVG renderer. That bridge and
the SMathML package remain outside Grove's runtime dependencies. The Guix
development manifest pins an upstream SMathML revision and exposes it directly
to Racket; a project can reuse that revision or choose its own.

The example renderer scans public Forest/page/content values, emits ordinary
Grove artifacts, and puts formula inputs in its cache configuration. Each
artifact carries its page source. The HTML rule references the declared output
path through the current route prefix. The same pattern can be adapted for
other renderers or for a project-specific semantic math node.

For source files that are already standalone assets, prefer
`make-asset-pipeline`: its declared filesystem inputs are fingerprinted and
its emitted bytes enter the same artifact store. The Forest-aware renderer
shown here is useful when compiler inputs live inside content nodes.

This is intentionally a small integration point, not a promise that one
formula representation or compiler is right for every project.
