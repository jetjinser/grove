<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Source math in `.plant`

This user-side extension lets an article carry source in the language its
author prefers. Grove stores it as typed content; the project chooses how to
compile and render it. Grove itself does not depend on Typst, LaTeX, Racket,
or SMathML.

```scribble
@use-modules[(examples hackable-math extension)]

@title{A formula in three source languages}

@typst-math[#:alt "The integral of x squared"]{
#set page(width: auto, height: auto, margin: 4pt)
$ integral x^2 dif x $
}

@latex-math[#:alt "The integral of x squared"]{
\[
  \int x^2 \, dx
\]
}

@smathml{
  (display (Xml (Math (Mi "x"))))
}
```

The `typst-math` and `latex-math` commands make `compiled-math` content nodes.
`make-math-asset-renderer` finds those nodes and calls the project's compiler
with `(language source)`. The compiler returns SVG text; the renderer records
the article and declared dependency files as artifact sources. It derives a
stable output path from the language and source, so authors do not choose
artifact paths.

```scheme
(define math-renderer
  (make-math-asset-renderer
    #:compiler
    (lambda (language source)
      (case language
        ((typst) (typst-source->svg source))
        ((latex) (latex-source->svg source))
        (else (error "unknown math source language" language))))
    #:dependencies (list "math/macros.typ" "fonts/site-fonts.toml")))

;; Add it beside the HTML renderer in the project's renderer list.
(make-site-options
  #:renderers
  (list math-renderer
        (make-html-renderer
          #:options (make-html-options
                      #:presentation (make-html-presentation
                                       #:style project-style)))))
```

The compiler ID defaults to `"project"`. Set `#:compiler-id` when a toolchain
version or option can change independently of the project configuration and
must invalidate the renderer cache.

The compiler functions are ordinary project Scheme procedures. They may call
the `typst` executable, run a LaTeX toolchain, or use another service. Keep
temporary-file handling, diagnostics, tool selection, and extra dependencies
in project code, where they can be changed without changing Grove.

SMathML has a different output shape: it produces HTML/MathML structures,
not a standalone SVG. `@smathml` therefore creates a source-bearing node and
`make-smathml-html-rules` accepts a project callback from Racket source to XML.
The example's `smathml-source->xml` runs that source with its pinned SMathML
collection; replace it when the project needs different Racket modules or
process handling. The rule parses the XML as SXML and passes it to Grove's
HTML serializer. Compose it with `math-html-rules` and include the
Racket/SMathML version in the HTML renderer's `#:rules-id`:

```scheme
(define project-math-rules
  (compose-render-rules
    math-html-rules
    (make-smathml-html-rules #:compile smathml-source->xml)))

(make-html-options
  #:presentation (make-html-presentation #:rules project-math-rules)
  #:rules-id "typst-0.13; latex-profile-2; smathml-racket-1")
```

`smathml-source->xml` can launch Racket with the project's pinned SMathML
collection, or be replaced by a different implementation. The adapter and
toolchain remain project-owned. For standalone source files rather than code
authored inside an article, use `make-asset-pipeline`: it fingerprints the
declared files and publishes emitted bytes to Grove's artifact store.

Grove also has its own math content IR and a TeX-source presentation path.
Use that when its semantics fit; use these source nodes when the project needs
the behavior or notation of another toolchain.
