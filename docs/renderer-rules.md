<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Renderer rules

A renderer is an interpretation of the content algebra, not a set of
imperative hooks.  Content has constructors such as `paragraph`, `image`,
`link`, and `subtree`; a rule set maps each constructor to a function that
returns HTML.

## The model

```scheme
(define picture-rules
  (make-html-rules
    (list (cons 'image picture-rule))))
```

- A rule is a value: `kind -> (context element children next -> sxml)`.
- `context` is a `render-context` value (forest, routes, page, variants); rules do
  not read hidden global state.  Use `render-context-ref` for named fields.
- `next` is the next interpretation of the same kind.  Delegation is explicit:
  `(next)`.  It walks composed rule sets, then the project handlers, then the
  built-in interpretation, so an overlay can adjust one case and leave the
  rest to the defaults.
- `compose-html-rules` is a left-biased merge: the overlay wins for a kind it
  interprets, and the base remains the fallback for everything else.
- `define-html-rules` is a thin macro that expands to `make-html-rules`; it
  adds no new evaluation model.  `next` remains an ordinary parameter, so the
  macro stays hygienic.

The laws the experiment checks:

1. **Identity**: composing with the empty rule set changes nothing.
2. **Associativity**: `a + (b + c) = (a + b) + c` under the same precedence.
3. **Idempotence**: composing a rule set with itself changes nothing.
4. **Override**: a later rule wins for its kind.
5. **Totality by fallback**: an overlay that calls `(next)` reaches the base
   rule and then the built-in interpretation.
6. **Context**: a rule sees exactly the context passed to the interpreter.
7. **Expansion**: the DSL produces the same values as the explicit API.

## Interpreter chain

`content->sxml` and `content->text` resolve a kind in this order:

```text
composed rules -> project handlers -> built-in interpretation
```

`content->sxml`/`content->text` accept `#:rules` and `#:context` directly, so
the built-in interpretation is always the terminal fallback.  Each backend
derives the page context per page (including transcluded targets), so a rule
can read `(render-context-ref context 'page)`.  Lazy handlers (trees and
transclusions) own structural recursion and stay outside the chain; `math` is
an internal interpretation that rules do not override in either backend.

## Presentation

`html-presentation` is the product of independent components:

```scheme
(make-html-presentation
  #:style (make-html-style #:stylesheets ... #:assets ...)
  #:rules rules)
```

`compose-html-presentations` composes styles and rules componentwise.  Its
laws reduce to the laws of the style monoid and the rule monoid; there is no
hidden ordering between the components.

## Image example

Image variants are data, not a format policy in the core:

```scheme
(define variants
  '(("forest.jpg" . (("avif" . "/images/forest.avif")
                     ("jxl"  . "/images/forest.jxl")))))

(define (picture-rule context element children next)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string children)))
    (if (assoc-ref variants source)
        (picture-from-variants source alt)
        (next))))
```

The asset pipeline produces the variant files; the rule only turns the image
element into `<picture>`.  Grove never learns which codec or encoder is used.

## Content transforms

Rendering is not the only composition layer.  A `content-transform` is an
endomorphism on content documents:

```scheme
(define normalize
  (compose-content-transforms
    merge-adjacent-paragraphs
    enrich-image-variants))

(transform-document normalize document)
```

Transforms rewrite the content algebra; rules interpret it.  Keeping them
separate means a transform can be tested without HTML and a rule can be tested
without rewriting the document.  Composition is ordinary function composition
with the identity transform as its unit.

## Image-set DSL

The image rule can be expressed as data:

```scheme
(define-image-sets image-sets
  (image-set "forest.jpg"
    (variant "image/avif" "/images/forest.avif")
    (variant "image/jxl" "/images/forest.jxl")
    (fallback "/images/forest.jpg")))

(define rules
  (make-html-rules
    (list (cons 'image (image-set-rule image-sets)))))
```

The macro expands to `make-image-set`/`make-image-variant` values.  It does
not run encoders; an asset pipeline produces the files, and the rule only
chooses the URLs and the fallback.

## Text and gemtext

The same rule values adapt to the text backends:

```scheme
(content->text document #:rules rules)
(site->text forest #:rules rules)
(site->gemtext forest #:rules rules)
```

The text adapter receives the rendered body as `children`; `next` delegates to
the project handler and then the default text interpretation.  Gemtext reuses
the text adapter and its own line-oriented handlers, so a project rule can
override one kind without rewriting the capsule.

## Options

HTML renderer configuration is a value:

```scheme
(make-html-options #:routes routes #:style style #:rules rules)
(site->html forest #:options options)
(render-html forest #:options options)
(make-html-renderer #:options options)
```

`make-html-options` is the single place for defaults and for merging
`#:presentation`.  The legacy keyword arguments remain as a compatibility shim
and are translated into the same value.

Text and gemtext follow the same pattern:

```scheme
(make-text-options #:routes routes #:rules rules)
(make-gemtext-options #:routes routes #:rules rules)
(site->text forest #:options options)
(site->gemtext forest #:options options)
```

## Production hook

`site->html`, `render-html`, `render-html-changed`, `make-html-renderer`, and
`site->text`/`render-text`/`make-text-renderer`/`site->gemtext` accept
`#:rules`.  The renderer builds the site context once per entry point and
derives the page context per page; rules receive it through `#:context`.

Project rules override the default interpretation; no core codec knowledge is
involved.  Cached text renderers disable caching when project rules are present
unless `make-text-renderer` receives a stable `#:rules-id`, so a rule change
cannot silently reuse old text artifacts.  Public hooks (`title-decoration`,
`home-link`, `site-nav`) accept an optional trailing render context, and
one-argument `metadata-items` hooks run with a scoped context fallback so their
chrome helpers still localize.
