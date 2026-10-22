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
- `context` is an explicit value (forest, routes, page, variants); rules do
  not read hidden global state.
- `next` is a thunk for the next interpretation of the same kind.  Delegation
  is explicit: `(next)`.
- `compose-html-rules` is a left-biased merge: the overlay wins for a kind it
  interprets, and the base remains the fallback for everything else.
- `html-rules->handlers` adapts the value to `content->sxml`, closing over a
  context.
- `define-html-rules` is a thin macro that expands to `make-html-rules`; it
  adds no new evaluation model.  `next` remains an ordinary parameter, so the
  macro stays hygienic.

The laws the experiment checks:

1. **Identity**: composing with the empty rule set changes nothing.
2. **Associativity**: `a + (b + c) = (a + b) + c` under the same precedence.
3. **Idempotence**: composing a rule set with itself changes nothing.
4. **Override**: a later rule wins for its kind.
5. **Totality by fallback**: kinds without an overlay still use the base.
6. **Context**: a rule sees exactly the context passed to the adapter.
7. **Expansion**: the DSL produces the same values as the explicit API.

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

## Experiment status

The candidate API lives in `(grove render html rules)` and
`(grove render html presentation)`, with tests in
`tests/unit/grove/html-rules.scm`.  It is not wired into the default site
renderer yet; the experiment is intentionally isolated so the value model can
be evaluated before production adoption.

The next step, if adopted, is to replace the internal handler alists in
`site->html`/`render-html` with rule values and pass the production render
context (forest, routes, page) to rules that need it.

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

## Production hook

`site->html`, `render-html`, `render-html-changed`, and `make-html-renderer`
now accept `#:rules`.  The renderer builds the context per page:

```scheme
((forest . ,forest) (routes . ,routes) (page . ,page))
```

Project rules override the default interpretation; no core codec knowledge is
involved.  The isolated tests cover 19 laws and integration cases.
