<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Renderer rules

A renderer is an interpretation of the content algebra, not a set of
imperative hooks.  Content has constructors such as `paragraph`, `image`,
`link`, and `subtree`; a rule set maps each constructor to a backend-specific
interpretation.

## The model

```scheme
(define picture-rules
  (make-render-rules
    (list (cons 'image picture-rule))))
```

- A rule is a value: `kind -> (context element children next -> backend value)`.
  HTML rules return SXML; text and gemtext rules return strings. The rule-set
  shape and composition are shared, while `children`, `next`, and the result
  follow the backend using that rule set. A rule is reusable across backends
  only when it handles those differences itself.
- `context` is a `render-context` value (forest, routes, page, variants); rules do
  not read hidden global state.  Use `render-context-ref` for named fields.
- `next` is the next interpretation of the same kind.  Delegation is explicit:
  `(next)`.  It walks composed rule sets, then the internal handler layer, then
  the built-in interpretation, so an overlay can adjust one case and leave the
  rest to the defaults.
- `compose-render-rules` layers an overlay over a base: the overlay runs first
  for a kind it interprets, and `next` reaches the base rule before the
  backend's internal and built-in interpretations.
- `define-render-rules` is a thin macro that expands to `make-render-rules`; it
  adds no new evaluation model.  `next` remains an ordinary parameter, so the
  macro stays hygienic.

The composition laws and behaviors:

1. **Identity**: composing with the empty rule set changes nothing.
2. **Associativity**: `a + (b + c) = (a + b) + c` under the same precedence.
3. **Override**: a later rule wins for its kind and may delegate to the prior
   rule through `next`.
4. **Totality by fallback**: an overlay that calls `(next)` reaches the base
   rule, the internal layer, and then the built-in interpretation.
5. **Context**: a rule sees exactly the context passed to the interpreter.
6. **Expansion**: the DSL produces the same values as the explicit API.

Composition is not generally idempotent: composing a delegating rule again
adds another layer that `next` can invoke. Use composition to build an
intentional fallback chain; do not treat repeated composition as deduplication.

## Interpreter chain

`content->sxml` and `content->text` resolve a kind in this order:

```text
composed rules -> internal handlers -> built-in interpretation
```

`content->sxml`/`content->text` accept `#:rules` and `#:context` directly, so
the built-in interpretation is always the terminal fallback.  Each backend
derives the page context per page (including transcluded targets), so a rule
can read `(render-context-ref context 'page)`.

### Public boundary

- **Public customization point:** HTML renderer options take one
  `#:presentation`, which combines style and rules; text and gemtext options
  take backend rules directly.
- **Internal layer:** `#:internal-handlers` and `#:internal-lazy-handlers` on
  `content->sxml`/`content->text`.  They exist for the stock renderer and may
  change without notice; projects should not pass them.
- **Math:** a built-in interpretation.  Rules do not override `math` in either
  backend; a rule for `math` is ignored.
- **Trees and transclusions:** handled by lazy structural recursion before
  rules are consulted.  A rule cannot replace the tree walk; customize the
  target page instead.

## Presentation

`html-presentation` is the product of independent components:

```scheme
(make-html-presentation
  #:style (make-html-style #:stylesheets ... #:assets ...)
  #:rules rules)
```

`compose-html-presentations` composes styles and rules componentwise. Styles
accumulate in base-to-overlay order (so later CSS can override earlier CSS);
rules give later layers precedence and let them delegate to earlier rules with
`next`. Both components have an identity and associative composition, but
their precedence semantics differ.

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
  (make-render-rules
    (list (cons 'image (image-set-rule image-sets)))))
```

The macro expands to `make-image-set`/`make-image-variant` values.  It does
not run encoders; an asset pipeline produces the files, and the rule only
chooses the URLs and the fallback.

## Text and gemtext

Text and gemtext use the same rule-set API with backend-specific value
contracts. A rule that only relies on compatible child and result values can
be shared; HTML rules that construct SXML usually need a text or gemtext
counterpart:

```scheme
(content->text document #:rules rules)
(site->text forest #:options (make-text-options #:rules rules))
(site->gemtext forest #:options (make-gemtext-options #:rules rules))
```

The text adapter receives the rendered body as `children`; `next` delegates to
the internal handler layer and then the default text interpretation.  Gemtext
reuses the text adapter and its own line-oriented internal handlers, so a
project rule can override one kind without rewriting the capsule.

## Options

HTML renderer configuration is a value:

```scheme
(make-html-options #:routes routes #:presentation presentation)
(site->html forest #:options options)
(render-html forest #:options options)
(make-html-renderer #:options options)
```

`make-html-options` is the single place for HTML defaults and takes exactly one
presentation. The entry points take only `#:options`.

Text and gemtext follow the same pattern:

```scheme
(make-text-options #:routes routes #:rules rules)
(make-gemtext-options #:routes routes #:rules rules)
(site->text forest #:options options)
(site->gemtext forest #:options options)
```

## Production rule

`site->html`, `render-html`, and `make-html-renderer` take a presentation
through `make-html-options`; text and gemtext options accept `#:rules`. The
renderer builds the site context once per entry point and
derives the page context per page; rules receive it through `#:context`.

Project rules override the default interpretation; no core codec knowledge is
involved. Cached renderers disable caching when project rules are present unless
the options carry a stable `#:rules-id` (a symbol or non-empty string). Declarative HTML presentation data is part of the
presentation and therefore part of its cache identity. This applies to HTML,
text, and gemtext. Derived artifacts require a `#:configuration` procedure so
their inputs are part of the cache identity.
