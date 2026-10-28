<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Renderer rules

A renderer interprets the content algebra through a composed rule set. Content
has constructors such as `paragraph`, `image`, `link`, and `subtree`; a rule
set maps each constructor to a backend-specific interpretation.

## The model

```scheme
(define picture-rules
  (make-render-rules
    (list (cons 'image picture-rule))))
```

- A rule is a value: `kind -> (context element payload next -> backend value)`.
  HTML rules return SXML; text and gemtext rules return strings. For ordinary
  elements, `payload` is their recursively rendered children. For `math`, it is
  the complete fallback rendering for that backend. The rule-set shape
  and composition are shared, while `payload`, `next`, and the result follow
  the backend using that rule set. A rule is reusable across backends only
  when it handles those differences itself. `make-render-rules` validates that
  each interpreter can accept the four positional arguments at construction.
- `context` is a `render-context` value (forest, routes, page, variants); rules do
  not read hidden global state.  Use `render-context-ref` for named fields.
- `next` is the next interpretation of the same kind.  Delegation is explicit:
  `(next)`.  It walks composed rule sets, then the internal handler layer, then
  the backend fallback. If a stock internal handler exists for that kind, it
  handles the element; otherwise the built-in interpretation does. An overlay
  can adjust one case and delegate the rest without replacing stock behavior.
- `compose-render-rules` layers any number of sets from left to right: the
  rightmost rule runs first for a kind it interprets, and `next` reaches earlier
  rules before the stock handler or built-in fallback.
The composition laws and behaviors:

1. **Identity**: composing with the empty rule set changes nothing.
2. **Associativity**: `a + (b + c) = (a + b) + c` under the same precedence.
3. **Override**: a later rule wins for its kind and may delegate to the prior
   rule through `next`.
4. **Totality by fallback**: an overlay that calls `(next)` reaches the base
   rules, then the stock handler when installed for that kind, or the built-in
   interpretation otherwise.
5. **Context**: a rule sees exactly the context passed to the interpreter.

Composition is not generally idempotent: composing a delegating rule again
adds another layer that `next` can invoke. Use composition to build an
intentional fallback chain; do not treat repeated composition as deduplication.

## Interpreter chain

`content->sxml` and `content->text` resolve a kind in this order:

```text
composed rules -> internal handlers -> built-in interpretation
```

The final two stages are alternatives for a given kind: an installed internal
handler replaces the built-in interpretation for that kind.

`content->sxml`/`content->text` accept `#:rules` and `#:context` directly, so
the built-in interpretation is always the terminal fallback.  Each backend
derives the page context per page (including transcluded targets), so a rule
can read `(render-context-ref context 'page)`.

### Public boundary

- **Public customization point:** HTML renderer options take one
  `#:presentation`, which combines style and rules; text and gemtext options
  take backend rules directly.
- **Stock-renderer layer:** the stock Forester renderers use private dispatch
  details for tree traversal and format-specific behavior. They are not
  arguments to the public `content->sxml` or `content->text` procedures and
  are not extension points. Projects should use render rules instead.
- **Math:** the built-in semantic renderer remains the fallback. A `math` rule
  can replace it or delegate with `next`; its `payload` is the complete
  fallback math rendering (MathML/HTML for HTML, text for text and gemtext),
  rather than a list of recursively rendered child nodes.
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

`compose-html-presentations` accepts any number of layers and combines each
component left to right. Styles accumulate in that order (so later CSS can
override earlier CSS); rules give later layers precedence and let them
delegate to earlier rules with `next`. The empty presentation is the unit.

## Image example

Image variants are data, not a format policy in the core:

```scheme
(define variants
  '(("forest.jpg" . (("avif" . "/images/forest.avif")
                     ("jxl"  . "/images/forest.jxl")))))

(define (picture-rule context element payload next)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string payload)))
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

(normalize document)
```

Transforms rewrite the content algebra; rules interpret it.  Keeping them
separate means a transform can be tested without HTML and a rule can be tested
without rewriting the document.  Composition is ordinary function composition
with the identity transform as its unit.

## Image-set data

The image rule can be expressed as data:

```scheme
(define image-sets
  (list
    (make-image-set "forest.jpg"
      #:variants
      (list (make-image-variant "image/avif" "/images/forest.avif")
            (make-image-variant "image/jxl" "/images/forest.jxl"))
      #:fallback "/images/forest.jpg")))

(define rules
  (make-render-rules
    (list (cons 'image (image-set-rule image-sets)))))
```

Each set carries its own source key, variants, and required fallback URL.
`image-set-rule` accepts the list and rejects duplicate source keys. It does not
run encoders: an asset pipeline produces the files, and the rule only chooses
the URLs and fallback. These values snapshot their strings and list structure
when built.

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

The text adapter passes the rendered body as `payload`; `next` delegates to the
stock internal handler when one exists, otherwise to the default text
interpretation. Gemtext reuses the text adapter and its own line-oriented
internal handlers, so a project rule can override one kind without rewriting
the capsule.

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
involved. A renderer with project rules is cacheable only when its constructor
receives a stable `#:rules-id` (a symbol or non-empty string). Renderer values
have independent cache identities. Plain procedures always run on each build;
cacheable peers can still reuse their artifacts. Pass `#:cache? #f` to
`make-html-renderer`, `make-text-renderer`, or `make-gemtext-renderer` to
disable that renderer's cache. These factories then return plain procedures.
The cache controls live on renderer factories rather than render options.
`make-forester-project` constructs its standard HTML renderer for you; give
that renderer's opaque rules identity with `#:html-rules-id`, and list files
read by the rules in the project's `#:extra-sources`.
HTML renderer identity includes declarative presentation data and resolved
page routes. Text and Gemtext identities include their own options and routes;
custom rules need a stable `#:rules-id`. Derived artifacts require a
`#:configuration` procedure so their inputs are part of the cache identity.
