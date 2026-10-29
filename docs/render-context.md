<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Render context

A render context is the explicit environment for one rendering pass.  It is a
value, not a dynamic parameter, so two pages or two sites can render
concurrently without sharing state.

## State by lifecycle stage

| Representation | Lifetime | Contents | Does not belong here |
| --- | --- | --- | --- |
| `render-context` record | one render pass | values supplied by the renderer or project | build state, reader locale, math assumptions |
| `project-context` record | one project analysis | read inputs, prepared source paths, extra source paths | render options, reader locale |
| build internals and callbacks | one build | progress, input cache, output root, dirty paths | page locale, presentation |
| reader parameters and module setup | one source read | reader locale, loaded modules, declared source dependencies | HTML/OG metadata |
| math assumptions parameter or explicit arguments | one evaluation | assumptions for mathematical relations | site rendering |

The first two rows are public context records: `render-context` is passed to
renderer rules, and `project-context` is passed to the project analyzer. The
other rows describe stage-specific state, not additional records in the public
context API. Keeping them separate avoids turning the open render environment
into global state for unrelated lifecycles.

## Shape

```scheme
(make-render-context #:model model #:project/extra "value")
(render-context-ref context 'model)
(render-context-with context #:model next-model)
(render-context-with context #:hreflang? #f)        ; explicit false
(render-context-without context 'og-image)          ; remove
(render-context-has? context 'og-image)
```

- Every provided key is stored, including an explicit `#f`.  Absence and a
  false value are distinct; use `render-context-has?` to tell them apart.
- `render-context-with` overlays every supplied value, including `#f`.  Omit a
  key to inherit it; use `render-context-without` to remove a key.
- Context keys are open-ended. Grove supplies no domain-specific keys or
  defaults; renderers and projects define the values they need.
- `content->sxml` and `content->text` create an empty render context when
  `#:context` is omitted. Rules therefore always receive a `render-context`
  value; passing any other value, including `#f`, is an error at the renderer
  boundary.
- Constructors and `render-context-ref` copy mutable strings, bytevectors,
  vectors, and pair structure. Opaque values such as records, procedures, and
  hash tables remain shared; treat those values as read-only for the render
  pass. Built-in
  translation groups use an association list so their structure is copied as
  ordinary context data.
- The context is shared with rules and custom renderers for the duration of a
  render pass. Treat it and its values as read-only; use `render-context-with`
  or `render-context-without` to derive a context with different bindings.
- Rules receive `(context element payload next)` and use
  `render-context-ref` for named fields. For ordinary elements, `payload` is
  rendered child content; for `math`, it is the complete fallback rendering
  for that backend (see [Renderer rules](renderer-rules.md#public-boundary)).
- Rules and built-in interface helpers receive the render context explicitly.
  A custom renderer builds one through `make-site-render-context` when needed.
  There is no ambient render context.

## Where contexts are built

`(forester render context)` provides `make-site-render-context`, which returns
the same `(grove render context)` record and defines the fields and defaults
used by Forester. It is a domain-specific constructor, not another context
type. Each HTML,
text, or gemtext entry point builds one site-level context for that render pass
and derives page contexts from it with `render-context-with`. Separate
renderer entry points build their own contexts from their options. A custom
renderer can build a context explicitly when it needs the shared Forester
helpers. `#:settings` supplies a shared site origin IRI and default-locale
value; `#:site-url`, `#:default-locale`, and `#:locale` override them. The
settings' default locale also supplies the interface locale unless `#:locale`
is given.
Translation groups are an association list from unique, non-empty
string keys to lists of site pages; they are derived from the forest by default
and can be supplied explicitly when a project needs a different index. Locale
and messages are resolved from the context builder's arguments. Derived groups
follow the first occurrence of each translation key in site page order, and
pages within each group keep that same order.

## Interpreter boundary

- `next` in a rule walks composed rules, then the stock renderer's private
  handler when one is installed for that kind, or the built-in interpretation
  otherwise. A rule that only wants to adjust one kind can call `(next)` and
  delegate the rest.
- Trees and transclusions use lazy structural recursion and stay outside the
  rule chain. Math rules can replace or delegate to the backend's built-in
  semantic math interpretation.
- Locale and message catalogs are explicit render-context values; the render
  path does not depend on dynamic parameters for them.

The parallel golden test in `tests/unit/forester/render-golden.scm` renders two
different contexts on separate threads and checks that site URL and language do
not leak.
