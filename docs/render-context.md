<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Render context

A render context is the explicit environment for one rendering pass.  It is a
value, not a dynamic parameter, so two pages or two sites can render
concurrently without sharing state.

## Four contexts, not one

| Context | Lifetime | Contents | Does not belong here |
| --- | --- | --- | --- |
| `render-context` | one site/page render | forest, routes, page, anchor prefix, locale, messages, site URL, default locale, hreflang, language switcher, translation groups, OG defaults | build progress, cache, reader modules, math assumptions |
| build context | one build | progress reporting, input cache, output root, dirty paths | page locale, presentation |
| reader context | one source read | reader locale, module manifest, source paths | HTML/OG metadata |
| math context | one evaluation | assumptions, derivation state | site rendering |

Only `render-context` is passed to renderer rules.  The others stay in their
own layers; merging them would turn the context into a new global state.

## Shape

```scheme
(make-render-context #:page page #:locale "en" #:project/extra "value")
(render-context-ref context 'page)
(render-context-with context #:page next-page)
(render-context-with context #:hreflang? #f)        ; explicit false
(render-context-without context 'og-image)          ; remove
(render-context-has? context 'og-image)
```

- Every provided key is stored, including an explicit `#f`.  Absence and a
  false value are distinct; use `render-context-has?` to tell them apart.
- `render-context-with` overlays every supplied value, including `#f`.  Omit a
  key to inherit it; use `render-context-without` to remove a key.
- Unknown namespaced keys are allowed, so projects can add data without
  changing the core record.
- Constructors and `render-context-ref` copy mutable strings and pair
  structure. Opaque values such as records, procedures, and hash tables remain
  shared; treat those values as read-only for the render pass.
- The context is shared with rules and custom renderers for the duration of a
  render pass. Treat it and its values as read-only; use `render-context-with`
  or `render-context-without` to derive a context with different bindings.
- Rules receive `(context element children next)` and use
  `render-context-ref` for named fields.
- Rules and chrome helpers receive the render context explicitly. A custom
  renderer builds one through `make-site-render-context` when needed. There is
  no ambient render context.

## Where contexts are built

`(forester render context)` provides `make-site-render-context`. Each HTML,
text, or gemtext entry point builds one site-level context for that render pass
and derives page contexts from it with `render-context-with`. Separate
renderer entry points build their own contexts from their options. A custom
renderer can build a context explicitly when it needs the shared Forester
helpers. Translation groups are derived from the forest by default and can be
supplied explicitly when a project needs a different index. Locale and
messages are resolved from the context builder's arguments.

## Interpreter boundary

- `next` in a rule walks composed rules, then the stock renderer's private
  handlers, then
  the built-in interpretation.  A rule that only wants to adjust one kind can
  call `(next)` and delegate the rest.
- Trees and transclusions use lazy structural recursion and stay outside the
  rule chain; `math` is a built-in interpretation that rules do not override.
- No `current-chrome-locale` / `current-message-catalog` parameters remain in
  the render path.

The parallel golden test in `tests/unit/forester/render-golden.scm` renders two
different contexts on separate threads and checks that site URL and language do
not leak.
