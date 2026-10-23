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
- Rules receive `(context element children next)` and use
  `render-context-ref` for named fields.
- `with-render-context` binds a scoped fallback for user hooks that call
  `chrome-message`/`chrome-date` without receiving a context.  Internal
  rendering always passes the explicit value; the fallback is thread-local.

## Where contexts are built

`(forester render context)` provides `make-site-render-context`. Each HTML,
text, or gemtext entry point builds one site-level context for that render pass
and derives page contexts from it with `render-context-with`. Separate
renderer entry points build their own contexts from their options. A project
may provide `make-site-options #:derived-context` for its derived artifacts; that
project context is not currently injected into the HTML, text, or gemtext
renderers. Locale, messages, and optional translation groups are resolved
from the context builder's arguments, not dynamic state.

`make-site-options` accepts `#:derived` (derived-artifact values) and
`#:derived-context` (a procedure of forest to render context); `make-site-project`
takes that value with `#:options`.  The project wraps the derived values into a
cached renderer that receives that context.

## Interpreter boundary

- `next` in a rule walks composed rules, then the internal handler layer, then
  the built-in interpretation.  A rule that only wants to adjust one kind can
  call `(next)` and delegate the rest.
- Trees and transclusions use lazy structural recursion and stay outside the
  rule chain; `math` is a built-in interpretation that rules do not override.
- No `current-chrome-locale` / `current-message-catalog` parameters remain in
  the render path.

## Migration status

Done:

- explicit site and page contexts for HTML, text, and gemtext;
- rule contexts and their built-in fallback in all three backends;
- chrome labels, dates, backmatter headings, skip links, and navigation names
  read the context;
- default metadata items read the page context; a custom `metadata-items` hook
  that accepts two arguments also receives it, and one-argument hooks run with
  a scoped context fallback so chrome helpers still localize;
- `title-decoration`, `home-link`, and `site-nav` hooks accept an optional
  trailing render context; their old arities keep working;
- derived artifacts receive the context and can be wired through the project;
- the parallel golden test renders two contexts on separate threads.

The parallel golden test in `tests/unit/forester/render-golden.scm` renders two
different contexts on separate threads and checks that site URL and language do
not leak.
