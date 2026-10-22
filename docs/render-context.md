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
(render-context-ref context 'page)
(render-context-ref context 'locale)
(render-context-with context #:page next-page)
```

- Site-level fields are created once by `site->html`/`render-html`.
- Page-level fields are derived with `render-context-with`; the previous value
  is never mutated.
- Rules receive `(context element children next)` and use
  `render-context-ref` for named fields.
- Unknown namespaced keys are allowed, so projects can add data without
  changing the core record.

## Migration status

Done:

- rule contexts;
- `html-document` locale/site-url/default-locale/OG locale;
- `page-html` hreflang, language navigation, OG defaults, and backmatter labels;
- `site->html`, `render-html`, `render-html-changed`, and `make-html-renderer`
  construct the site context once;
- chrome date/messages for tree labels, TOC, and language switcher;
- `page-html` requires a context and the site-level `current-*` parameters
  are gone.

Pending:

- backmatter entry metadata and default metadata items still read chrome
  parameters;
- gemtext uses a render-context record for rules; its labels still use the
  chrome compatibility path;
- the project `#:derived` entry is not wired yet.

The parallel golden test in `tests/unit/forester/render-golden.scm` renders two
different contexts on separate threads and checks that site URL and language do
not leak.
