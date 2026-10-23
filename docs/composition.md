<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Composition audit

This is an inventory of Grove's existing features against the composition
model: values with an identity, an associative composition, and an explicit
interpreter.  It is a map for refactoring, not a claim that every layer should
become the same type.

See [Render context](render-context.md) for the render/build/reader/math
boundary.

## Layers

| Layer | Current implementation | Composition | Gap |
| --- | --- | --- | --- |
| Content algebra | `grove ir`, `forester commands`, `forester composition`, `forester input scribble` | tree construction | none; keep it a free algebra, not a monad |
| HTML interpretation | `grove render html`, `forester render html`, `grove render rules` | associative rule layers with rightmost precedence and explicit `next` fallback; the HTML renderer takes `#:presentation` | internal handler layer is still an alist (not public) |
| Text interpretation | `grove render text`, `forester render text` | `content->text #:rules`/`#:context`; internal handlers then built-in fallback | none |
| Gemtext interpretation | `forester render gemtext` | `site->gemtext`/`render-gemtext #:rules` | reuses the text adapter and its own line-oriented internal handlers |
| RSS / sitemap / graph | `forester render rss`, `sitemap`, `graph`, `grove render derived` | derived-artifact values; list composition; project `#:derived` | none |
| Presentation | `grove style html`, `forester style html`, `grove render html presentation` | stylesheets/assets append base-to-overlay; rules use rightmost precedence with explicit `next` fallback | HTML style and rule policy share one input; hooks and site metadata remain renderer options |
| Content transforms | `grove content transform` | endomorphism monoid | wired as `make-site-options #:transforms` |
| Asset pipelines | `grove pipeline`; Flora font/figures | pipeline values; content-addressed cache | AVIF figure variants wired through `image-set`; no shared encoder/format registry yet |
| Context | `grove render context`; `forester render context`; rules, page HTML, entry points, text, gemtext | explicit value, unknown keys, explicit `#f`, one site context per renderer entry point | project `#:derived-context` currently feeds derived artifacts only |
| Effects/build | `grove build`, `project`, `serve`, `output` | dependency graph and cache | intentionally separate; do not force into the value model |
| Manifests | `@use-commands`, project modules | module dependency declaration | orthogonal to rendering |
| i18n | `grove i18n`, `forester i18n` | bilingual constructors; chrome reads the render context | bilingual could be a content transform |
| A11y / OG | HTML renderer options | renderer configuration and per-page metadata | defaults belong to HTML rendering; project-specific values belong to site settings or hooks |
| CLI | `grove cli` | command interface | not a composition layer |

## Implemented composition layers

1. **Make `html-presentation` the only HTML style/rule input.** (done:
   `make-html-options` accepts `#:presentation`; style and rule options are
   grouped into that value)
2. **Generalize rules beyond HTML.** (done: HTML, text, and gemtext share the
   rule-set representation and composition API; each backend keeps its own
   children and result contract)
3. **Wire content transforms into the project.** (done: `make-site-options
   #:transforms` applies the endomorphism pipeline after reading)
4. **Implement the real image pipeline in Flora.** (done: 127 figures,
   PNG 9.3M -> AVIF 2.2M; JXL/WebP were larger for line art)
5. **Replace dynamic chrome parameters with an explicit render context.**
   (done: each renderer entry point builds one site context, page contexts are
   derived from it, and the `current-*` chrome parameters are gone)
6. **Model derived artifacts as interpretations (optional).** (done: sitemap,
   RSS, and graph are `derived-artifact` values wired through the project)

## Do not unify

- The build cache and filesystem effects: they are an interpreter/memo table,
  not a value monoid.
- The content algebra and a monad: sequencing would destroy order-independent
  rendering and caching.
- Manifests and module dependencies: they are a separate layer from rendering.
- Every backend behind one concrete type: a generic interpretation interface is
  useful; a single universal renderer is not.

## Open design questions

- Keep the handler alist private unless third-party backends need to replace
  dispatch itself; render rules already cover element-level extension.
- Keep HTML hooks and site metadata separate from `html-presentation` while
  they describe site structure or metadata rather than reusable visual policy.
- Consider a shared image-variant/encoder registry only when more than one
  consumer needs the same format-selection contract.
