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
| HTML interpretation | `grove render html`, `forester render html`, `grove render rules` | left-biased rule monoid; `#:rules`/`#:presentation` in production | default handlers are still an alist; generic rules are aliases over the HTML module |
| Text interpretation | `grove render text`, `forester render text` | `content->text #:rules` | gemtext still has its own handler layer |
| Gemtext interpretation | `forester render gemtext` | `site->gemtext`/`render-gemtext #:rules` | text-specific rules share the text adapter |
| RSS / sitemap / graph | `forester render rss`, `sitemap`, `graph`, `grove render derived` | derived-artifact values; list composition | built-ins provided; project `#:derived` entry is next |
| Presentation | `grove style html`, `forester style html`, `grove render html presentation` | style product; rules product | wired as `#:presentation`; style/rules options remain for compatibility |
| Content transforms | `grove content transform` | endomorphism monoid | wired as `make-site-project #:transforms` |
| Asset pipelines | `grove pipeline`; Flora font/figures | pipeline values; content-addressed cache | AVIF figure variants wired through `image-set`; no shared encoder/format registry yet |
| Context | `grove render context`; rules, page HTML, entry points, text, gemtext | Reader-like | backmatter metadata and chrome labels still use compatibility parameters |
| Effects/build | `grove build`, `project`, `serve`, `output` | dependency graph and cache | intentionally separate; do not force into the value model |
| Manifests | `@use-commands`, project modules | module dependency declaration | orthogonal to rendering |
| i18n | `grove i18n`, `forester i18n` | bilingual constructors; chrome locale parameters | bilingual could be a content transform; chrome context is still dynamic |
| A11y / OG | HTML renderer options | options on the HTML interpretation | could become presentation metadata |
| CLI | `grove cli` | command interface | not a composition layer |

## Candidate refactors

1. **Make `html-presentation` the production option.** (done: `#:presentation`
   is accepted by the HTML renderers; `#:style`/`#:rules` remain for
   compatibility)
2. **Generalize rules beyond HTML.** (done for the public shape: `grove render
   rules` aliases the algebra and text/gemtext accept `#:rules`; the internal
   implementation still lives in the HTML module and can be renamed later)
3. **Wire content transforms into the project.** (done: `make-site-project
   #:transforms` applies the endomorphism pipeline after reading)
4. **Implement the real image pipeline in Flora.** (done: 127 figures,
   PNG 9.3M -> AVIF 2.2M; JXL/WebP were larger for line art)
5. **Replace dynamic chrome parameters with an explicit render context.**
   (partial: rules and `html-document` read the context; entry options still
   feed `current-*`)
6. **Model derived artifacts as interpretations (optional).** (done: sitemap,
   RSS, and graph are `derived-artifact` values)

## Do not unify

- The build cache and filesystem effects: they are an interpreter/memo table,
  not a value monoid.
- The content algebra and a monad: sequencing would destroy order-independent
  rendering and caching.
- Manifests and module dependencies: they are a separate layer from rendering.
- Every backend behind one concrete type: a generic interpretation interface is
  useful; a single universal renderer is not.

## Suggested order

1. Presentation in production.
2. Generic renderer rules for HTML + text/gemtext.
3. Content transforms in project reading.
4. Real image variants in Flora.
5. Explicit render context.
6. Derived-artifact interpretations, if they earn their keep.
