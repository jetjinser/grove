<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Composition audit

This is an inventory of Grove's existing features against the composition
model: values with an identity, an associative composition, and an explicit
interpreter.  It is a map for refactoring, not a claim that every layer should
become the same type.

## Layers

| Layer | Current implementation | Composition | Gap |
| --- | --- | --- | --- |
| Content algebra | `grove ir`, `forester commands`, `forester composition`, `forester input scribble` | tree construction | none; keep it a free algebra, not a monad |
| HTML interpretation | `grove render html`, `forester render html`, `grove render html rules` | left-biased rule monoid; `#:rules` in production | default handlers are still an alist; `html-presentation` is not the public option |
| Text interpretation | `grove render text`, `forester render text` | direct `case` on kind | no composable rules |
| Gemtext interpretation | `forester render gemtext` | text handlers | no composable rules |
| RSS / sitemap / graph | `forester render rss`, `sitemap`, `graph` | direct artifact renderers | derived outputs are not modeled as interpretations |
| Presentation | `grove style html`, `forester style html`, `grove render html presentation` | style product; rules product | presentation value is not wired into `site->html` |
| Content transforms | `grove content transform` | endomorphism monoid | not wired into project reading |
| Asset pipelines | `grove pipeline`; Flora font/figures | pipeline values; content-addressed cache | no shared encoder/format registry; image-set is data only |
| Context | `current-*` parameters; rule context alists | Reader-like | two context mechanisms coexist |
| Effects/build | `grove build`, `project`, `serve`, `output` | dependency graph and cache | intentionally separate; do not force into the value model |
| Manifests | `@use-commands`, project modules | module dependency declaration | orthogonal to rendering |
| i18n | `grove i18n`, `forester i18n` | bilingual constructors; chrome locale parameters | bilingual could be a content transform; chrome context is still dynamic |
| A11y / OG | HTML renderer options | options on the HTML interpretation | could become presentation metadata |
| CLI | `grove cli` | command interface | not a composition layer |

## Candidate refactors

1. **Make `html-presentation` the production option.** (done: `#:presentation` is accepted by the HTML renderers)
   `site->html` and `make-html-renderer` currently take `#:style` and `#:rules`
   separately.  A single `#:presentation` would make the product monoid the
   public shape and remove the chance of mismatched components.
2. **Generalize rules beyond HTML.**
   Rename/parameterize `html-rules` as renderer rules with an output type, and
   let text/gemtext use the same identity/compose/next model.  This is the
   biggest payoff for the theory: one composition story across backends.
3. **Wire content transforms into the project.**
   Add a transform pipeline between reading and page construction, so rewrites
   (variant enrichment, adjacent-paragraph merging) are pure and testable
   without HTML.
4. **Implement the real image pipeline in Flora.**
   The encoders are now in Flora's manifest.  Generate AVIF/JXL/WebP variants
   from a small manifest, feed `image-set`, and compare sizes before choosing a
   default policy.
5. **Replace dynamic chrome parameters with an explicit render context.**
   The rule context is already explicit; the remaining `current-*` parameters
   (site URL, locale, hreflang, OG defaults) are a second mechanism.  This is a
   larger refactor; do it after the presentation/rules shape settles.
6. **Model derived artifacts as interpretations (optional).**
   RSS, sitemap, and graph are already artifact renderers; they can share the
   same interpretation vocabulary without sharing the HTML rule type.

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
