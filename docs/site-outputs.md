<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Site outputs

Every output derived from the analyzed model is a renderer. Renderers are
ordinary procedures or renderer values, listed in the order Grove runs them.
Their artifacts share validation, caching, and publication behavior.

Forester provides renderers for HTML, text, Gemtext, RSS, sitemap, and a
Scheme-datum graph. Import the constructors from the format modules:

```scheme
(use-modules (forester render html)
             (forester render rss)
             (forester render sitemap)
             (forester render graph))

(make-forester-project
  #:home "hello"
  #:additional-renderers
  (list (make-rss-renderer #:site-url "https://example.org")
        (make-sitemap-renderer #:site-url "https://example.org")
        (make-graph-renderer)))
```

The normal Forester project always includes HTML. `#:additional-renderers`
adds peer formats and projections; a `make-site-project` instead receives the
complete renderer list through `#:renderers`.

When routes omit a home page, HTML, RSS, and sitemap renderers infer it from the
analyzed site's `#:home`. You do not need to repeat the home tree ID in each
renderer. If a project customizes page paths, pass the same route value to HTML,
RSS, and sitemap so their canonical links agree.

An extension can use a plain procedure from the analyzed model to an artifact
list. Use `make-cached-renderer` when it needs cache configuration Grove cannot
infer. Built-in renderers return cache-aware renderer values and include their
settings in cache identity.

For an advanced renderer that can update only affected outputs, pass
`#:render-changed` to `make-cached-renderer`. Grove calls it with the analyzed
model, changed source paths, dirty artifact paths, and the renderer's previous
artifacts. Return the complete current artifact list, reusing unaffected old
artifacts when appropriate; return `#f` to ask Grove to run the full renderer
procedure. Grove uses this path only when it can localize source changes and
the renderer configuration is cacheable; project-wide or untracked changes
fall back to the full renderer. Keep the normal full renderer for initial
builds and as the explicit fallback.

Asset pipelines consume declared file inputs and emit artifacts independently
of the analyzed model. A renderer can also invoke an external tool when its
input is model data, such as source embedded in a page, and the generated asset
belongs to that content. Use a renderer rule when a custom content node should
become inline output markup. A pipeline procedure receives an input lookup and
an emit callback; the lookup resolves each unique declared `#:inputs` path to
a materialized path, while emit accepts a relative artifact path and string or
bytevector content. It must emit at least one artifact. See the
[customization guide](customizing.md) for examples.
