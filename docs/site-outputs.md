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

An extension can use a plain procedure from the analyzed model to an artifact
list. Use `make-cached-renderer` when it needs cache configuration Grove cannot
infer. Built-in renderers return cache-aware renderer values and include their
settings in cache identity.

Asset pipelines remain separate because they declare file inputs and drive
external tools. Use a renderer for a projection of the analyzed model; use a
pipeline when generated output depends on declared files or external tool
execution. See the [customization guide](customizing.md) for examples.
