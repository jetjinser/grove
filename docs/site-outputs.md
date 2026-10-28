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
             (forester project)
             (forester render rss)
             (forester render sitemap)
             (forester render graph)
             (forester render site)
             (grove render html routes))

(define settings
  (make-site-settings #:site-url "https://example.org"))
(define routes
  (make-html-routes #:url-prefix "/notes/"))

(make-forester-project
  #:home "hello"
  #:html-options (make-html-options #:settings settings #:routes routes)
  #:additional-renderers
  (list (make-rss-renderer #:settings settings #:routes routes)
        (make-sitemap-renderer #:settings settings #:routes routes)
        (make-graph-renderer)))
```

The public renderer APIs have three levels. `site->html`, `site->text`,
`site->gemtext`, and `site->graph` return in-memory format values for callers
that need to inspect or further transform a projection. `render-html`,
`render-text`, `render-gemtext`, `render-rss`, `render-sitemap`, and
`render-graph` return Grove artifact lists without writing them. Use
`make-*-renderer` to adapt output to the project renderer contract and its
cache rules. RSS and sitemap expose no separate `site->*` projection; their
direct `render-*` functions already return the feed or sitemap artifacts.

The normal Forester project always includes HTML. `#:additional-renderers`
adds peer formats and projections; a `make-site-project` instead receives the
complete renderer list through `#:renderers`. A shared `make-site-settings`
value keeps the HTML, RSS, and sitemap origin and default locale aligned.
`#:site-url` is an absolute HTTP(S) origin; put a deployment path prefix in the
shared HTML routes' `#:url-prefix` so generated HTML links and absolute feed,
sitemap, and canonical URLs agree. `make-site-settings` may omit the origin
when only the locale is shared; RSS and sitemap still require an origin,
supplied through the shared settings or directly to either renderer with
`#:site-url`.

When routes omit a home page, HTML, RSS, and sitemap renderers infer it from the
analyzed site's `#:home`. You do not need to repeat the home tree ID in each
renderer. A home page set in routes must exist in the analyzed site. If a
project customizes page paths, pass the same route value to HTML, RSS, and
sitemap so their canonical links agree.

Gemtext uses `.gmi` page and index paths by default. Use
`make-gemtext-routes` to change one path while retaining the other Gemtext
default; `make-text-routes` defaults to `.txt` for the plain-text renderer.

An extension can use a plain procedure from the analyzed model to an artifact
list; plain procedures rerun on each build. Wrap it with
`make-cached-renderer` to reuse artifacts, declaring configuration Grove cannot
infer. Built-in renderers include their settings in cache identity. The HTML,
text, and Gemtext factories return cache-aware renderer values when caching is
enabled and either no custom rules are supplied or they have a stable
`#:rules-id`; otherwise they return plain procedures. Other built-in renderer
factories return cache-aware values.

For an advanced renderer that can update only affected outputs, pass
`#:render-changed` to `make-cached-renderer`. Grove calls it with the analyzed
model, changed source paths, dirty artifact paths, and the renderer's previous
artifacts. Return the complete current artifact list, reusing unaffected old
artifacts when appropriate; return `#f` to ask Grove to run the full renderer
procedure. Grove uses this path only when it can localize source changes and
the renderer configuration is cacheable; project-wide or untracked changes
fall back to the full renderer. Keep the normal full renderer for initial
builds and as the explicit fallback.

Asset pipelines consume declared file inputs and return artifacts independently
of the analyzed model. A renderer can also invoke an external tool when its
input is model data, such as source embedded in a page, and the generated asset
belongs to that content. Use a renderer rule when a custom content node should
become inline output markup. A pipeline procedure receives an input lookup,
which resolves each unique declared `#:inputs` path to a materialized path,
and returns a list of Grove artifacts, which may be empty. Grove
validates their paths and contents and attaches the pipeline's declared and
runtime-read dependencies. See the
[customization guide](customizing.md) for examples.
