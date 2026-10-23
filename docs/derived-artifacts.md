<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Derived artifacts

Sitemap, RSS, and graph are projections of the forest, not HTML
interpretations.  They share a value and a composition law, but not the HTML
rule type.

## Value

```scheme
(make-derived-artifact
  #:name "sitemap"
  #:procedure (lambda (context forest) ...))
```

- The procedure receives an explicit `render-context` and the forest.
- It returns a list of artifacts.
- `compose-derived-artifacts` concatenates artifact values/lists; the empty
  composition is the identity.
- `render-derived-artifacts` runs each procedure and concatenates the results.

## Built-ins

`(forester render derived)` provides:

```scheme
(sitemap-derived #:routes routes)
(rss-derived #:routes routes)
(graph-derived #:path "data/graph.scm")
```

- Sitemap uses the context site URL and default locale.
- RSS uses the context site URL and language, plus explicit stylesheet options.
- Graph is a pure data projection; it needs no render rules.

## Project wiring

`make-site-project` accepts derived artifacts directly:

```scheme
(make-site-project
  #:derived (list (sitemap-derived #:routes routes)
                  (rss-derived #:routes routes)
                  (graph-derived))
  #:context (lambda (forest)
              (make-site-render-context forest
                #:settings settings
                #:routes routes
                #:locale "en")))
```

- `#:derived` is a list of `derived-artifact` values.
- `#:context` is a procedure from forest to `render-context`; the default
  builds a context with only the forest.
- The project wraps the list in a cached renderer.  Its configuration includes
  the artifact names plus each artifact's `derived-artifact-configuration`
  (path, routes, language, stylesheet, and site URL), so changing a derived
  option invalidates the cache.
- The project computes the context once per forest and shares it between the
  configuration and the render call.
- Without `#:derived` the project behaves exactly as before.

## Boundary

- Do not model these as `render-rules`: they have no per-kind interpretation.
- Do not put cache or progress into the derived value; the build cache remains
  a separate interpreter.
- Derived artifacts still receive the same `render-context`; that is the only
  shared mechanism with the HTML/text/gemtext renderers.
