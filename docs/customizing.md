<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Customizing a site

This is the shortest path from “I want to change X” to the right Grove API.

## Decision table

| I want to change… | Use | Where |
| --- | --- | --- |
| one element's HTML | an HTML presentation | `make-html-options #:presentation (make-html-presentation #:rules ...)` |
| one element's text/gemtext output | backend rules using the shared rule API | `make-text-options` / `make-gemtext-options #:rules` |
| the metadata line under a title | page-specific presentation data | `make-html-presentation #:metadata-items` |
| the site navigation | static SXML and route-aware links | `make-html-presentation #:site-nav` |
| the home link | a declarative home-link value | `make-html-presentation #:home-link` |
| tree headings or the table of contents | page-specific presentation data | `make-html-presentation #:title-decorations` |
| locale, messages, or date formatting | renderer options | `make-html-options #:locale/#:messages` |
| output paths | routes | `make-html-routes`, `make-text-routes`, `#:prefix` |
| a site-wide sitemap, feed, or graph projection | derived artifacts | Forester: `make-forester-project #:derived`; general sites: `make-site-project #:derived` |
| content before rendering | content transforms | `make-site-project #:transforms` |
| copy a directory of static files | a renderer | `make-static-assets-renderer` |
| generate files from declared inputs with an external tool | an asset pipeline | `#:pipelines` |
| add a custom output format from the site model | a renderer returning artifacts | `make-site-project #:renderers` |
| project cache behavior | project options | `make-site-project #:cache?`, `#:output-directory` |
| one renderer's cache behavior | cached renderer constructor | `make-*-renderer #:cache?`, `#:rules-id` |

If the change is about *how one node is interpreted*, use rules.  If it is
about *the environment of a render pass*, use a context.  If it is about
*reusable output policy*, use options or presentation.  If it is about
*rewriting content*, use a transform.

## Add a derived site artifact

For a Forester project, pass derived artifacts directly and keep the standard
HTML output:

```scheme
(use-modules (forester project)
             (forester render derived))

(make-forester-project
  #:home "hello"
  #:derived (list (graph-derived #:path "forest-graph.scm")))
```

Derived artifacts run against the analyzed forest and participate in Grove's
renderer cache. Use `make-site-project #:derived` when building a non-Forester
site or when you need lower-level site configuration.

## Add output from an external tool

Keep the tool invocation in project Scheme and describe its inputs and output
as an asset pipeline. A `make-asset-pipeline` value from `(grove pipeline)` can
be passed to the compact Forester constructor, which keeps its standard reader,
analyzer, and HTML renderer:

```scheme
;; diagram-pipeline is a project-owned asset-pipeline value.
(make-forester-project
  #:home "hello"
  #:pipelines (list diagram-pipeline))
```

The [Graphviz adapter](../examples/hackable-assets/graphviz.scm) is a small
example: it declares a source file, runs `dot`, and emits the SVG as an ordinary
Grove artifact. A pipeline procedure receives an input lookup and an `emit`
callback; it can use any executable or library available to the project. Grove
tracks declared files and loaded Scheme code. Include tool identity in the
pipeline's `#:version` when Grove cannot observe it, as the Graphviz adapter
does.

The ordinary path needs no cache key: Grove wraps plain renderer procedures
for caching and tracks project inputs. A pipeline may omit `#:inputs` when it
does not read project files, and may keep the default empty `#:version` when
its behavior is determined by project code and declared inputs. Add a version
only for relevant state Grove cannot observe, such as an external toolchain
version or environment variables. Use `make-cached-renderer` only when a
renderer reads additional files or has hidden configuration; declare those
dependencies so the cache stays correct.

Use a Scheme expression in `.plant` when the tool's result belongs inline in
that document. Use a pipeline when the result should be a separate build file.
Both keep the integration code in the project. For inline expressions, declare
file reads with `depend-on!` from `(grove input scribble)` and opt out of reader
caching when behavior depends on untracked external state. Pipelines use Grove's
artifact validation and content-addressed cache; declare opaque tool identity
with `#:version`.

## Three renderer entry points

| Function | Returns | Use when |
| --- | --- | --- |
| `site->html` / `site->text` / `site->gemtext` | alist of page id → rendered string; text and gemtext also include `"index"` | tests, inspection, small tools |
| `render-html` / `render-text` / `render-gemtext` | complete list of build artifacts, including shared assets and indexes | custom build steps that need artifacts directly |
| `make-html-renderer` / `make-text-renderer` / `make-gemtext-renderer` | cached renderer value | `make-site-project #:renderers` |

All of them take one options value:

```scheme
(site->html forest #:options (make-html-options ...))
(render-html forest #:options (make-html-options ...))
(make-html-renderer #:options (make-html-options ...))
```

For direct composition without a project build, `(grove build)` exports
`render-model`: it applies a list of renderer values or ordinary renderer
procedures to one model and concatenates their artifact lists in order. It
does not collect inputs, cache results, or write files.

### Write a renderer

A site renderer is an ordinary procedure from the analyzed site model to a list
of artifacts. Artifact paths are relative to the project's output directory.
For example, a text summary can be added alongside the built-in renderers:

```scheme
(use-modules (grove output))

(define (summary-renderer forest)
  ;; forest->summary is supplied by the project.
  (list (make-artifact "summary.txt" (forest->summary forest))))
```

Pass `summary-renderer` in `make-site-project`'s `#:renderers` list. Grove
wraps plain renderer procedures for caching. Use `make-cached-renderer` when
the renderer reads inputs outside the model or needs an explicit cache
configuration; declare those inputs there so Grove can invalidate its output
correctly. See [project.md](project.md#dependencies-and-caching).

## Options vs context

| | options | context |
| --- | --- | --- |
| lifetime | configuration snapshot | one render pass |
| shape | typed record; list and string inputs are copied | symbol alist with typed accessors |
| read by | renderer entry points | rules and derived artifacts |
| examples | style, rules, presentation, routes | forest, page, locale, messages |

Use `make-html-options` / `make-text-options` / `make-gemtext-options` for
configuration.  Use `render-context-page`, `render-context-locale`, and the
other typed accessors inside a rule.  `render-context-ref` remains available
for project-defined keys.

## Minimal examples

### Change one image rule

```scheme
(define rules
  (make-render-rules
    (list (cons 'image
                (lambda (context element children next)
                  (if (string=? (content-element-property element 'source)
                                "forest.jpg")
                      (picture-from-variants ...)
                      (next)))))))

(make-html-options
  #:presentation (make-html-presentation #:rules rules))
```

### Add a metadata item

```scheme
(make-html-options
  #:presentation
  (make-html-presentation
      #:metadata-items
      '(("about" . ((li (@ (class "meta-item")) "author"))))))
```

### Add sitemap and feed

```scheme
(make-site-project
  #:derived
  (list (sitemap-derived #:routes routes)
        (rss-derived #:routes routes))
  #:output-directory "build")
```

### Rewrite content before rendering

```scheme
(make-site-project
  #:transforms (list (compose-content-transforms
                       merge-adjacent-paragraphs
                       enrich-image-variants))
  ...)
```

### Cache expensive generated bytes

`(grove cache)` provides a small shared content cache for renderer extensions.
Choose a namespaced cache and a key that includes every semantic input and a
version for the generating tool or arguments. Cache misses return `#f`; the
extension remains responsible for generating and emitting the bytes:

```scheme
(define subset-cache (make-content-cache "site-font-subsets"))
(define key
  (content-fingerprint
    (string-append "fonttools-v1:"
                   (file-fingerprint font-path) ":" codepoints)))

(or (content-cache-ref subset-cache key)
    (let ((bytes (run-subsetter font-path codepoints)))
      (content-cache-store! subset-cache key bytes)
      bytes))
```

The cache stores verified bytevectors in Grove's shared store. It is an
optimization only: a missing or unusable entry means the extension computes
the value again.

## Where to read next

- [renderer-rules.md](renderer-rules.md) — rules, composition, `next`.
- [render-context.md](render-context.md) — contexts and the four context kinds.
- [derived-artifacts.md](derived-artifacts.md) — sitemap, RSS, graph.
- [project.md](project.md) — project options, cache, CLI.
- [composition.md](composition.md) — the algebra behind the layers.
