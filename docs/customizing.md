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
| output paths | routes | `(grove render html routes)` → `make-html-routes`; `(forester render text routes)` → `make-text-routes`; renderer `#:prefix` |
| a site-wide sitemap, feed, or graph projection | additional renderers | Forester: `make-forester-project #:additional-renderers`; general sites: `make-site-project #:renderers` |
| content before Forester analysis | content transforms | `make-forester-project #:transforms` |
| content before a custom site's analysis | content transforms | `make-site-project #:transforms` |
| copy a directory of static files | a renderer | `make-static-assets-renderer` |
| generate artifacts from declared project files with an external tool | an asset pipeline | `#:pipelines` |
| turn source embedded in the analyzed model into output assets | a renderer | `#:renderers` or `#:additional-renderers` |
| choose the complete renderer set for a site | a renderer list | `make-site-project #:renderers` |
| cache behavior for any project kind | project options | `make-site-project` / `make-forester-project #:cache?`, or `project-with-options #:cache?` |
| control caching for HTML/text/Gemtext | cached renderer constructors | These accept `#:cache?`; custom rules need `#:rules-id`. On Forester's standard HTML renderer, pass it as `make-forester-project #:html-rules-id`. Project options control caching globally. |

HTML routes distinguish output paths from URLs. `#:page-path` and
`#:index-path` choose artifact paths relative to the project's output
directory. `#:url-prefix` is added to links emitted by the renderers; it does
not move artifacts into a matching subdirectory. Set the project output
directory to choose where files are written, and reuse the same routes for
HTML, RSS, and sitemap so their links agree. Set the absolute HTTP(S) site base
with `#:site-url`; it may include a path prefix but cannot include a query or
fragment.

Each renderer has its own artifact-cache identity. A plain procedure, a
renderer that opts out with `#:cache? #f`, or one with opaque rules and no
`#:rules-id` reruns on each build; cacheable peers can still reuse their
artifacts. Source changes may still rerun the full set because renderers see
the same analyzed model. Use `project-with-options #:cache? #f` to disable
document, renderer, and pipeline caching for a project.
When `make-forester-project` supplies its standard HTML renderer, use
`#:html-rules-id` to identify custom rules in `#:html-options`; declare files
those rules read through `#:extra-sources`.

If the change is about *how one node is interpreted*, use rules.  If it is
about *the environment of a render pass*, use a context.  If it is about
*reusable output policy*, use options or presentation.  If it is about
*rewriting content*, use a transform.

## Add a site output

For a Forester project, add projection renderers and keep the standard HTML
output:

```scheme
(use-modules (forester project)
             (forester render graph))

(make-forester-project
  #:home "hello"
  #:additional-renderers
  (list (make-graph-renderer #:path "forest-graph.scm")))
```

Projection renderers run against the analyzed forest and participate in
Grove's renderer cache. Use `make-site-project #:renderers` when building a
non-Forester site or when you need lower-level site configuration.

## Add output from an external tool

Keep the tool invocation in project Scheme and describe its inputs and output
as an asset pipeline when it consumes declared project files. A
`make-asset-pipeline` value from `(grove pipeline)` can be passed to the compact
Forester constructor, which keeps its standard reader, analyzer, and HTML
renderer:

```scheme
;; diagram-pipeline is a project-owned asset-pipeline value.
(make-forester-project
  #:home "hello"
  #:pipelines (list diagram-pipeline))
```

The [Graphviz adapter](../examples/hackable-assets/graphviz.scm) is a small
example: it declares a source file, runs `dot`, and returns the SVG as an
ordinary Grove artifact. A pipeline procedure receives an input lookup and
returns an artifact list; it can use any executable or library available to
the project. Grove tracks declared files and loaded Scheme code. Include tool
identity in the pipeline's `#:version` when Grove cannot observe it, as the
Graphviz adapter does.

Pipeline caching assumes that the same project code, declared and materialized
input paths, input bytes, and version produce the same returned bytes. Both
path forms participate in identity because the procedure receives materialized
paths and may use their names or locations. Grove verifies stored bytes against
their content hashes, but cannot verify that an external tool is deterministic.
Include relevant tool and environment state in `#:version`, or disable project
caching when that state cannot be represented.

Choose the extension point by the data flow, not by whether the adapter starts
a process. A pipeline consumes declared project files and returns artifacts
independently of the analyzed model. A renderer can consume values already in
the model and emit corresponding assets; the
[hackable math experiment](../examples/hackable-math/README.md) uses this for
Typst and LaTeX source embedded in pages. A render rule can turn a custom
content node into inline markup; the same experiment demonstrates this with
SMathML. Declare external files and tool identity in the renderer or HTML
configuration so cache reuse stays correct.

Plain renderer procedures run on each build because Scheme procedures have no
stable cache identity. Use `make-cached-renderer` when artifact reuse is useful;
declare unique extra file paths with `#:inputs` and state not represented by the
model or tracked project files with `#:configuration`. A pipeline may omit `#:inputs`
when it does not read project files, and may keep the default empty `#:version`
when its behavior is determined by project code and declared inputs. Add a
version for state Grove cannot observe, such as an external toolchain version
or environment variables.

Use a Scheme expression in `.plant` when the tool's result belongs inline in
that document. Declare file reads with `depend-on!` from `(grove input
scribble)` and opt out of reader caching when behavior depends on untracked
external state. Use a pipeline when the tool consumes declared project files
and returns standalone build artifacts. Both keep the integration code in the
project. Pipelines use Grove's artifact validation and content-addressed cache;
declare opaque tool identity with `#:version`.

## Three renderer entry points

| Function | Returns | Use when |
| --- | --- | --- |
| `site->html` / `site->text` / `site->gemtext` | alist of page ID string → rendered string; text and gemtext use the symbol `'index` for the aggregate index | tests, inspection, small tools |
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

Pass `summary-renderer` in `make-site-project`'s `#:renderers` list. Plain
procedures rerun on each build. Wrap the renderer with `make-cached-renderer`
when avoiding that work is worth declaring a stable cache identity; include
extra inputs there so Grove can invalidate its output correctly. See
[project.md](project.md#dependencies-and-caching).

## Options vs context

| | options | context |
| --- | --- | --- |
| lifetime | configuration snapshot | one render pass |
| shape | typed record; list and string inputs are copied | open symbol-keyed context value |
| read by | renderer entry points | rules and custom renderers |
| examples | style, rules, presentation, routes | model and renderer-defined values |

Use `make-html-options` / `make-text-options` / `make-gemtext-options` for
configuration. Use `render-context-ref` to read fields inside a rule;
project-specific fields remain available through the same interface.

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
  #:renderers
  (list (make-sitemap-renderer #:routes routes)
        (make-rss-renderer #:routes routes))
  #:output-directory "build")
```

### Transform content before analysis

Forester projects can transform each page's Content IR while keeping the
standard reader, analysis, and HTML renderer:

```scheme
(make-forester-project
  #:home "hello"
  #:transforms (list normalize-forester-content))
```

For a custom site, the same seam is available on `make-site-project`:

```scheme
(make-site-project
  #:transforms (list (compose-content-transforms
                       merge-adjacent-paragraphs
                       enrich-image-variants))
  ...)
```

### Cache intermediate bytes in extensions

Project builds already cache complete renderer artifacts and pipeline outputs.
Use `(grove cache)` only when an extension needs to reuse intermediate bytes
inside its own computation. Choose a namespaced cache and a key that includes
every semantic input and a version for the generating tool or arguments. Cache
misses return `#f`; the extension remains responsible for generating and
emitting the bytes:

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
- [render-context.md](render-context.md) — context roles and their lifecycle
  boundaries.
- [site-outputs.md](site-outputs.md) — sitemap, RSS, graph, and renderer composition.
- [project.md](project.md) — project options, cache, CLI.
- [composition.md](composition.md) — the algebra behind the layers.
