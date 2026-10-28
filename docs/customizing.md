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
| output paths | routes | `(grove render html routes)` → `make-html-routes`; `(forester render text routes)` → `make-text-routes` for text and Gemtext; renderer `#:prefix` |
| a site-wide sitemap, feed, or graph projection | additional renderers | Forester: `make-forester-project #:additional-renderers`; general sites: `make-site-project #:renderers` |
| content before Forester analysis | content transforms | `make-forester-project #:transforms` |
| content before a custom site's analysis | content transforms | `make-site-project #:transforms` |
| copy a directory of static files | a renderer | `make-static-assets-renderer` |
| generate artifacts from declared project files with an external tool | an asset pipeline | `#:pipelines` |
| turn source embedded in the analyzed model into output assets | a renderer | `#:renderers` or `#:additional-renderers` |
| choose the complete renderer set for a site | a renderer list | `make-site-project #:renderers` |
| cache behavior for any project kind | project options | `make-site-project` / `make-forester-project #:cache?`, or `project-with-options #:cache?` |
| control caching for HTML/text/Gemtext | built-in renderer factories | These accept `#:cache?` and return plain procedures when caching is disabled; custom rules need `#:rules-id`. On Forester's standard HTML renderer, pass it as `make-forester-project #:html-rules-id`. Project options control caching globally. |

`make-static-assets-renderer` copies regular files below its source directory,
preserving their relative paths and optionally adding `#:prefix`. It follows
`collect-directory`: hidden directories, directory symlinks, and special files
are skipped; hidden files and symlinks to regular files are included. Grove
tracks each copied file as both a project input and an artifact dependency, so
changed files are reread and removed files disappear from the output set.
Use it when directory membership should be discovered during each build. Use
an asset pipeline when a tool step has a declared input list and needs Grove to
provide materialized paths to its procedure.

HTML routes distinguish output paths from URLs. `#:page-path` and
`#:index-path` choose artifact paths relative to the project's output
directory. `#:url-prefix` is added to links emitted by the renderers; it does
not move artifacts into a matching subdirectory. Set the project output
directory to choose where files are written, and reuse the same routes for
HTML, RSS, and sitemap so their links agree. Set the absolute HTTP(S) origin
with `#:site-url` (scheme and authority, optionally followed by `/`). Put a
deployment path such as `/notes/` in the shared routes' `#:url-prefix`; this
keeps HTML links and absolute RSS, sitemap, and canonical URLs aligned. A URL
prefix is a site path, so it cannot start with `//` (which would select another
authority). The origin cannot include credentials, a query, or a fragment.

Route callbacks must be deterministic for a fixed project state: the same page
ID must resolve to the same artifact path. Renderers may evaluate them more
than once while validating paths, building links, and computing cache identity.

HTML, RSS, and sitemap renderers infer the home page from the site when the
routes omit it. For lower-level integrations that need to derive a routes value
with an explicit home page while preserving its other settings, use
`html-routes-with-home-page` from `(grove render html routes)`.

Each renderer has its own artifact-cache identity. A plain procedure, a
built-in renderer configured with `#:cache? #f`, or one with opaque rules and
no `#:rules-id` reruns on each build; cacheable peers can still reuse their
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

## Integrate an external tool

Choose the extension point from the tool's input and where its result belongs:

| Tool input and result | Extension point |
| --- | --- |
| A declared project file produces a standalone artifact, independently of the analyzed model | Asset pipeline in `#:pipelines`. |
| Data in the analyzed model produces a standalone artifact | Renderer in `#:renderers` or `#:additional-renderers`; the [hackable math experiment](../examples/hackable-math/README.md) compiles Typst and LaTeX source embedded in pages this way. |
| A custom content node should become inline output markup | Render rule; the hackable math experiment uses this for SMathML. |
| The result belongs inline in the `.plant` page being evaluated | Scheme in `.plant`; register files with `depend-on!` from `(grove input)`, or use `#:cache-dependencies` for reader-wide inputs. Disable reader caching when behavior depends on untracked external state. |

Inline Scheme uses the normal document cache. `depend-on!` tracks files, but
Grove does not fingerprint an executable's version, environment variables, or
network state. Track a version or configuration file when those affect the
result, or call `disable-cache!` before the effect so it runs on every build.
Use a renderer or pipeline when tool identity should be declared once and its
output should be reused as a project artifact.

### File-driven tool example

Keep the invocation in project Scheme and describe its declared inputs and
artifacts with `make-asset-pipeline` from `(grove pipeline)`. Pass the value to
the compact Forester constructor to retain its standard reader, analyzer, and
HTML renderer:

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
Graphviz adapter does. If a path is only known during execution, register it
with `depend-on!` before reading or passing it to the tool; see the
[project guide](project.md#dependencies-and-caching) for how this affects cache
reuse.

Each value in `#:pipelines` is an independent file-to-artifact generator. A
pipeline can perform several tool steps inside its procedure, but its outputs
are not inputs to sibling pipelines. If one generated result feeds another,
compose those steps in one pipeline procedure.

### Cache identity

Plain renderer procedures run on every build. For artifact reuse, wrap one with
`make-cached-renderer`, declare external files with `#:dependencies`, and
describe untracked output-affecting state with `#:configuration`. An asset
pipeline may omit `#:inputs` when it reads no project files; add `#:version`
when toolchain or environment state can change its output. See
[site outputs](site-outputs.md)
for the cache identity and dependency rules. Pipeline reuse assumes that the
same project code, declared and materialized paths, input bytes, and version
produce the same bytes. Grove verifies stored-byte integrity but cannot verify
external tool determinism.

Renderer `#:dependencies` make those files part of the project's change set
and invalidate cached renderer output when they change. If the renderer has
`#:render-changed`, its callback receives those changed paths for selective
work. Artifact `#:sources` maps each output to the tracked paths that affect
it; it does not register those paths. Cached renderers can use
`#:dependencies` for renderer inputs. For a plain renderer, put renderer-only
inputs in project `#:extra-sources`; use the reader input tracker when the
reader or analyzer itself reads them. To select outputs safely, also pass each
artifact's actual dependencies with `#:sources` to `make-artifact` or
`make-byte-artifact`; Grove uses these paths to identify dirty outputs for the
callback. Without
`#:render-changed`, Grove reruns the renderer as a whole when an output is dirty.
Omitting `#:sources` means an artifact's dependencies are unknown, so it
cannot be selectively reused after project changes; an explicit empty list
declares that it depends on no files.

## Three renderer entry points

| Function | Returns | Use when |
| --- | --- | --- |
| `site->html` / `site->text` / `site->gemtext` | alist of page ID string → rendered string; text and gemtext use the symbol `'index` for the aggregate index | tests, inspection, small tools |
| `render-html` / `render-text` / `render-gemtext` | complete list of build artifacts, including shared assets and indexes | custom build steps that need artifacts directly |
| `make-html-renderer` / `make-text-renderer` / `make-gemtext-renderer` | cached renderer when caching is enabled and either no custom rules are supplied or they have a stable `#:rules-id`; otherwise a plain procedure | `make-site-project #:renderers` |

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
                (lambda (context element payload next)
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

### Cache derived bytes inside extensions

Project builds already cache complete renderer artifacts and pipeline outputs.
Use `(grove cache)` when an extension needs finer-grained reuse than its
complete renderer or pipeline result provides. For example, a font subset may
depend only on one font's bytes, a character set, and the subsetting tool
version, so it can be reused even when an unrelated source change reruns the
renderer. Choose a namespaced cache and a key that includes every semantic
input and a version for the generating tool or arguments. Cache misses return
`#f`; the extension remains responsible for generating and emitting the bytes:

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
