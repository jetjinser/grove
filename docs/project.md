<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Projects

A Grove project connects four steps: collect source paths, read each source,
optionally analyze the resulting inputs, and render artifacts. Grove writes
those artifacts to a local output directory and reports which inputs changed.

```text
paths → reader → project inputs → analyzer → model → renderers → artifacts
```

The reader and analyzer belong to the project. Renderers receive the same
model and can produce unrelated formats. The model can be any Scheme value;
Grove does not require pages, content IR, or HTML. See
[`examples/catalog/build.scm`](../examples/catalog/build.scm) for a compact
project-owned model, and [`examples/inventory/build.scm`](../examples/inventory/build.scm)
for CSV and Scheme output without Grove's document IR.

## A linked-page project

For a Forester site, use `(grove project site)` with Forester's reader,
analyzer, and renderers. The complete runnable setup is
[`examples/linked-notes/build.scm`](../examples/linked-notes/build.scm); it
also configures CSS, text routes, a feed, a sitemap, a graph, and static files.

The site needs an explicit home page ID. For example,
`(make-html-routes #:home-page "index")` selects the page read from
`plants/index.plant`. Grove does not create a page-list landing page. HTML
routes can map IDs to files; the default page path is `<id>.html`, and the
default index path, `index.html`, redirects to the selected home page.

Forester reads each `.plant` file as a page. Its ID is the filename without
the extension; parent directories organize files but do not affect IDs. Thus
`plants/algebra/quadratics.plant` has ID `quadratics`. IDs must be unique.
Links may target a page or a heading, such as
`@link["quadratics#difference-of-squares"]{the identity}`. Missing pages,
headings, or composition targets fail while the Forest is built.

Use `@title` for the required page title. `@summary`, `@published`, and
`@updated` become page metadata. Tags and taxa remain in the content IR, so
they can be rendered or queried by Forester without being treated as ordinary
page metadata. The [relations guide](relations.md) covers content queries;
the [composition guide](forester-composition.md) explains subtrees and
transclusion.

## Inputs and outputs

The default source collector walks a directory recursively, skips hidden
directories, and returns regular files in sorted order. Use `#:source-filter`
to select filenames or `#:collect-sources` to provide an ordered path list.
Preparation hooks can load shared Scheme modules before reading. Paths
returned by the hook and paths declared with `#:extra-sources` are tracked as
project inputs.

Renderers return artifacts with relative output paths and text or byte content.
An artifact can list
the files it depends on; these dependencies let build reports identify
potentially affected outputs. Grove writes a manifest of its own artifacts,
updates unchanged files only when needed, and removes artifacts that the
previous manifest lists but the current build no longer produces. Files not
managed by Grove are left alone.

Forester provides HTML, plain text, RSS, sitemap, and Scheme graph renderers.
Grove provides HTML/text helpers and static asset copying. Each renderer owns
its output paths and behavior. In particular, the RSS renderer uses the site
URL and page dates, the sitemap uses routed page URLs and update dates, and
the graph renderer writes a Scheme-readable representation of the Forest.

## Renderer caching

Caching is opt-in. Wrap a deterministic renderer with
`make-cached-renderer` or use a Forester renderer's `#:cache-key` option. Set
the project `#:cache-key` as well. Keys are caller-managed versions for the
reader, analyzer, project options, and renderer configuration; Grove does not
hash Scheme procedures. Change a key when the behavior it covers changes.
Declare file inputs with `#:extra-sources` or as artifact dependencies so
their contents can invalidate affected outputs.

A cache hit reuses renderer artifacts, but the build still runs the
preparation hook, reads every source, and runs the analyzer over the current
project. Incremental renderer callbacks can update only affected outputs;
they must return the complete artifact set. This keeps analysis predictable
while avoiding work in renderers. See the linked-notes example for a complete
configuration.

## Scribble documents

Grove's Scribble reader evaluates Guile Scheme and converts document syntax to
content IR. Ordinary prose becomes paragraphs; commands can return content
elements or lists of document values. Grove's standard HTML and text
renderers handle common elements such as paragraphs, headings, code, and
lists. Forester adds page-specific commands such as links, tags, subtrees,
and transclusion.

Math documents use separate entry points: Formula (`φ:`) for convenient
expression reading, Semantic (`◆:`) for computational values, and Notation
(`◇:`) for display structure. Read the [math guide](math.md) for examples
and supported operations.
