<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove projects

`(grove project)` runs a model-independent pipeline:

1. Collect source paths, either from a directory or a project-supplied collector.
2. Run an optional preparation hook before reading sources.
3. Read each source path into any Scheme value, retaining its path in a
   `project-input` record.
4. Pass a `project-context` containing those inputs and auxiliary paths to the
   project analyzer.
5. Run each renderer over the analyzer's result, combine its artifacts, and
   write them to the configured output directory.

Neither `project-input` values nor the analyzer's result need to be Grove
pages or Grove content. A project can use Grove's collector, artifact, and
build-report facilities while defining its own data model. For example,
[`examples/catalog/build.scm`](../examples/catalog/build.scm) reads `.quote`
files into a private catalog record and renders HTML and plain text without
using Forester. [`examples/inventory/build.scm`](../examples/inventory/build.scm)
reads CSV into private inventory records and emits CSV and Scheme data without
using Grove's content IR or any HTML renderer.

Projects that do want Grove's optional page/site convenience records can use
`(grove project site)`. A linked-notes project then uses Forester as its
analyzer and renderer library:

```scheme
(use-modules (forester input scribble)
             (forester render html)
             (forester render text)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project site)
             (grove render html routes)
             (grove style html))

(define read-document (make-scribble-document-reader))
(define source-directory "source")
(define style (make-html-style))
(define routes (make-html-routes))

(define project
  (make-site-project
    #:title "Linked Notes"
    #:description "A linked notebook for mathematical ideas."
    #:source-directory source-directory
    #:prepare (make-scribble-module-setup "modules")
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer
            #:style style #:routes routes #:cache-key "linked-html-v1")
          (make-forester-text-renderer
            #:prefix "text" #:cache-key "linked-text-v1"))
    #:output-directory "result"
    #:cache-key "linked-project-v1"))

(build-project project)
```

## Component contracts

| Component | Grove contract | Responsibility |
| --- | --- | --- |
| Collector | configuration → ordered source paths | Grove owns default directory collection and source fingerprints; projects can supply their own collector. |
| Prepare hook | `() → auxiliary source paths` | A project may initialize its reader environment; returned paths become tracked inputs. |
| Reader | source path → any Scheme value | An input library chooses the language and may evaluate Scheme. Grove pairs the value with its path. |
| Analyzer | `project-context → any model value` | The project or a library builds domain indexes such as Forester's links. |
| Renderer | model value → list of artifacts | Each renderer owns one output format and declares its artifact dependencies. |
| Writer | artifacts → output directory | Grove owns unchanged-file writes, stale-file removal, and build state. |

The analyzer result is opaque to Grove. Multiple renderers receive the same
value and may produce unrelated output formats; the generic project does not
require pages, Forester, or HTML. `(grove project site)` is an optional adapter
that gathers read values into Grove `site` records for libraries that want
that model. Forester uses this adapter, Grove content records, and Grove's
artifact API; it does not define the generic project contract. The current
dependency and packaging decision is recorded in
[`architecture.md`](architecture.md).

## Platform extension boundary

The extension points are callbacks and values, not subclasses: a project owns
the reader, analyzer, and model; each renderer maps that model to artifacts;
Grove coordinates phases, fingerprints declared inputs, writes artifacts, and
reports changes. An analyzer can return any Scheme value, and renderers can
produce unrelated formats. Grove's IR, Site records, HTML renderer, and HTML
style are optional libraries for projects that want them.

The current I/O contract is filesystem based. Collectors, preparation hooks,
extra sources, and artifact dependencies identify files by path. Grove reads
those files to fingerprint them, and the writer emits text artifacts beneath a
local output directory. The default collector recursively scans the configured
directory, skips hidden directories, and selects regular files in sorted
order. Its filter receives each file's basename, while the returned source
paths retain their directory structure. A project collector can choose and
order other file paths. Artifact paths are relative and cannot contain `.` or
`..` path components. This contract does not currently model in-memory inputs,
URLs, binary artifacts, or alternate output backends. A future source or
writer interface should be introduced when a concrete consumer needs one,
with stable identities and fingerprints defined for that backend.

The default collector selects non-hidden regular files recursively below the
configured directory, in sorted order. Supply `#:source-filter` to restrict
them, or `#:collect-sources` to provide an ordered path list. Keep shared
Scribble module sources outside a recursively collected page directory, then
load them with `make-scribble-module-setup`; their `.grv` files use Guile's
ordinary `define-module` form. The generic project does not interpret module
syntax. With no analyzer, renderers receive the `project-context` directly.

## Forester source paths and page IDs

`make-forester-page-reader` derives a page ID from the source file's basename,
without its `.grv` extension. Directories organize the source tree but do not
become part of the ID or page link. A prefix-and-sequence name such as
`math-0002` gives trees a globally unique, flat namespace; the source could be
`source/analysis/math-0002.grv` while the configured HTML route publishes it
at `/math-0002.html`. Two files with the same basename produce a duplicate
page ID error that lists their source paths.

`write-artifacts` keeps a `.grove-artifacts` manifest in the output directory.
On the next build it removes files listed in the previous manifest that are no
longer produced, while leaving files it did not manage alone. The manifest path
and `.grove-build-state` path are reserved for Grove metadata.

Each `project-input` contains its source path and reader value. The context
also exposes preparation paths and `#:extra-sources`; Grove fingerprints all
of these and includes them in build reports. Auxiliary inputs do not
automatically become dependencies of every artifact: declare relevant paths
on each artifact, such as a stylesheet artifact's source file. The Site
adapter gathers read page paths and preparation paths into `site-sources`.
Forester indexes depend on all site inputs; each rendered page depends on its
own source, incoming-link sources that supply backlink titles, and shared
Scheme module sources. Custom renderers can pass a source-path list as the
third argument to `make-artifact`; omitting it means dependencies are unknown.
This provenance lets Grove explain which outputs may be affected by source
changes. Grove still reads and analyzes the complete site on each build.

`build-project/report` returns a build report with added, modified, and removed
input paths plus potentially affected artifact paths. `build-project` remains
available and continues to return the artifact list. Outputs from renderers
that leave their dependencies unknown are always reported as affected. Writing
an artifact whose contents are already current leaves its file untouched.

Scribble syntax errors include the source path and current line/column. Errors
while evaluating a Scheme fragment include the source path and top-level form
number. Reader failures are wrapped with their input path; Forester metadata
and unresolved-link errors identify the page source. Evaluation errors report
the form number rather than a line because the current content IR does not
retain source spans for Scheme forms.

Renderer reuse is opt-in and applies only when every configured renderer is
wrapped as a cached renderer. Wrap a deterministic renderer with
`make-cached-renderer`, giving it a key that covers its configuration, and set
`#:cache-key` on the project for the reader, analyzer, project configuration,
and other project-level inputs. These are caller-managed version keys, not hashes of
Scheme procedures. Bump them when reader, analyzer, or renderer code or
configuration changes. File inputs declared with `#:extra-sources` are
fingerprinted automatically; cacheable renderers must not rely on side effects.
Grove reuses outputs only when source fingerprints and both keys match and the
saved output files still match their recorded contents. Each build still runs
the preparation hook, reads every source, and runs the analyzer over the full
project model; only renderer calls and their outputs are reused. This keeps
Scheme evaluation and analysis semantics predictable while avoiding repeated
output work. Consider caching earlier phases only if profiling shows they are
a meaningful cost and their side effects and invalidation rules are clear.
By default, input changes run all renderers. A cached renderer can opt into
partial updates with
`#:render-changed`; its callback receives the current model, changed source
paths, dirty old output paths, and its old artifacts. Return the complete
current artifact set for that renderer, reusing old artifacts where useful;
return `#f` to request a full render. New inputs make all that renderer's old
outputs candidates for replacement because they may add cross-page effects.
Callbacks that perform partial updates take responsibility for keeping their
complete output set correct. Cacheable renderers must not rely on skipped side
effects, and partial callbacks run only on source changes.

`make-forester-html-renderer` accepts an optional `#:cache-key`. With a key,
its HTML artifacts depend on the page itself and pages that link to it, plus
shared Scheme module sources. The incremental callback uses those dependencies
to preserve unrelated page output while refreshing indexes and pages whose
backlinks changed. The key must cover the renderer's routes, style, and
MathJax configuration.
`make-forester-text-renderer` provides the same opt-in caching and incremental
behavior, with an optional output `#:prefix` such as `"text"`.

Text output paths and cross-page references can be configured with
`make-text-routes` from `(forester render text routes)`. The route object takes
an `#:index-path` and a `#:page-path` procedure, both relative to the renderer's
output root. Links between nested text pages are rewritten as relative paths;
the renderer's optional `#:prefix` remains outside those routes.

Forester can also add an RSS 2.0 feed as an independent target:

```scheme
(use-modules (forester render rss))

(make-forester-rss-renderer
  #:site-url "https://notes.example"
  #:routes routes
  #:feed-path "feed.xml"
  #:cache-key "notes-rss-v1")
```

Add the returned renderer to `#:renderers` beside HTML or text. `#:site-url`
is the public site origin; page links use the HTML routes and the feed contains
each page's title and plain-text content. XML text is escaped, and the artifact
declares all forest source paths as dependencies. The host must provide a
correct public URL because local output paths alone cannot determine one. The
optional cache key must change when the feed URL, route, or renderer behavior
changes. Dated entries are ordered newest first using `@updated`, then
`@published`; undated entries follow in site order.

Forester can also emit a sitemap as an independent target:

```scheme
(use-modules (forester render sitemap))

(make-forester-sitemap-renderer
  #:site-url "https://notes.example"
  #:routes routes
  #:sitemap-path "sitemap.xml"
  #:cache-key "notes-sitemap-v1")
```

The sitemap contains the configured index URL and every routed page URL. It
uses the same public site origin and HTML routes as the RSS renderer, escapes
XML text, and declares all forest inputs as artifact dependencies. Pages with
`@updated` include a sitemap `lastmod` date; `@published` is not used as a
modification date. Include it in `#:renderers` beside HTML, text, or RSS; the
renderer writes `sitemap.xml` by default.

The analyzed Forest can also be exported as a readable Scheme graph datum:

```scheme
(use-modules (forester render graph))

(make-forester-graph-renderer
  #:path "forest-graph.scm"
  #:cache-key "notes-graph-v1")
```

The datum lists pages and their headings, followed by directed references with
source occurrence IDs, target page and fragment, and visible labels. It can be
loaded with Scheme's `read`; the renderer writes `forest-graph.scm` by default
and tracks all forest source paths as dependencies.

Forester links may target a page or a heading within one:

```scheme
@link["algebra"]{the algebra note}
@link["algebra#quadratic-equations"]{the quadratic equations section}
```

Each `@heading{...}` receives an HTML `id` derived from its visible text:
letters and numbers are lowercased, runs of other characters become hyphens,
and an empty slug becomes `section`. Repeated slugs receive numeric suffixes
(`overview`, `overview-2`); suffix collisions are skipped. Fragment targets
are checked against these IDs while the forest is built, so a misspelled or
stale fragment fails with the source page identified. Links in HTML and text
retain the fragment. Reverse links continue to group references by page.
Pages with headings get an automatically generated table of contents in HTML
and plain text. Plain-text headings carry matching `[#id]` markers, while
fragment links retain their `#id` in the output path.

An inline addressable tree is declared with `@subtree{...}` and must contain
one direct `@title{...}`. Forester assigns internal IDs such as
`unstable-0001` in traversal order; these are deliberately unstable and are
not durable page names. The subtree remains visible at its definition site and
also receives its own HTML, text, and sitemap entry. Its ID shares the same
namespace as file-backed page IDs, so collisions are errors. Links inside the
subtree belong to its own ID and are validated and rendered using that tree's
reference index. `@transclude["tree-id"]` embeds another tree's content
at its call site. Transclusions form a relation distinct from ordinary links,
and unresolved targets fail during forest analysis. HTML and plain-text
renderers expand the target content while keeping its standalone tree page.
Nested subtree and transclusion composition is supported; a cycle across either
kind of edge fails during forest analysis. Host artifacts declare the source
files of all composed trees as dependencies, and repeated transclusions receive
separate HTML anchor prefixes.

See [Forester subtrees and transclusion](forester-composition.md) for the
numbering rules, link behavior, and a complete runnable example.

The project title and optional `#:description` become site-level metadata.
Forester HTML emits the description in the index page and uses it as a page
description when a page has no `@summary`. RSS uses it for the channel
description, falling back to the site title when omitted. A page's `@summary`
is emitted as its HTML description and takes precedence over the site value.

The Scribble command library provides common structural content commands:
`@heading{...}`, `@blockquote{...}`, `@itemize{@item{...} ...}`, `@code{...}`,
and `@code-block{...}`. These evaluate to generic Grove IR elements, so a
renderer can choose its own output semantics. Grove's HTML renderer maps them
to `h2`, `blockquote`, `ul`/`li`, `code`, and `pre`/`code`; the plain-text
renderer uses an underlined heading, quoted lines, bullets, backticks, and
fenced code blocks. The IR remains open to other element kinds and custom
renderer handlers.

The complete runnable example, including custom routes and CSS, is in
`examples/linked-notes/build.scm`.

The model-independent catalog example can be run with:

```sh
guile -L modules examples/catalog/build.scm
```

It writes `index.html`, `catalog.css`, and `catalog.txt` to
`/tmp/grove-catalog`. The catalog defines its own record model and uses Grove's
generic project, IR, HTML, text, style, and artifact APIs.

Forester pages can declare optional metadata at top level. `@summary` is plain
text; `@published` and `@updated` use `YYYY-MM-DD` dates:

```scheme
@title{Algebra}
@summary{A short introduction to algebra.}
@published{2026-09-27}
@updated{2026-09-28}
@paragraph{...}
```

The Forester page reader removes these declarations from the body and stores
them in the Grove page metadata alist. RSS uses the summary instead of the
whole page when present. It emits `updated`, or `published` when no updated
date is supplied, as an RFC 822 `pubDate` at midnight UTC. Sites that do not
need metadata can continue to provide only a title and body.

Forester's HTML renderer uses MathJax 4's combined TeX-to-CommonHTML component
on pages that contain math. It loads the component with `defer`; projects can
pass `#:mathjax-src` to `make-forester-html-renderer` to use a locally hosted
copy or another compatible MathJax build, and can pass `#f` to disable it.
