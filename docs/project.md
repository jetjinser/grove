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
[`examples/catalog/project.scm`](../examples/catalog/project.scm) for a compact
project-owned model, and [`examples/inventory/project.scm`](../examples/inventory/project.scm)
for CSV and Scheme output without Grove's document IR.

## Command-line projects

The CLI loads `project.scm` from the current directory, or the file selected by
`--project PATH`. The file's final expression must return a `project` value; it
must not call `build-project` itself. For example, this generic project copies
text inputs into an output artifact:

```scheme
(use-modules (grove project)
             (grove output)
             (ice-9 textual-ports))

(make-project
  #:source-directory "sources"
  #:read-source
  (lambda (path) (call-with-input-file path get-string-all))
  #:renderers
  (list (lambda (context)
          (list (make-artifact "combined.txt"
                  (apply string-append
                    (map project-input-value
                         (project-context-inputs context)))))))
  #:output-directory "build")
```

Create `sources/` with text files, then run `grove build` or `grove check`.
Forester projects use `make-site-project` in exactly the same way.

CLI option paths are relative to the directory where the command was invoked.
Paths inside the project are relative to the project file's directory throughout
loading and execution. The loader restores the caller's directory and uses a
fresh top-level evaluation module. Named Guile modules still have their normal
process-wide lifetime. The CLI tracks the project file and includes its content
fingerprint in configured renderer cache keys.

`build` uses renderer caching and writes validated artifacts. `check` always
runs the renderers, ignores published cache state, and does not create or modify
the output directory. Both validate duplicate, reserved, and conflicting output
paths. Checking does not sandbox Scheme callbacks or perform browser layout
validation. Output writing is not transactional across the entire directory.

Use `grove --help` or `grove check --help` for options. Success exits with `0`,
project execution failure with `1`, and invalid arguments with `2`. Summaries go
to stdout, diagnostics to stderr; `--debug` includes the original exception and
backtrace. Existing Scheme callers can use `check-project` alongside
`build-project/report`; both return the same build-report structure. A check
has no previous output state, so its report describes all tracked sources as
added and all produced artifacts as affected.

### Local preview

```sh
grove serve
grove serve --port 8081
grove serve --no-watch --output /tmp/preview
```

The first build must succeed before HTTP preview starts. An occupied port fails
without running a build. The server binds to `127.0.0.1` and serves only generated
artifacts, with `/` selecting an existing `index.html`; it does not list
directories or expose project files and cache metadata. GET and HEAD are
supported, including binary assets and MIME types. Browser caching is disabled.

Each build runs in a new Guile process, so edits to project modules take effect.
The watcher polls file identity, size, and precise timestamps, then uses Grove's
existing fingerprints and renderer caching during builds. It tracks the project
directory, source directories, declared file dependencies, and loaded Scheme
sources. Discovered external input directories are monitored for new files too.
Nested directory symlinks and hidden entries are skipped during recursive scans;
explicit source-directory symlinks are resolved and tracked file dependencies
remain observed. Keep outputs in a separate directory: generated outputs are
excluded from watching.

Builds run one at a time. Changes observed during a build queue another build.
After success, the server replaces its entire in-memory artifact set; failure
keeps the previous set available. Scheme output cannot corrupt the private
worker protocol. The browser does not refresh automatically.

Ctrl-C stops the server, its active worker, and project subprocesses in the
worker's process group, exiting with `130`. SIGTERM performs the same cleanup
and exits with `143`. Detached processes created by project code are outside
that group. `--no-watch` performs one build and serves that fixed snapshot.

Scheme runners remain silent by default. `build-project`, `build-project/report`,
and `check-project` accept `#:progress`, a callback receiving a stage symbol and
an optional path. Stages are `collect`, `prepare`, `read`, `analyze`, `render`,
`validate`, and `write`; `read` identifies each source, and `render` is skipped
on a complete renderer cache hit. CLI formatting uses this same callback.

## A linked-page project

For a Forester site, use `(grove project site)` with Forester's reader,
analyzer, and renderers. The complete runnable setup is
[`examples/linked-notes/project.scm`](../examples/linked-notes/project.scm); it
also configures CSS, text routes, a feed, a sitemap, a graph, and static files.

The site needs an explicit home page ID. For example,
`(make-html-routes #:home-page "index")` selects the page read from
`plants/index.plant`. Grove does not create a page-list landing page. HTML
routes can map IDs to files; the default page path is `<id>.html`, and the
default index path, `index.html`, redirects to the selected home page.

Forester reads each `.plant` file as a page. Its ID is the filename without
the extension; parent directories organize files but do not affect IDs. Thus
`plants/algebra/quadratics.plant` has ID `quadratics`. IDs must be unique.
Links target tree IDs, such as `@link["difference-of-squares"]{the identity}`.
A named subtree uses the same identity namespace as a file-backed tree.
Missing link or transclusion targets fail while the Forest is built.

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
renderers handle common elements such as paragraphs, code, and
lists. Forester adds page-specific commands such as links, tags, subtrees,
and transclusion.

Math documents use separate entry points: Formula (`φ:`) for convenient
expression reading, Semantic (`◆:`) for computational values, and Notation
(`◇:`) for display structure. Read the [math guide](math.md) for examples
and supported operations.
