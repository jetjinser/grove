<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Project reference

For setup and CLI usage, see the [README](../README.md) and the
[API map](api.md). The project file's
final expression returns a project; it must not run a build itself. Use
[`examples/linked-notes/project.scm`](../examples/linked-notes/project.scm)
for a site configuration, or the [catalog](../examples/catalog/project.scm)
and [inventory](../examples/inventory/project.scm) for project-owned models.

## Choose a project shape

| If your project… | Start with… |
| --- | --- |
| reads arbitrary files into a project-owned model | `make-project` from `(grove project)` |
| reads pages into Grove content, then analyzes and renders a site | `make-site-project` from `(grove project site)` |
| reads and analyzes Forester `.plant` trees with standard HTML output | `make-forester-project` from `(forester project)` |

`make-project` is the general build lifecycle: collect inputs, prepare,
read each input, analyze the resulting model, and run renderers that return
artifacts. It does not require Grove's Content IR or a site model. The catalog
and inventory examples show this shape.

`make-site-project` adapts that lifecycle for pages. It builds `site-page?`
values from the page reader, optionally transforms their Content IR, combines
them into a `site?`, then passes that site to `#:analyze`. The analyzer may
return the same site or another model for the renderers. Forester adds its
linked-tree analysis on top of that site. Use this entry point when
those page and content semantics are useful; there is no need to wrap an
unrelated project model as a site. `make-site-project` does not select an
output format or install a renderer: `#:renderers` is the complete output set.
The higher-level `make-forester-project` supplies HTML and accepts peer outputs
through `#:additional-renderers`.

## Smallest Forester site

A one-page HTML site needs an explicit home page. The Forester constructor
requires its home tree ID and supplies the standard reader, analyzer, and HTML
renderer. Omitting the ID or passing an empty string fails when the project is
constructed, before a build starts:

```scheme
;; project.scm
(use-modules (forester project))
(make-forester-project #:home "hello")
```

```scribble
@; plants/hello.plant
@use-modules[(forester commands) (language scribble commands)]

@title{Hello, Grove}

This is a first page.
```

Run `grove check` and then `grove build` from the project directory. The home
ID selects the tree rendered at `index.html`; Grove does not guess which page
should represent the site. `make-site-project` remains available when a project
needs to assemble these pieces itself. A named command vocabulary is optional:
use `@use-commands` to refer to a reusable manifest. Modules already on Guile's
load path need no setup; preload local `.plant` modules discovered in a
directory with `make-scribble-module-setup`. Use
`make-scribble-module-session` when module articles also appear as pages and
their preparation results should be reused for display. Direct
`@use-modules` imports of available modules also need no preparation.

`make-forester-project` always includes its standard HTML renderer. Configure it
with `#:html-options`, and add peer outputs with `#:additional-renderers`:

```scheme
(use-modules (forester project)
             (forester render text))

(make-forester-project
  #:home "hello"
  #:additional-renderers (list (make-text-renderer)))
```

The additional renderers receive the same analyzed Forest and return artifacts
alongside HTML. `#:html-options` takes the usual value from
`(forester render html)`; reuse its routes and site settings when configuring
other outputs such as a sitemap or feed. Forester projects can also transform
Content IR before Forester analysis with `#:transforms` and add cached
site-wide outputs through renderers; see
[site outputs](site-outputs.md).
Pass `#:collect-sources` to replace directory traversal or choose a specific
ordered set of `.plant` paths while keeping Forester's standard reader,
analysis, and renderers. `#:source-directory` still supplies the default
destination for creating new trees. Use `#:extra-sources` for additional files
that should participate in project invalidation and preview watching.
`#:cache?` defaults to `#t`; set it to `#f` to disable document, renderer, and
pipeline caches for this project.

Use `make-site-project` when the project needs a different reader or analyzer,
or otherwise needs to compose the complete site lifecycle.

## Execution and paths

`make-project` connects source collection, preparation, reading, analysis,
and rendering. `make-site-project` assembles page inputs into a site before
calling its analyzer. All renderers receive the same model and return artifacts.
Use `build-project` when a Scheme host only needs the resulting artifact list;
use `build-project/report` when it also needs source changes, affected
artifacts, or cache status. `check-project` returns the same report shape but
does not read or write published output state.
`project-with-options` returns a project copy with an output-directory override,
additional tracked inputs, or a cache setting; the original project is unchanged.
Project constructors snapshot declared path strings and list containers.
Project inputs and callback contexts copy mutable strings, bytevectors, and
pair/list structure when exposed. Other values, including mutable vectors,
records, and procedures, are passed through and may remain shared; projects and
extension types own their mutation semantics. Grove keeps the reader, analyzer,
renderer, collector, and preparation procedures as the project author's
callbacks. Paths returned by collector and preparation callbacks are copied
before Grove reads them or includes them in analysis.

Build reports snapshot their source, artifact, and change lists. Their list
accessors return independent copies, so a caller can annotate or filter a
report without changing the result observed by another consumer. Project
execution errors likewise snapshot the original exception key, arguments, and
source path; the captured stack remains an opaque diagnostic value.
`build-report-render-cache-hit?` reports whether the build reused the complete
renderer result from cache. It does not report whether output files needed to
be restored or written, and a false value does not imply every renderer ran:
some renderers may reuse their outputs while others are invalidated.

Scheme hosts can catch the `grove-project-error` key and inspect the resulting
`project-error` with accessors from `(grove project diagnostic)`. The original
Guile exception key and arguments remain available, alongside Grove's stage,
path, and opaque stack. Use `print-project-error` to preserve Grove's concise
or debug formatting:

```scheme
(use-modules (grove project)
             (grove project diagnostic))

(catch 'grove-project-error
  (lambda () (build-project project))
  (lambda (_ error)
    (print-project-error error #f)
    1))
```

When a host adds its own operation around Grove, `call-with-project-stage`
attaches a stage and optional path without flattening the original Guile
exception. Grove's project callbacks already receive stage context automatically.

The analyzer's `project-context` follows the same value rule: its input list,
source-path strings, and pair-based source values are copied when the context is
made and when accessed. Prepared and extra source lists are also independent
copies. Opaque reader values such as records remain shared and follow the
reader's own contract.

The default collector walks recursively in sorted order, skipping hidden
directories, directory symlinks, and special files; symlinks to regular files
are included. The generic and site project filters skip dot-prefixed
basenames. Forester's project constructor additionally selects `.plant` files,
keeping that format policy in the Forester layer. `(grove collect)` exposes
`collect-directory` for custom collectors;
its optional predicate sees each regular file basename, and by default it
accepts hidden file basenames while still skipping hidden directories and
special files.
`#:source-filter` selects basenames; `#:collect-sources` supplies the ordered
input paths instead of directory traversal. Both options can be supplied:
the collector determines which paths are read, while `#:source-directory`
remains the default destination for `create-project-source`. `#:prepare`
runs before reading and returns additional tracked paths. It can load shared
modules, but does not import their commands into individual articles.
`#:extra-sources` declares other inputs.

CLI paths are relative to the invocation directory. Configured paths are
relative to the project file throughout loading and synchronous execution.
`(grove project load)` restores the caller's directory and evaluates the
configuration in a fresh top-level module. Named Guile modules still have
process-wide lifetime; the working directory is not thread-local.

Without `--project`, commands search the invocation directory and its
ancestors for `project.scm`, git-style, and stop at a `.git` boundary or
the filesystem root; `--project` always wins when given.

`--profile` (build, check, serve) reports where a build's wall-clock and
memory go: the input-cache cost split (reused vs fresh document reads,
with seconds), the project file's own evaluation time, and the process's
peak resident set (VmHWM). Serve passes the flag to its workers, so the
lines land on the serve terminal.

`check-project` fully evaluates, analyzes, renders, and validates without
accessing published output state. It does not sandbox Scheme effects or check
browser layout. Its report treats all sources as added and artifacts as affected.
CLI exit codes are 0 for success, 1 for execution failure, and 2 for invalid
arguments. Summaries use stdout; diagnostics use stderr.

Artifacts carry relative paths, text or bytes, and optional source dependencies.
Validation rejects duplicate, reserved, and conflicting output paths. Writing
preserves unchanged files and removes obsolete manifest-owned artifacts; other
files remain untouched. The configured output root may be a symlink, but
artifact parent directories beneath it may not: Grove refuses to write through
or clean stale outputs through a symlinked parent. Writing is not transactional
across the directory.

A site declares its start tree once with
`(make-site-project #:home ID ...)`. HTML routes
may omit `#:home-page`; if routes are omitted entirely, the default HTML routes
are used. HTML, RSS, and sitemap renderers resolve the index page from the
site's home tree when their routes omit it.
File-backed tree IDs are basenames without `.plant`; directories do not
qualify IDs. Named subtrees share that namespace. See
[composition](forester-composition.md) for tree rules.

`analyze-project` returns the analyzed model after collection, preparation,
reading, and analysis, without rendering or touching published state.
`browse-project` uses it and calls the
project's optional `#:browse` callback with the analyzed model, input, and
output ports. Grove's `grove browse` command drives this path; a Forester
project wires `#:browse (make-forest-browser)` to read the Forest directly in
the terminal instead of reading rendered HTML or Gemtext.

Add `--ssh PORT` (with `--host-key` and `--authorized-keys`) to serve the same
browse callback over SSH. The `(grove ssh)` module is built only when guile-ssh
is available; the launcher then exposes the Guile extension path so an installed
Grove finds it without the development shell, and a build without guile-ssh
simply reports that SSH is unavailable. It accepts only public-key authentication,
never executes a client command, and hands the session channel to the project
callback as its input and output. The server analyzes the project once and
forks a pool of worker processes (four by default), so sessions are served
concurrently and each is capped by an optional per-session timeout; clients
that do not authenticate within the login timeout (default 60 seconds) are
dropped.

Generate the RSA host key with `grove ssh-keygen` (add `--force` to overwrite),
or let the server create it on demand with `--ssh-generate-host-key` when the
`--host-key` file does not exist yet:

```sh
grove ssh-keygen --host-key host_rsa
grove browse --project project.scm \
  --ssh 2222 --host-key host_rsa --authorized-keys authorized_keys
grove browse --project project.scm \
  --ssh 2222 --ssh-generate-host-key \
  --host-key host_rsa --authorized-keys authorized_keys
```

Tune the server with `--ssh-workers N` (concurrency, default 4),
`--ssh-login-timeout S`, `--ssh-session-timeout S` (0 disables), and
`--ssh-memory-limit BYTES` (each worker's address-space cap).

Add a Gemtext renderer to `#:additional-renderers` in a Forester project, or
include it in the complete `#:renderers` list passed to `make-site-project`:

```scheme
(use-modules (forester render gemtext))

(make-gemtext-renderer #:prefix "gemini")
```

This writes `index.gmi` and one `.gmi` file per page below the project's output
directory. The linked-notes example includes this renderer; Grove's `serve`
command remains an HTTP preview server and does not implement the Gemini
protocol.

## Dependencies and caching

Projects and document readers cache by default, as do Grove's built-in
renderers. Plain renderer procedures run on every build because a procedure
does not have a stable cache identity. Wrap a renderer with
`make-cached-renderer` to opt into artifact reuse. Set `#:cache? #f` to disable
the project caches; `project-with-options` applies this to any project,
including Forester projects. Both `make-site-project` and
`make-forester-project` accept it directly. Readers accept a path predicate for
finer control.
Preparation and whole-project analysis still run. Document hits reuse Content
IR; renderer hits reuse artifacts. Shared code changes conservatively force
full rendering. Standalone reader calls evaluate normally; `check` bypasses
published caches.

Each renderer has its own cache identity. A plain procedure, a renderer that
opts out with `#:cache? #f`, or one with opaque rules but no stable
`#:rules-id` reruns on each build; cacheable peers can still reuse their own
artifacts. A changed model input can still require the full renderer set to
run, since renderers receive the same analyzed model.

Project files, imported modules, loader dependencies, framework source/data,
compiled files, and effective renderer settings are fingerprinted automatically.
Module search paths and absent preferred compiled files are also tracked.
Built-in factories include their settings in cache identity. A custom cached
renderer should declare extra files with `#:inputs` (a fixed path list or a
zero-argument collector); each returned path must be unique. Declare any
output-affecting state not represented by the model or tracked project files
with `#:configuration`. Plain procedures avoid this declaration by running on
every build. Opaque render rules need a stable
`#:rules-id`.
For the default HTML renderer installed by `make-forester-project`, pass this
as `#:html-rules-id`; use `#:extra-sources` for files those rules read.

Asset pipelines fingerprint the project file, its load-time inputs, declared
files, and Scheme files loaded while the pipeline runs. A self-contained
pipeline can omit `#:inputs`. Set its `#:version` when behavior depends on
external toolchain state, environment variables, or runtime arguments Grove
cannot inspect. Pipeline artifacts and build reports retain the file inputs
and loaded Scheme dependencies, so cache invalidation and affected-artifact
reporting use the same dependency set.

Arbitrary Scheme I/O is not intercepted. Declare a file before reading it:

```scheme
(call-with-input-file (depend-on! "data.txt") get-string-all)
```

`depend-on!` and `disable-cache!` are both exported by `(grove input)`. Use
`depend-on!` for files discovered during evaluation: Grove records those reads
and checks the recorded files
before reusing that document. A reader's `#:cache-dependencies` collector is
for reader-wide files that may affect any document even when its evaluation
does not open them; it runs before each document cache lookup. Time,
randomness, network state, and effects that must repeat require opting out
with `disable-cache!` or disabling reader caching. An opted-out document also
forces the renderer set to run for that build, because its untracked effects may
change the analyzed model; opting out one renderer still allows its cacheable
peers to reuse artifacts. Imported module definitions remain uncached so their
bindings exist in every build process.

Cached renderers and pipelines are assumed deterministic for their tracked
inputs and declared configuration. Grove verifies cached bytes against their
content hashes, but cannot verify that a callback or external tool repeats the
same output. A plain renderer procedure is the safe default and reruns on each
build; use `make-cached-renderer` when avoiding that work is worth declaring a
stable cache identity.

Document entries live in the per-user shared content store,
`$XDG_CACHE_HOME/grove/store/input-cache` (override the base with
`GROVE_STORE_DIR`), shared by every checkout and output directory of
every project this user builds. Keys are content-derived from the project's
load-time inputs plus the project-relative document path; tracked paths are
normalized project-relative, so two checkouts of one project share document
analyses. Content-addressed blobs join the same store under `artifacts/`:
rendered outputs, pipeline products, and bytes stored explicitly by extensions
all use the same verified blob store. Builds restore missing output files from
it instead of re-rendering them, so deleting a build directory is cheap and
parallel checkouts share output bytes. The build state
and artifact manifest live in a per-output directory keyed by the
output path, so the output directory stays deployable as-is. Every
build process that targets an output directory — manual builds and
serve workers alike — uses the same cache and store locations, so a
manual build over a serve-written output stays incremental (the build
lock serializes the writers). `guix shell --container --pure` runs
with a throwaway home, so point `GROVE_STORE_DIR` and `GROVE_CACHE_DIR`
at a host directory when caching should survive the shell.

`grove store size` reports entry counts and byte totals per store
subtree; `grove store gc [--dry-run] [--older-than=N]` prunes entries
whose mtime is older than N days (default 30) and sweeps directories
left empty. Every store subtree is a pure content-addressed or
content-verified cache, so pruning is always safe — it can only cost
recomputation. The BLUE content store lives outside this tree by
default; point `BLUE_CONTENT_STORE_DIR` inside `GROVE_STORE_DIR` to
have `grove store` cover it too. Only
data-only Content IR is
saved, including styles and source locations. Closures, unsupported records,
and cyclic data are re-evaluated. Corruption is a miss; successful builds
prune unselected entries. Declared inputs also participate in rendering
invalidation and file watching.

## Executable source and module articles

The Scribble reader preserves ordinary Scheme syntax, including dotted pairs,
rest arguments, quoted data, and macros using `...` or `#'`. Content source
annotations apply to explicit at-commands rather than every nested Scheme pair.

A document (`.plant`) evaluates each top-level `@(...)` form in a page
environment, so a bare `@(define (helper) …)` at the top level binds
`helper` for every later form in that page — no `begin` wrapper is needed.
Definitions inside a command argument still follow Scheme's definition-context
rules and are rejected; writing them inside a subtree does not turn them into
top-level bindings. Page helpers that grow, or that several pages share,
belong in a `.plant` module article loaded through `#:prepare` with
`make-scribble-module-setup`.

`(language scribble source)` exports `source-text` and `source-range`. Both take
an alist from `source-properties` (parsed data) or `syntax-source` (macro input).
For example, a macro can call `(source-text (syntax-source #'definition))` to
show the same definition it expands and executes. Ranges are half-open, with
one-based lines/columns and character offsets relative to the reader input.
Columns follow Guile's tab handling; offsets remain suitable for `substring`.
Internal comments and whitespace are included; comments outside the expression
are not. An escaped expression's text includes its authored `@` prefix.

Parsed compound expressions retain ranges. Guile strips custom properties from
syntax objects, so macro range lookup uses the active document evaluation's
range table. Obtain text/ranges during expansion and keep those returned values
if needed later. Unavailable ranges return `#f`; files are never reopened and
there is no global source cache. Not all Guile reader extensions are supported.

`make-scribble-module-session` returns a preparation procedure and a reader:

```scheme
(define-values (prepare read-document)
  (make-scribble-module-session (list "plants/interpreter.plant")))
```

Pass `prepare` as the project's `#:prepare` and use `read-document` in its page
reader. Module articles execute once during preparation; their document results
are reused for display. Preparing again resets the session and refreshes bindings.
Imports retain their actual `.plant` module filenames, and dependencies captured
while preparing are replayed when a document is reused. Normal imported-module
tracking therefore works without making every article depend on every module.
Supply module paths in dependency order; automatic sorting, cycle analysis, and
standalone Guile autoloading of `.plant` modules are not provided.

## Preview and progress

`grove serve` binds localhost and starts only after a successful build. Each
build uses a fresh process. Edits observed during a build queue the next one;
failure preserves the last successful in-memory artifact set. HTTP serves
only those artifacts, supports GET/HEAD, disables browser caching, and exposes
neither project files nor cache metadata. The browser does not refresh itself.
Scheme hosts can call `serve-project` from `(grove serve)` with a project-file
path and the same output, port, watch, debug, and profile options. The call
blocks while the preview server runs; the command is the simpler entry point
for ordinary local use.

The watcher polls identity, size, and precise timestamps. It observes project
and source directories, declared dependencies, loaded sources, and discovered
external input directories. Recursive scans skip hidden entries and nested
directory symlinks; explicit source-directory symlinks are resolved. Outputs
are excluded from watching and should live in a separate directory. Store
paths and compiled artifacts (`.go` files, grove's compiled tree) are excluded
too: they change behind builds and load-path resolutions, so watching them
would requeue rebuilds no project edit caused. Grove source modules stay
watched; editing one restarts the worker on purpose.

Ctrl-C and SIGTERM stop the server and active worker process group, returning
130 and 143 respectively. Detached project processes are outside that group.
Teardown owns the whole group: a worker that dies mid-build takes the project
subprocesses it spawned down with it. A serve killed outright (SIGKILL) runs
no teardown at all - the worker's watchdog notices the dead serve and tears
the group down itself, so a hard-killed session leaves no resident builder or
subprocess behind.
The worker retains the analysis model across edits: document edits (the
source set, and anything else analysis read) re-analyze, while load-time
assets and undeclared render-time reads only re-render against the retained
model.
`--no-watch` serves the initial successful snapshot only.

Scheme runners are silent unless given `#:progress`, a `(stage detail)` callback.
Stages are `collect`, `sources`, `prepare`, `read`, `read-done`, `input-cache`,
`analyze`, `render`, `render-renderer`, `validate`, and `write`. `sources`
carries a count, read events carry a path, and `write` carries the output
directory. `input-cache` carries `(hits . total-inputs)` when the project read
at least one input. Each `render-renderer` event carries `(index seconds
artifact-count)`, with a zero-based renderer index and elapsed time in seconds.
Other details are `#f`. A complete build-cache hit skips both render events.
CLI formatting uses the same events.

## Tree creation

`grove new` delegates source creation to the project's `#:create-source`
procedure. Grove does not choose a source format or template. A generic project
may omit this callback; `grove new` then reports that creation is unavailable.
The procedure receives the optional positional prefix and destination and
returns the path to print. This keeps naming and file-format rules with the
project that owns them. Scheme callers can invoke the same policy with
`create-project-source` from `(grove project)`; omitting `#:destination` uses
the project's source directory.

Forester provides that policy in `(forester project new)`. The default
constructor installs it automatically; advanced `make-site-project` users can
install it directly:

```scheme
(use-modules (forester project new))

(make-forester-source-creator
  #:source-directory "plants"
  #:default-prefix "note"
  #:template
  (lambda (id date)
    (string-append
      "@use-modules[(forester commands) (language scribble commands)]\n\n"
      "@title{}\n"
      "@published{" date "}\n")))
```

The creator scans the source and destination directories for existing `.plant`
IDs, allocates a four-digit lowercase base36 suffix, and uses exclusive file
creation to avoid overwrites. Its default template imports
`(forester commands)`, leaves the title empty, and adds today's publication
date. A custom template receives the allocated ID and local date and must return
UTF-8 source text. Imports used by each article belong in the template;
`#:prepare` loads modules for the build but does not import them into articles.

## Highlighting

Code blocks accept a language symbol: `@code-block[#:language 'scheme]{(define x 2)}`.
Scheme highlighting is included by default in the Nix and Guix development
environments and the installed CLI. No project configuration is needed for
`#:language 'scheme`.
To replace the defaults, configure the HTML renderer with `#:highlighters`, an
association list of language symbols and values created by `(grove highlight)`:

```scheme
(define scheme-highlight
  (make-highlighter "/absolute/path/scheme.so" "tree_sitter_scheme"
    "(symbol) @variable (number) @number"))
(make-html-renderer
  #:options
  (make-html-options
    #:routes routes
    #:highlighters (list (cons 'scheme scheme-highlight))))
```

Import `(grove highlight)` alongside `(forester render html)`. Configure
`render-html`, `site->html`, and `make-html-renderer` through
`make-html-options`; Grove's `content->sxml` accepts `#:highlighters` directly.
Use `#:highlighters '()` to disable the defaults, or combine your mapping with
`(default-highlighters)` to keep Scheme support. Without a configured
highlighter, a named language produces a warning and plain code. Blocks without
a language remain plain code without a warning.

Forester's default style includes `highlight.css`. Custom styles can compose
`default-highlight-style` from `(grove style highlight)` through
`compose-html-styles`. Later stylesheets can select `[data-syntax="keyword"]`
or set variables such as `--syntax-keyword` on `pre`. Capture names are preserved
verbatim, for example `data-syntax="function.builtin"`. Select a category and
its subcategories with `[data-syntax="function"], [data-syntax^="function."]`.
The code tag carries `data-language="scheme"`; class remains available for
author styles. Text output preserves the
source without highlight markup. The `grove-highlights` Nix package includes
the grammar, its matching upstream query, and the grammar's license; their
version is fixed by `nix/flake.lock`.

## HTML presentation

The HTML renderer accepts presentation options instead of requiring projects to
rewrite rendered markup. They are available on `render-html`, `site->html`, and
`make-html-renderer`. The presentation carries static, composable presentation data:

```scheme
(make-html-options
  #:presentation
  (make-html-presentation
      #:head '((link (@ (rel "alternate") (href "/feed.xml"))))
      #:site-nav (list (make-html-nav-link "about" "About"))
      #:home-link (make-html-home-link '("Home")
                    #:attributes '((class . "brand")))
      #:title-decorations
      '(("about" . ((span (@ (class "taxon")) "guide. "))))
      #:metadata-items
      '(("about" . ((li (@ (class "meta-item")) "Author"))))))
```

- `#:html-attributes` — an association list merged onto the root `<html>`
  element, for example `'((lang . "zh-Hans"))`.
- `#:head` — static SXML nodes placed in `<head>` after the description and
  before the stylesheets. Use it for viewport metadata or feed autodiscovery.
- `#:site-nav` — static SXML nodes appended inside the site `<nav>`; use
  `make-html-nav-link` for links whose route and current-page state Grove should
  resolve.
- `#:home-link` — a `make-html-home-link` value replaces the default link while
  Grove supplies the route. Its content and attributes can add icons or
  branding without a callback.
- `#:title-decorations` — a page-ID-to-SXML map inserted before titles in tree
  headings and the table of contents. Resolve page metadata while constructing
  the presentation (for example, `(page-taxon page)`), then store the resulting
  static markup by page ID.
- `#:metadata-items` — a page-ID-to-SXML map appended after the built-in date
  items in the metadata list.
- `#:article?` — wraps the page tree and its backmatter in one `<article>`
  element, leaving the table of contents outside it.
- `#:side-lane` — a list of content element kinds (e.g. `(sidenote)`).
  Elements of these kinds are lifted out of the text flow into a
  `<div class="margin-lane">` beside the page tree; each note becomes a
  lane item with a numbered back-reference, and its anchor position keeps
  a superscript marker. Pair with grid CSS that places the lane beside
  the reading column; the option participates in the renderer's cache
  configuration. Kinds match the content element's `kind`, which
  `make-element` defaults to `span` — declare `#:kind 'sidenote` on the
  element definition (or build the element with `make-content-element`)
  so the lane can see it.
- `#:backmatter-details?` — renders each backmatter section as a collapsible
  `details`/`summary` block.

Static SXML in a presentation participates in its cache identity, so changing
it invalidates cached pages without requiring a separate identity token. The
SXML must be well-formed: procedures, duplicate attributes, and repeated page
IDs are rejected when constructing the value. Composition appends head and
navigation nodes, appends page fragments by page ID, and lets the rightmost
declared home link replace earlier ones.

## Packaging and measurement

The installed launcher records canonical Guile source/compiled dependency
paths, not the entire devshell path. Installation tests use an empty environment.
The packaged `(grove highlight languages)` table registers grammar/query paths;
other packagers can provide the same table. `GROVE_TEST_SCHEME_GRAMMAR` selects
a different grammar library for tests. The Nix bundle validates the matching
query against the actual highlighter during packaging.

Document reader `#:timings` callbacks receive `(path phase seconds)` for
`parse`, `translate`, `evaluate`, and `decode`. `make-forest` callbacks receive
`(phase seconds)` for extraction, references, content facts, composition, and
query resolution. Use `statprof` for evaluation hotspots. Compare cold and warm
builds in separate processes without recompiling during measurement.

For coverage, build first, then run:

```sh
GROVE_TESTS_ENABLE_COVERAGE=1 blue check
```

Coverage runs default to a 900-second suite limit; `GROVE_TESTS_TIME_LIMIT`
overrides it.

The vector identity suite may need a separate 1200-second tracing run. Blue
compiles tests before tracing; suites write LCOV `.cov` files beside their logs.
CLI/preview subprocesses write `build/coverage-child-*.cov`. Merge by source
and line using only the current run's files. Killed workers cannot flush reports.
Coverage counts traced compiled code, not every repository line; untraced module
initialization and unloaded modules are outside its denominator. Provenance tests
must supply reader locations explicitly: compiled strings do not retain them.
