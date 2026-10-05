<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Project reference

For setup and CLI usage, see the [README](../README.md). The project file's
final expression returns a project; it must not run a build itself. Use
[`examples/linked-notes/project.scm`](../examples/linked-notes/project.scm)
for a site configuration, or the [catalog](../examples/catalog/project.scm)
and [inventory](../examples/inventory/project.scm) for project-owned models.

## Execution and paths

`make-project` connects source collection, preparation, reading, analysis,
and rendering. `make-site-project` assembles page inputs into a site before
calling its analyzer. All renderers receive the same model and return artifacts.

The default collector walks recursively in sorted order, skipping hidden
directories. `#:source-filter` selects basenames; `#:collect-sources` supplies
an ordered path list instead. `#:prepare` runs before reading and returns
additional tracked paths. It can load shared modules, but does not import their
commands into individual articles. `#:extra-sources` declares other inputs.

CLI paths are relative to the invocation directory. Configured paths are
relative to the project file throughout loading and synchronous execution.
`(grove project load)` restores the caller's directory and evaluates the
configuration in a fresh top-level module. Named Guile modules still have
process-wide lifetime; the working directory is not thread-local.

`check-project` fully evaluates, analyzes, renders, and validates without
accessing published output state. It does not sandbox Scheme effects or check
browser layout. Its report treats all sources as added and artifacts as affected.
CLI exit codes are 0 for success, 1 for execution failure, and 2 for invalid
arguments. Summaries use stdout; diagnostics use stderr.

Artifacts carry relative paths, text or bytes, and optional source dependencies.
Validation rejects duplicate, reserved, and conflicting output paths. Writing
preserves unchanged files and removes obsolete manifest-owned artifacts; other
files remain untouched. Writing is not transactional across the directory.

Site HTML routes require an explicit `#:home-page`. File-backed tree IDs are
basenames without `.plant`; directories do not qualify IDs. Named subtrees
share that namespace. See [composition](forester-composition.md) for tree rules.

## Dependencies and caching

Projects, document readers, and built-in renderers cache by default. Set
`#:cache? #f` to opt out; readers also accept a path predicate. Preparation and
whole-project analysis still run. Document hits reuse Content IR; renderer
hits reuse artifacts. Shared code changes conservatively force full rendering.
Standalone reader calls evaluate normally; `check` bypasses published caches.

Project files, imported modules, loader dependencies, framework source/data,
compiled files, and effective renderer settings are fingerprinted automatically.
Module search paths and absent preferred compiled files are also tracked.
There are no user-maintained cache keys or version numbers.

Arbitrary Scheme I/O is not intercepted. Declare a file before reading it:

```scheme
(call-with-input-file (depend-on! "data.txt") get-string-all)
```

`depend-on!` and `disable-cache!` are exported by `(grove input scribble)`.
A reader's `#:cache-dependencies` collector can declare inputs in bulk.
Time, randomness, network state, and effects that must repeat require opting
out with `disable-cache!` or disabling reader caching. Imported module
definitions remain uncached so their bindings exist in every build process.

Plain renderer callbacks are assumed deterministic under tracked inputs.
`make-cached-renderer` adds `#:inputs` and `#:configuration` (data or a
model-to-data procedure), or disables caching. Grove cannot infer hidden
closure state. Built-in factories declare their settings themselves.

Document entries live in `OUTPUT/.grove-input-cache`. Only data-only Content IR
is saved, including styles and source locations. Closures, unsupported records,
and cyclic data are re-evaluated. Corruption is a miss; successful builds prune
unselected entries. Declared inputs also participate in rendering invalidation
and file watching.

## Executable source and module articles

The Scribble reader preserves ordinary Scheme syntax, including dotted pairs,
rest arguments, quoted data, and macros using `...` or `#'`. Content source
annotations apply to explicit at-commands rather than every nested Scheme pair.
Definitions inside a command argument still follow Scheme's definition-context
rules; writing them inside a subtree does not turn them into top-level bindings.

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

## Preview and progress

`grove serve` binds localhost and starts only after a successful build. Each
build uses a fresh process. Edits observed during a build queue the next one;
failure preserves the last successful in-memory artifact set. HTTP serves
only those artifacts, supports GET/HEAD, disables browser caching, and exposes
neither project files nor cache metadata. The browser does not refresh itself.

The watcher polls identity, size, and precise timestamps. It observes project
and source directories, declared dependencies, loaded sources, and discovered
external input directories. Recursive scans skip hidden entries and nested
directory symlinks; explicit source-directory symlinks are resolved. Outputs
are excluded from watching and should live in a separate directory.

Ctrl-C and SIGTERM stop the server and active worker process group, returning
130 and 143 respectively. Detached project processes are outside that group.
`--no-watch` serves the initial successful snapshot only.

Scheme runners are silent unless given `#:progress`, a `(stage detail)` callback.
Stages are `collect`, `sources`, `prepare`, `read`, `read-done`, `analyze`,
`render`, `validate`, and `write`. `sources` carries a count, read events carry
a path, and `write` carries the output directory; other details are `#f`.
Complete renderer hits skip `render`. CLI formatting uses the same events.

## Tree creation

`#:default-prefix` supplies the prefix for `grove new`. Explicit prefixes
contain lowercase ASCII letters, digits, and internal hyphens. IDs use lowercase
base36 with at least four digits. Creation takes the largest matching source
suffix plus one, without filling gaps; deleting the maximum can reuse its number.
Destination files are scanned too, and exclusive creation never overwrites them.

The default destination is `source-directory`; collector-only projects require
`--dest`. It must exist. Creation evaluates configuration but not article
readers, preparation, analysis, or renderers. It prints the absolute path.
The default template imports `(forester commands)` and includes an empty title
and today's publication date. Fill in the title before building.

A project can supply `#:tree-template`, a procedure receiving the allocated ID
and the local date as strings and returning the complete UTF-8 source text:

```scheme
#:tree-template
(lambda (id date)
  (string-append
    "@use-modules[(flora commands) (language scribble commands)]\n\n"
    "@title{}\n"
    "@published{" date "}\n"))
```

The example is configuration for a consumer that provides `(flora commands)`;
Grove does not depend on that module. The callback can use the ID in the source,
add metadata, or return text loaded from a project template file. `#:prepare`
loads project modules for later imports; it does not import them into each
article. Consequently, a template must include the imports its commands need.
Template errors or non-string results fail before creating a file.

## Highlighting

Code blocks accept a language symbol: `@code-block[#:language 'scheme]{(define x 2)}`.
Scheme highlighting is included by default in the Nix devshell and installed
CLI. No project configuration is needed for `#:language 'scheme`.
To replace the defaults, configure the HTML renderer with `#:highlighters`, an
association list of language symbols and values created by `(grove highlight)`:

```scheme
(define scheme-highlight
  (make-highlighter "/absolute/path/scheme.so" "tree_sitter_scheme"
    "(symbol) @variable (number) @number"))
(make-html-renderer #:routes routes
  #:highlighters (list (cons 'scheme scheme-highlight)))
```

Import `(grove highlight)` alongside `(forester render html)`. The same option
is available on `render-html`, `site->html`, and Grove's `content->sxml`.
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
GROVE_TESTS_ENABLE_COVERAGE=1 GROVE_TESTS_TIME_LIMIT=600 blue check
```

The vector identity suite may need a separate 1200-second tracing run. Blue
compiles tests before tracing; suites write LCOV `.cov` files beside their logs.
CLI/preview subprocesses write `build/coverage-child-*.cov`. Merge by source
and line using only the current run's files. Killed workers cannot flush reports.
Coverage counts traced compiled code, not every repository line; untraced module
initialization and unloaded modules are outside its denominator. Provenance tests
must supply reader locations explicitly: compiled strings do not retain them.
