<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Hacking on Grove

This guide is for people changing Grove or building a new project on its
interfaces. User-facing setup and examples are in the [README](README.md).

## Development environment

From the repository root:

```sh
nix develop ./nix
```

The development shell provides Guile, Blue, and the project dependencies. If
you are already in that shell, run the commands directly:

```sh
blue check
reuse lint
```

Run one test file while iterating:

```sh
blue check tests/unit/grove/content.scm
```

For a sequential full test run, Blue options go before the command:

```sh
blue -j 1 check
```

Build a runnable example from the repository root with
`grove build --project examples/linked-notes/project.scm --output /tmp/grove-preview`.
Examples are integration fixtures as well as demonstrations; keep them small
and focused on a feature. Flora is the place for longer teaching examples.

## Project flow

The generic project runner knows about paths, values, and artifacts. It does
not require Grove content records or Forester pages.

```text
sources → prepare hook → reader → inputs → analyzer → model → renderers → artifacts → writer
```

The reader and analyzer are project callbacks. Each renderer consumes the
same analyzer result and returns artifacts. Shared artifact validation runs
before writing; `check-project` uses the same execution flow with full rendering
and no output-state access. CLI loading lives in `(grove project load)`, with
argument handling and diagnostic presentation in `(grove cli)`. The loader
scopes the working directory for synchronous execution; it is not a
thread-local path environment. The writer updates output files
and the artifact manifest. See the [project guide](docs/project.md) for a
minimal setup and renderer options.

The current project I/O contract uses local paths. Artifacts can contain text
or bytes. Source files, preparation paths, extra sources, and artifact
dependencies are tracked by path. A renderer cache hit reuses artifacts, but
the build still prepares modules and runs whole-project analysis. Document
results and renderer outputs are cached by default. Module source/compiled
files, loader dependencies, and effective renderer settings supply automatic
fingerprints; external effects must declare inputs or opt out.

The CLI template includes the installed Grove library directories and the
canonical source and compiled directories of its runtime dependencies. Resolve
those directories from the build environment, rather than recording the whole
devshell path. The launcher test runs with an empty environment to catch
accidental dependence on development load paths or a user compilation cache.

Preview execution lives in `(grove serve)`. A single event loop supervises a
fresh build process, polls source changes, and handles local HTTP requests.
The private preview modules separate file observation, HTTP artifact handling,
and worker execution. Workers use the same project loader and
`build-project/report`; their file protocol transfers artifacts and dependency
metadata, not a new public project representation. HTTP reads the successful
in-memory artifact set rather than files being rewritten. The build report's
`build-report-sources` accessor provides the complete tracked input paths.

The preview integration suite opens localhost sockets and exercises module
reloads, failure recovery, queued edits, and process cleanup. It needs an
environment that permits local socket creation.

## Module boundaries

Grove owns generic project orchestration, content records, rendering helpers,
artifact writing, and relational query primitives. Forester is a library on
top of Grove: it adds tree IDs, links and backlinks, inline subtrees,
transclusion, content queries, and site renderers. Keep domain policy in
Forester and generic contracts in Grove.

Forester structure is tree composition. Titles are tree properties; renderer
nesting determines their HTML levels. Keep tree identity separate from local
display anchors. Root documents and subtree bodies share the paragraph decoder
in `(language scribble flow)`.

The Grove content IR is defined in `(grove ir)`. It has text and layout leaves,
elements with open symbol kinds and properties, and root documents. Properties
carry data for domain libraries; the core IR does not assign meaning to them.
Renderers interpret element kinds. A host can give a generic disclosure
additional behavior: Forester promotes disclosures marked addressable into
indexed trees at its site boundary.

The math interfaces live in separate modules:

- `φ:` is the Formula facade. It reads text or combines Formula syntax with
  Semantic values.
- `◆:` constructs semantic values for computation. Calculation passes return
  reports, derivations, and diagnostics rather than rendered content.
- `◇:` builds display notation and MathML structure. It does not assign
  computational meaning to the notation.

Formula and Scribble commands choose how reports become document content.
Keep computation, notation, and document presentation separate when adding a
math feature. The [math guide](docs/math.md) describes supported operations
and authoring forms.

The relation API in `(grove relation)` provides finite relations and
miniKanren goals. Domain libraries own their schemas and predicates. Forester's
page and content queries are documented in the
[relations guide](docs/relations.md).

## Where changes belong

| Area | Main location |
| --- | --- |
| Generic project lifecycle and artifact tracking | `modules/grove/project/`, `modules/grove/build/`, `modules/grove/output.scm` |
| Content records and renderers | `modules/grove/ir.scm`, `modules/grove/render/` |
| Forester page analysis and queries | `modules/forester/` |
| Math syntax, semantics, passes, and notation | `modules/grove/math/` |
| Project and feature tests | `tests/unit/` |
| Runnable project examples | `examples/` |

Prefer a small public interface with one clear owner. Do not add a generic
extension point for a single example; promote it when a real second consumer
needs it. Preserve observable ordering and error behavior when simplifying
tree walks or query code.

## Making and checking a change

Before editing, find the owning module and its tests. Add tests for changed
public behavior, including failure cases when the change affects validation
or diagnostics. Keep fixtures readable and use the existing independent
oracles for mathematical transformations instead of reusing the implementation
under test.

After editing:

1. Run the focused test file with `blue check tests/unit/...`.
2. Run `blue check` (or `blue -j 1 check` if the parallel runner conflicts
   over a fixture).
3. Run `reuse lint` when files or licensing metadata change.
4. Build the affected example when a user-facing command or authoring form
   changes.

Documentation follows the same ownership boundaries. Keep the README short
and navigational, put developer architecture here, and keep detailed behavior
with the relevant [project](docs/project.md), [math](docs/math.md),
[relations](docs/relations.md), or
[composition](docs/forester-composition.md) guide. Update examples when a
documented form changes.

To measure document reading, pass `#:timings` to
`make-scribble-document-reader`. Its callback receives `(path phase seconds)`
for `parse`, `translate`, `evaluate`, and `decode`. Timings are disabled by
default. Use Guile's `statprof` to locate hotspots inside evaluation.

For forest analysis, `make-forest` accepts `#:timings` with a callback receiving
`(phase seconds)`. It measures tree extraction, references, content facts,
composition, and query resolution separately. Measure cold and warm builds in
separate processes; do not recompile files while measuring cache reuse.

For line coverage, build first, then run
`GROVE_TESTS_ENABLE_COVERAGE=1 GROVE_TESTS_TIME_LIMIT=600 blue check`.
The patched Blue compiles test files before tracing their execution. Each suite
writes an LCOV `.cov` file beside its test log. CLI and preview tests also write
`build/coverage-child-*.cov` for their child processes, including completed
preview workers. Merge these records by source file and line, using only files
from the current run. Cancelled or killed workers cannot flush a report.
Coverage counts executed compiled code; module initialization outside tracing
and modules never loaded are not part of its denominator.
