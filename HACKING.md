<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Hacking on Grove

Start with the [README](README.md) for setup and usage. This file describes the
principles to preserve when changing Grove; people and coding agents use the
same development guidance.

## Design

Grove owns generic project execution, content, artifacts, and relation
primitives. Forester adds tree identity, composition, references, queries, and
site presentation. Keep writing policy in Forester and project-independent
behavior in Grove.

A project reads sources, analyzes the resulting values, and passes one model
to its renderers. The model can be any Scheme value. Output validation is shared
between building and checking. Caching must preserve this behavior, not create
a second execution model.

Document structure consists of trees, not a separate heading or section
hierarchy. Titles belong to trees. Keep durable tree IDs distinct from local
HTML anchors. Root and subtree prose use the same paragraph decoder.

Give each pass and IR a clear responsibility. Semantic math carries meaning;
Notation carries layout. Formula is an optional frontend producing Semantic
values, not an alternative calculation engine. Calculations return data and
reports; document commands decide how to present them.

Prefer small interfaces and remove redundant representations. Do not merge
abstractions merely because their fields overlap: different responsibilities
can require different IRs. Avoid adding a generic extension point for a single
example. Preserve ordering, diagnostics, and source provenance when refactoring.

## Make a change

Find the owning module and existing tests before editing. Test observable
behavior and meaningful failures, rather than duplicating the implementation.
Mathematical checks should use independent oracles where available.

Enter the development shell with `nix develop ./nix` or
`guix shell --container --pure -m manifest.scm`. The two environments
track the same dependency set; the nix flake additionally builds
Racket-era tooling the Guix manifest omits. The container is required,
not cosmetic: when the host already exports GUIX_LOCPATH or LANG, a
plain `guix shell` inherits them and Guile cannot install a UTF-8
locale, which mangles multilingual test fixtures. Inside the
development shell:

```sh
blue build
blue check tests/unit/grove/content.scm  # choose the relevant suite
blue check
reuse --no-multiprocessing lint
```

Each suite runs under a wall-clock limit (two minutes by default; set
`GROVE_TESTS_TIME_LIMIT` to override), and blue runs the suites in parallel by
default. The subprocess-heavy suites — preview, store sharing, ssh, cli — are
sensitive to that combination: full parallel `blue check tests/unit` runs can
kill suites at the limit and surface nondeterministic module-resolution errors
in spawned guile subprocesses, cascading into dozens of false failures. A
suite run alone, or the whole tree with `blue -j 1 check tests/unit`, is the
authoritative signal; suspect the harness before the code, and reach for
`-j 1` when diagnosing interference between parallel fixtures.

The module-resolution errors have a known mechanism. Blue forks each suite
subprocess out of its multi-threaded fibers scheduler, and on the Guile 3.0.11
this project pins, that fork runs under a GC-disable workaround that blue
itself documents as undefined behavior; suite children can therefore die at
startup (empty logs, empty suite error messages) or fail resolving modules
they resolved in earlier runs — with a different suite set each time. Which
suites are hit is random; the application code is not involved. Set
`GROVE_TEST_CHILD_DEBUG=1` to make every spawned subprocess dump its load
paths under `/tmp` for comparison, and re-run the failing suite alone: it
passes.

Replacing blue's fork with Guile's `spawn` (the documented multi-threaded
option) does not work here, and a patch attempt was reverted after testing:
`apply-in-subprocess` passes the parent's module state to the child by
inheritance, not by re-execution. The suite child relies on blue's redefinition
of `primitive-load-path` (installed in the root module while the blueprint
loads) to resolve modules; `spawn` starts a fresh Guile, which loses that
redefinition and fails on the first module lookup. A working fix would have to
make blue's module resolution independent of in-process state — upstream work,
not a local patch. `popen`'s non-test users (`blue build`'s external commands)
tolerate `spawn` fine; the obstacle is specifically `apply-in-subprocess`.

Profile the polling watcher against a Flora-sized synthetic project (without
starting a server or touching Flora):

```sh
guile --no-auto-compile -L modules -C build tests/perf/serve-watch.scm
```

Build before testing CLI changes: subprocess tests load compiled modules.
The preview tests require permission to bind localhost sockets. Use
`blue -j 1 check` when diagnosing interference between parallel fixtures.
For installation changes, verify the installed launcher in an empty environment.

Keep repository examples small and focused. Flora is a separate consumer and
hosts the longer learning material; changes here should not silently rewrite it.
Update documented behavior when public interfaces change. Prefer logical,
reviewable commits with Conventional Commit titles.

## Technical references

The references below hold detailed contracts and implementation constraints.
Keep a fact in one place and link to it rather than repeating it here.

| Reference | Scope |
| --- | --- |
| [Projects](docs/project.md) | Configuration, dependencies, caching, preview, templates, highlighting, and measurement. |
| [Tree composition](docs/forester-composition.md) | Identity, transclusion, backmatter, and styling. |
| [Internationalization](docs/i18n.md) | Locales, message catalogs, and bilingual content. |
| [Relations](docs/relations.md) | Fact schemas, query semantics, and deferred queries. |
| [Mathematics](docs/math.md) | Frontends, assumptions, calculations, and verification boundaries. |

The corresponding implementations live in `modules/grove/`,
`modules/forester/`, and `modules/language/scribble/`; behavior tests live in
`tests/unit/`. Consult the owning module for exact signatures rather than
maintaining a second API inventory in prose.
