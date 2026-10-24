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
track the same development tools, including optional Typst, Racket, and
TeX Live tools for the hackable math example. Those compilers are not Grove
runtime dependencies. The Guix manifest also exposes a pinned SMathML
collection to Racket for that example. The manifest exposes locale data
through `GUIX_LOCPATH`, including in a pure shell, so locale-aware tests do
not depend on the host's locale setup. Inside the development shell:

```sh
blue build
blue check tests/unit/grove/content.scm  # choose the relevant suite
blue check
blue verify
```

`blue verify` runs `blue check`, `blue lint`, and `blue check-examples` in
order. `blue check-examples` builds the five audited examples and runs
axe-core; `blue a11y [HTML ...]` audits explicit pages; `blue lint` runs
`reuse --no-multiprocessing lint`. The audit needs Chromium: it uses
`CHROMIUM` if set, otherwise `chromium` on `PATH`, then a
`/gnu/store/*chromium*` match. The first run downloads pinned axe-core 4.10.2
into `${XDG_CACHE_HOME:-$HOME/.cache}/grove` and verifies its SHA-256, so it
needs network access once.

Each suite runs under a wall-clock limit (two minutes by default; set
`GROVE_TESTS_TIME_LIMIT` to override), and blue runs the suites in parallel by
default. When a full run reports failures, re-run the failing suite alone
before looking at the code; `blue -j 1 check tests/unit` separates harness
interference from real failures. `GROVE_TEST_CHILD_DEBUG=1` makes every
spawned subprocess record its load paths under `/tmp` for comparison.

The tree pins blue with patches under `nix/pkgs/blue/`, applied by both
`manifest.scm` and the Nix package. Two are load-bearing for the harness.
`blue-sandman-shutdown.patch` keeps a single reader on the scheduler's
self-pipe; without it, shutdown can hang after the last suite, or unrelated
suites can fail with empty errors. `blue-coverage-api.patch` drops a
`coverage-data->lcov` keyword this Guile rejects; without it, every coverage
run dies before writing a report. If either symptom returns, check that the
patches still apply to the blue pin before suspecting the code under test.
Do not replace blue's fork with `spawn`: the suite child relies on blue's
`primitive-load-path` redefinition, which a freshly exec'd process loses.

Coverage runs (`GROVE_TESTS_ENABLE_COVERAGE=1`) execute under Guile's debug
VM, which is far slower per build, so the default time limit rises to 900
seconds. Prefer `-j 1`: their subprocess bursts starve each other in
parallel.

Profile the polling watcher against a Flora-sized synthetic project (without
starting a server or touching Flora):

```sh
guile --no-auto-compile -L modules -C build tests/perf/serve-watch.scm
```

Build before testing CLI changes: subprocess tests load compiled modules.
The preview tests require permission to bind localhost sockets.
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
