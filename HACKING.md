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
`GROVE_TESTS_TIME_LIMIT` to override). The subprocess-heavy suites — preview,
store sharing, ssh, cli — approach that limit while other fixtures run beside
them, so a full parallel `blue check tests/unit` can report cascading suite
failures that vanish when the suite runs alone. Suspect the limit, not the
code, and reach for `blue -j 1 check` when diagnosing interference between
parallel fixtures.

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
