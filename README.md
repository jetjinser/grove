<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove

Grove is a Scheme toolkit for turning source files into useful projects and
documents. You provide a reader, an optional analyzer, and one or more
renderers; Grove tracks inputs and writes the resulting artifacts.

The repository's writing format is `.plant`: Guile Scheme with Scribble-style
at-expressions. Forester builds on Grove to add linked pages, backlinks,
subtrees, transclusion, and site renderers. Grove's project and content APIs
can also be used without Forester or HTML.

Document structure is made of trees: each `.plant` file defines a root tree,
and `subtree` defines a nested tree. Titles are tree properties; links and
transclusions use tree IDs. Ordinary text becomes paragraphs in both root and
subtree bodies.

## Try it

From the repository root, enter the development environment and build the
linked-notes example:

```sh
nix develop ./nix
```

Install the CLI as described below, then run:

```sh
grove build --project examples/linked-notes/project.scm --output /tmp/grove-preview
```

Open `/tmp/grove-preview/index.html`. The example's home page is selected in
its build configuration; Grove does not generate a page-list index. The build
also writes plain text, RSS, a sitemap, a graph, and static assets. Omit
`--output` to write to the example’s `build/` directory.

To see the authoring syntax, start with the
[`math-0001.plant`](examples/linked-notes/plants/algebra/math-0001.plant) page
and follow its links. For smaller demonstrations, build one of these examples:

| Example | Focus |
| --- | --- |
| [`catalog`](examples/catalog/project.scm) | A custom model rendered as HTML and text, without Forester. |
| [`inventory`](examples/inventory/project.scm) | CSV input and data outputs, without Grove content or HTML. |
| [`math-derivations`](examples/showcases/math-derivations/project.scm) | Executable math articles, checked derivations, search, and diagnostics. |
| [`forest-category`](examples/showcases/forest-category/project.scm) | A compact cross-linked collection of authored pages. |

Each example returns a project from `project.scm` and writes to its own `build/`
directory. Use `--output` to choose another location.

## Command line

Install the CLI and modules to a local prefix from the development shell:

```sh
blue prefix=/tmp/grove-install exec-prefix=/tmp/grove-install \
  bindir=/tmp/grove-install/bin \
  guile-module-dir=/tmp/grove-install/share/guile/site/3.0 \
  guile-object-dir=/tmp/grove-install/lib/guile/3.0/site-ccache \
  build -- install
export PATH=/tmp/grove-install/bin:$PATH
```

A CLI project has a `project.scm` whose final expression returns a Grove project.
From its directory, run:

```sh
/tmp/grove-install/bin/grove build
/tmp/grove-install/bin/grove check
/tmp/grove-install/bin/grove serve --port 8080
```

Progress starts with `Building` or `Checking` for the project, then keeps a
`Reading` line for each source on stderr.
A separate terminal progress bar shows completed reads and the current file;
it disappears when reading ends. Stage lines and the elapsed-time summary remain
visible. Redirected logs use plain lines without the dynamic bar.

`build` updates outputs using the existing renderer cache. `check` runs the full
pipeline, including rendering and artifact validation, without reading or writing
output state. It does not undo side effects in project code. Use `--project PATH`
to select a project, `build --output PATH` to override its output directory,
and `--debug` for the original exception and backtrace. `serve` previews at
`http://127.0.0.1:8080/`, rebuilds on changes, and keeps the last successful
outputs available when a rebuild fails. Use `--no-watch` for a fixed preview;
reload the browser to see a successful update. See the
[project guide](docs/project.md#command-line-projects) for a complete project file.
The same commands work from each example directory, using its `project.scm`.

## What you can build

- Read text or Scheme-based sources into any Scheme data model.
- Analyze that model once and send it to independent renderers.
- Use Grove's content IR and HTML/text renderers, or provide your own output
  formats and artifact data.
- Write `.plant` documents with executable Scheme, reusable commands, and
  automatic paragraphs for ordinary prose.
- Use Forester for page links, named content trees, transclusion, and
  miniKanren queries over pages and document content, including deferred
  `@forest-query` blocks in authored pages.
- Write math with the Formula (`φ:`), Semantic (`◆:`), and Notation (`◇:`)
  interfaces. Computation produces inspectable reports; document commands
  render those reports as content.

## Learn more

- [Project guide](docs/project.md): configure readers, analyzers, renderers,
  output paths, and caching.
- [Math guide](docs/math.md): authoring, computation, notation, and current
  mathematical scope.
- [Relations guide](docs/relations.md): miniKanren API and Forester queries.
- [Subtrees and transclusion](docs/forester-composition.md): composing
  addressable content trees.
- [HACKING.md](HACKING.md): development setup, code boundaries, tests, and
  contribution workflow.

## Checks

Inside the development environment, run:

```sh
blue check
reuse lint
```
