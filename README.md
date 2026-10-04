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

Then, inside that shell, run:

```sh
guile -L modules examples/linked-notes/build.scm /tmp/grove-preview
```

Open `/tmp/grove-preview/index.html`. The example's home page is selected in
its build configuration; Grove does not generate a page-list index. The build
also writes plain text, RSS, a sitemap, a graph, and static assets. Omit the
output argument to write to `/tmp/grove-linked-notes`.

To see the authoring syntax, start with the
[`math-0001.plant`](examples/linked-notes/plants/algebra/math-0001.plant) page
and follow its links. For smaller demonstrations, build one of these examples:

| Example | Focus |
| --- | --- |
| [`catalog`](examples/catalog/build.scm) | A custom model rendered as HTML and text, without Forester. |
| [`inventory`](examples/inventory/build.scm) | CSV input and data outputs, without Grove content or HTML. |
| [`math-derivations`](examples/showcases/math-derivations/build.scm) | Executable math articles, checked derivations, search, and diagnostics. |
| [`forest-category`](examples/showcases/forest-category/build.scm) | A compact cross-linked collection of authored pages. |

Examples write to `/tmp` by default and accept an output directory as their
first argument.

## Command line

Install the CLI and modules to a local prefix from the development shell:

```sh
blue configure --prefix=/tmp/grove-install
blue build
blue install
```

A CLI project has a `project.scm` whose final expression returns a Grove project.
From its directory, run:

```sh
/tmp/grove-install/bin/grove build
/tmp/grove-install/bin/grove check
```

`build` updates outputs using the existing renderer cache. `check` runs the full
pipeline, including rendering and artifact validation, without reading or writing
output state. It does not undo side effects in project code. Use `--project PATH`
to select a project, `build --output PATH` to override its output directory,
and `--debug` for the original exception and backtrace. See the
[project guide](docs/project.md#command-line-projects) for a complete project file.
The repository examples still use the direct Scheme entry points shown above.

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
