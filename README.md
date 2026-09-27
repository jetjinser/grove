<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove

Grove is a Guile-based content platform. Its project pipeline composes a
reader, an optional analyzer, and independent renderers. The current example
uses the Scribble-inspired `.grv` language and Forester's bidirectional links
to build a notes site.

## Try the example

Enter the development environment from the repository root:

```sh
nix develop ./nix
```

Then build the linked-notes example:

```sh
guile -L modules examples/linked-notes/build.scm
```

By default, the build writes HTML to `/tmp/grove-linked-notes` and plain text
to `/tmp/grove-linked-notes/text`, plus `feed.xml` and `sitemap.xml`. Pass an
output directory as the first argument to choose another location.

The example shows the main project pieces:

- `.grv` pages are Guile Scheme sources with at-exp document syntax.
- `source/modules/commands.grv` defines shared Scheme commands loaded before
  pages are read.
- Forester turns page links into forward and reverse links.
- HTML, plain-text, RSS, and sitemap renderers write separate targets.
- The HTML style declares `style.css` as both a project input and an artifact
  dependency, so edits are reported and rebuild the stylesheet output.

## Try a non-Forester project

```sh
guile -L modules examples/catalog/build.scm
```

This project reads `.quote` files into its own catalog record, then writes
`index.html`, `catalog.css`, and `catalog.txt` to `/tmp/grove-catalog`. Its
reader, analyzer, HTML output, and text output use Grove's generic project and
artifact APIs without importing Forester.

For a non-document project with no HTML renderer, try the inventory example:

```sh
guile -L modules examples/inventory/build.scm
```

It reads CSV into project-owned Scheme records and writes a summary CSV plus a
Scheme data file to `/tmp/grove-inventory`.

## Project structure

Grove owns file collection, model-independent project orchestration, the
generic content IR, build reports, artifact writing, and optional renderer
caching. Projects supply a source reader and analyzer; the analyzer can return
any Scheme value, which each configured renderer consumes to produce
artifacts. The page-oriented Site records are an optional adapter.

Forester is one library built on those interfaces. It currently provides
linked-page analysis and HTML, text, and RSS renderers. See
[`docs/project.md`](docs/project.md) for project configuration, source
dependencies, cache behavior, routing, and MathJax. See
[`docs/ir.md`](docs/ir.md) for the Grove content records.
[`docs/architecture.md`](docs/architecture.md) records the Grove/Forester
dependency and packaging boundary.

## Checks

From the development environment, run:

```sh
blue check
reuse lint
```
