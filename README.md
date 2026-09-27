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
to `/tmp/grove-linked-notes/text`. Pass an output directory as the first
argument to choose another location.

The example shows the main project pieces:

- `.grv` pages are Guile Scheme sources with at-exp document syntax.
- `source/modules/commands.grv` defines shared Scheme commands loaded before
  pages are read.
- Forester turns page links into forward and reverse links.
- HTML and plain-text renderers write separate targets.
- The HTML style declares `style.css` as both a project input and an artifact
  dependency, so edits are reported and rebuild the stylesheet output.

## Project structure

Grove owns file collection, the generic content IR, build reports, artifact
writing, and optional renderer caching. Projects supply a page reader and may
provide an analyzer that returns any Scheme value. Each renderer consumes that
value and returns artifacts; renderers can be combined for multiple targets.

Forester is one library built on those interfaces. It currently provides
linked-page analysis and HTML, text, and RSS renderers. See
[`docs/project.md`](docs/project.md) for project configuration, source
dependencies, cache behavior, routing, and MathJax. See
[`docs/ir.md`](docs/ir.md) for the Grove content records.

## Checks

From the development environment, run:

```sh
blue check
reuse lint
```
