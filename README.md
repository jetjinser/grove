<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove

Grove is a Scheme toolkit for building documents and other file-based projects.
A project supplies a reader, an optional analyzer, and renderers. Grove tracks
inputs and writes their outputs.

Forester adds linked writing: each `.plant` file is a tree, with a title and a
stable address. Trees can contain subtrees, link to one another, and embed other
trees. Ordinary prose becomes paragraphs; Scheme expressions make documents
programmable. The same content can produce HTML, text, feeds, and other formats.

## Get started

From the repository root, enter the development environment and install locally:

```sh
nix develop ./nix
blue prefix=/tmp/grove-install exec-prefix=/tmp/grove-install \
  bindir=/tmp/grove-install/bin \
  guile-module-dir=/tmp/grove-install/share/guile/site/3.0 \
  guile-object-dir=/tmp/grove-install/lib/guile/3.0/site-ccache \
  build -- install
export PATH=/tmp/grove-install/bin:$PATH
```

Build the linked-notes example:

```sh
grove build --project examples/linked-notes/project.scm --output /tmp/grove-preview
```

Open `/tmp/grove-preview/index.html`. The project selects its home tree explicitly.
The installed launcher records runtime dependency paths, so it also runs outside
the development shell while those dependencies remain installed.

## Write and build

A `.plant` document uses Scribble-style at-expressions:

```scheme
@use-modules[(forester commands) (language scribble commands)]

@title{A first note}
@tag["example"]

Ordinary text becomes a paragraph.

@subtree[#:id "an-idea"]{
  @title{An idea}
  A tree can be written inside another tree.
}

See @link["an-idea"]{the idea}, or embed it with @transclude["an-idea"].
```

A project's `project.scm` returns its configuration. From that directory:

| Command | Purpose |
| --- | --- |
| `grove build` | Build outputs, reusing cached results where possible. |
| `grove check` | Run the complete pipeline without writing outputs. |
| `grove serve` | Preview at `http://127.0.0.1:8080/` and rebuild after edits. |
| `grove browse` | Analyze the project and browse its model in the terminal. |
| `grove ssh-keygen` | Generate an RSA host key for `grove browse --ssh`. |
| `grove new` | Create a tree using the project's default prefix. |

`grove browse` can also serve the same browser over SSH with `--ssh PORT`
(plus `--host-key` and `--authorized-keys`); see
[docs/project.md](docs/project.md) for host keys, limits, and concurrency.

Set `#:default-prefix "jsk"` to create addresses such as `jsk-0001` and
`jsk-000a`. `grove new math` selects another prefix. New trees need a title
before building. Use `grove --help` for options and the
[project reference](docs/project.md) for configuration.

## Explore

Documents can query authored trees and content with miniKanren, run Scheme
calculations, and highlight Scheme code blocks. Mathematics has separate
interfaces for meaning (`◆:`), optional formula text (`φ:`), and notation
(`◇:`). Supported calculations include symbolic rewriting, differentiation,
Cartesian vector calculus, and limited polynomial integration and Poisson
solutions; Grove is not a general-purpose computer algebra system.

The repository examples are small, runnable demonstrations:

- [Linked notes](examples/linked-notes/project.scm): trees, composition, queries,
  styles, and several output formats, including static Gemtext in `build/gemini/`.
- [Math derivations](examples/showcases/math-derivations/project.scm): calculations,
  checked steps, search, and diagnostics.
- [Catalog](examples/catalog/project.scm) and [inventory](examples/inventory/project.scm):
  custom models and readers without Forester.

See [HACKING](HACKING.md) for development principles, checks, and technical references.
