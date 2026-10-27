<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove

Grove is a Scheme toolkit for building documents and other file-based projects.
A project supplies a reader, an optional analyzer, and renderers. Grove tracks
inputs and writes their outputs.

Forester adds linked writing: each `.plant` file is a tree with a title and an
ID. Trees can contain subtrees, link to one another, and embed other
trees. Ordinary prose becomes paragraphs; Scheme expressions make documents
programmable. The same content can produce HTML, text, feeds, and other formats.

## Get started

Enter the development environment:

```sh
nix develop
```

Build and install the CLI:

```sh
nix build .#grove
export PATH=$PWD/result/bin:$PATH
```

With Guix, enter the development environment and run the checkout's CLI with
Blue:

```sh
guix time-machine -C channels.scm -- shell -m manifest.scm
blue grove build --project examples/linked-notes/project.scm --output /tmp/grove-preview
```

The development manifest supplies Grove's build and test dependencies. The
standalone Guix package is constructed separately through `grove-package.scm`.

Build the linked-notes example:

```sh
grove build --project examples/linked-notes/project.scm --output /tmp/grove-preview
```

Open `/tmp/grove-preview/index.html`. The project selects its home tree explicitly.
The installed launcher records runtime dependency paths, so it also runs outside
the development shell while those dependencies remain installed.

## Write and build

A minimal Forester project has a `project.scm` and a page under `plants/`:

```sh
mkdir -p plants
```

```scheme
;; project.scm
(use-modules (forester project))
(make-forester-project #:title "My notes"
                       #:home "hello"
                       #:default-prefix "note")
```

```scribble
;; plants/hello.plant
@use-modules[(forester commands) (language scribble commands)]

@title{Hello, Grove}
@tag["example"]

Ordinary text becomes a paragraph.

@subtree[#:id "an-idea"]{
  @title{An idea}
  A tree can be written inside another tree.
}

See @link["an-idea"]{the idea}, or embed it with @transclude["an-idea"].
```

From the project directory, run `grove check` and `grove build` when the CLI is
installed. In the Guix development shell, use `blue grove check` and
`blue grove build`. The `#:home` ID selects the page rendered at `index.html`;
the project constructor supplies the standard reader, analyzer, and HTML
renderer. `grove serve` builds and previews the site locally.

To add a tree, run `grove new`, then fill the required `@title{}` in its new
file before building.

For custom project models, renderers, transformations, and external-tool
pipelines, see the [project reference](docs/project.md) and
[customization guide](docs/customizing.md). The [API map](docs/api.md) shows
which entry point to start from and when to reach for lower-level interfaces.

Other project commands:

| Command | Purpose |
| --- | --- |
| `grove build` | Build outputs, reusing cached results where possible. |
| `grove check` | Run the complete pipeline without writing outputs. |
| `grove serve` | Preview at `http://127.0.0.1:8080/` and rebuild after edits. |
| `grove browse` | Analyze the project and browse its model in the terminal. |
| `grove ssh-keygen` | Generate an RSA host key for `grove browse --ssh`. |
| `grove new` | Ask the project to create a source file. |

`grove browse` can also serve the same browser over SSH with `--ssh PORT`
(plus `--host-key` and `--authorized-keys`); see
[docs/project.md](docs/project.md) for host keys, limits, and concurrency.

Forester projects can set `#:default-prefix "jsk"` so `grove new` creates
addresses such as `jsk-0001` and `jsk-000a`; without a configured default,
provide a prefix, as in `grove new math`. Other
projects can define their own source-creation callback, or omit this command.
New Forester trees need a title before building. Use `grove --help` for options
and the [project reference](docs/project.md) for configuration. For a task-oriented
map from “change X” to the right API, see
[docs/customizing.md](docs/customizing.md).

## Explore

Documents can query authored trees and content with miniKanren, run Scheme
calculations, and highlight Scheme code blocks. Mathematics has separate
interfaces for meaning (`◆:`), optional formula text (`φ:`), and notation
(`◇:`). Supported calculations include symbolic rewriting, differentiation,
Cartesian vector calculus, and limited polynomial integration and Poisson
solutions; Grove is not a general-purpose computer algebra system.

See the [Math reference](docs/math.md) for the supported calculation APIs and
the [Relation reference](docs/relations.md) for finite relation values,
miniKanren goals, and the optional rule DSL.

The repository examples are small, runnable demonstrations:

- [Linked notes](examples/linked-notes/project.scm): trees, composition, queries,
  styles, and several output formats, including static Gemtext in `build/gemini/`.
- [Math derivations](examples/showcases/math-derivations/project.scm): calculations,
  checked steps, search, and diagnostics.
- [Catalog](examples/catalog/project.scm) and [inventory](examples/inventory/project.scm):
  custom models and readers without Forester.

See [HACKING](HACKING.md) for development principles, checks, and technical references.
