<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove and Forester boundary

Grove is the host platform. Forester is a library built on Grove that supplies
one linked-notes model and its renderers. The dependency direction is
one-way: Forester uses Grove interfaces; Grove does not import Forester.

| Grove owns | Forester owns |
| --- | --- |
| File collection, model-independent project phases, source tracking, artifact writing, and renderer caching. | The forest model, page-link analysis, backlink lookup, and unresolved-link errors. |
| Generic content, optional site/page records, artifact, renderer, and project contracts. | Forester commands and the `.grv` page metadata convention. |
| Scribble input support plus generic HTML, text, and style helpers. | HTML, text, RSS, and sitemap output for a Forester forest. |

The generic `(grove project)` reader may return any Scheme value. Grove pairs
it with its source path and passes a `project-context` to the analyzer; the
analyzer may return any value, which Grove passes to each configured renderer
without inspecting. `(grove project site)` is an optional adapter for projects
that want common page and site records. The catalog example exercises the
generic path without importing Forester. Another library can provide a
different reader, analyzer, and renderer set without depending on Forester.

Preparation is a general project hook that runs before reading and returns
additional source paths for tracking. Input libraries use it for concerns
such as loading shared Guile modules. The Grove core does not decide how those
paths are interpreted.

The source tree and Blueprint configuration keep Grove and Forester in
separate module groups (`grv-modules` and `forester-modules`), but the current
install command installs both groups from this repository. The catalog consumer
was copied outside the repository and run with a temporary Guile load path
containing only Grove modules; its actual imports do not require Forester. This
verifies the source-level dependency boundary, but not a published or
independently installable package.
Forester's `(forester private ...)` modules are implementation helpers and are
not host extension points.

From the repository root, the isolated check is:

```sh
mkdir -p /tmp/grove-only-modules
cp -R modules/grove /tmp/grove-only-modules/grove
cp -R examples/catalog /tmp/grove-catalog-consumer
cd /tmp/grove-catalog-consumer
GUILE_AUTO_COMPILE=0 guile -L /tmp/grove-only-modules \
  build.scm /tmp/grove-only-output
```

Keep the current single-repository packaging. The catalog project is a
separate consumer directory, not an independently maintained repository, so
it does not yet establish packaging, versioning, or compatibility needs. When
a second repository wants to use Grove, first verify a Grove-only install and
its public module closure. Split the repository or promise a compatibility
policy only if that consumer's release and maintenance needs call for it.
