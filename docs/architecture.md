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
| File collection, project phases, source tracking, artifact writing, and renderer caching. | The forest model, page-link analysis, backlink lookup, and unresolved-link errors. |
| Generic content, site/page, artifact, renderer, and project contracts. | Forester commands and the `.grv` page metadata convention. |
| General Scribble reader support and generic HTML, text, and style helpers. | HTML, text, RSS, and sitemap output for a Forester forest. |

Projects connect these parts by passing procedures to `make-project`. The
analyzer may return any value; Grove passes it to each configured renderer
without inspecting it. Another library can provide a different analyzer and
renderer set without depending on Forester.

The source tree and Blueprint configuration keep Grove and Forester in
separate module groups (`grv-modules` and `forester-modules`), but the current
install command installs both groups from this repository. This is a source
and dependency boundary, not yet a separately versioned or independently
released package boundary. Forester's `(forester private ...)` modules are
implementation helpers and are not host extension points.

Keep the current single-repository packaging. A separate Grove package would
need versioning and compatibility commitments before a second consumer has
shown which interfaces it needs. Reassess packaging when an independent
library or repository consumes Grove without Forester. At that point, verify
that it can install and use the collector, project contracts, content/site
records, and artifact API without importing Forester modules; extract a
smaller shared interface only if that consumer demonstrates a need.
