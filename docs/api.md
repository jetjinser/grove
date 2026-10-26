<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# API map

Grove is a Scheme platform, so the project file and `.plant` pages can use
ordinary Guile and SRFI code. Start at the highest layer that already matches
the work; drop lower only when you need to replace part of its behavior.

| Task | Start here | Reach lower when… |
| --- | --- | --- |
| Write a Forester page | `(forester commands)` and `(language scribble commands)` in `.plant` | the page needs project-local Scheme modules or a new command vocabulary |
| Configure the normal Forester site | `(forester project)` → `make-forester-project` | the reader, analysis, site model, or renderer composition needs changing |
| Build a site from page documents | `(grove project site)` → `make-site-project` | the input is not a page or the model is project-specific |
| Build an arbitrary file-based model | `(grove project)` → `make-project` | custom collection, reading, analysis, artifacts, or execution options are needed |
| Change how content renders | the format's renderer options and `(grove render rules)` | the project needs a new output format or a different build stage |
| Add a separate generated file | `#:derived` for a site projection; `#:pipelines` for tool-driven outputs | the project needs to own the full build lifecycle |
| Write or extend math | `(grove math formula)` for formula text; `(grove math semantic)` for typed values; `(grove math notation)` for display layout | code needs to inspect reports, compose passes, or query rule relations |

`make-forester-project` supplies the normal reader, forest analysis, HTML
renderer, and source-creation behavior. `make-site-project` keeps the page and
Content IR model while exposing those pieces for composition. `make-project`
is the general lifecycle and does not require Grove's Content IR. See the
[project reference](project.md) for callback contracts and examples.

For renderer extensions, a plain procedure from model to artifact list is the
default. `make-cached-renderer` is an advanced opt-in when an extension has
inputs or configuration Grove cannot see through the model and tracked files.
`make-render-rules` is narrower: it changes the interpretation of content kinds
inside an existing renderer. See the [customization guide](customizing.md) for
how these layers compose.

The modules under `(grove private ...)` and `(forester private ...)` are
implementation support for Grove and its bundled formats. Their bindings may
change with those implementations. Public content interpreters accept render
rules and context; stock renderers use private dispatch details for tree
traversal and format-specific behavior.

For math, Formula is the writing-oriented facade and Semantic constructors
are the typed extension surface. `(grove math pipeline)` and the rule, proof,
and audit modules expose lower-level calculation machinery for code that needs
to inspect and compose those results; ordinary article authors should not need
them.
