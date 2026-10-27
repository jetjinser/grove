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
| Configure the normal Forester site | `(forester project)` → `make-forester-project` | the reader, analysis, or complete renderer set needs replacing |
| Build a site from page documents | `(grove project site)` → `make-site-project` | the input is not a page or the model is project-specific |
| Build an arbitrary file-based model | `(grove project)` → `make-project` | custom collection, reading, analysis, artifacts, or execution options are needed |
| Handle project failures in a Scheme host | `(grove project diagnostic)` for structured errors and Grove's formatter | custom work needs to add a stage or path; use `call-with-project-stage` |
| Change how content renders | the format's renderer options and `(grove render rules)` | the project needs a new output format or a different build stage |
| Add a model-derived output | a renderer in `#:additional-renderers` or `#:renderers` | the output depends on declared files or an external tool; use `#:pipelines` |
| Write or extend math | `(grove math formula)` for text-based writing and calculations; `(grove math semantic)` for typed values; `(grove math notation)` for display layout | code needs to inspect reports, compose passes, or query rule relations |
| Count user-perceived text units | `grapheme-count` from `(grove unicode)` | the task needs codepoint or UTF-8 byte counts instead |

`make-forester-project` supplies the normal reader, forest analysis, default
HTML renderer, and source-creation behavior. Configure the default HTML with
`#:html-options`, transform pages before analysis with `#:transforms`, and add
peer outputs with `#:additional-renderers`. These changes do not require
reassembling the standard Forester reader and analyzer.
`make-site-project` keeps the page and Content IR model while exposing the
complete renderer set and other lifecycle stages for composition. `make-project`
is the general lifecycle and does not require Grove's Content IR. See the
[project reference](project.md) for callback contracts and examples.

For renderer extensions, a plain procedure from model to artifact list is the
default. `make-cached-renderer` is an advanced opt-in when an extension has
inputs or configuration Grove cannot see through the model and tracked files.
`make-render-rules` is narrower: it changes the interpretation of content kinds
inside an existing renderer. See the [customization guide](customizing.md) for
how these layers compose.

## API layers

The modules form three user-facing layers rather than one umbrella module:

- **Project entry points** provide defaults: `(forester project)` for the
  standard Forester site, `(grove project site)` for custom page-based sites,
  and `(grove project)` for arbitrary models.
- **Composition points** are the intended extension seams: renderer
  procedures, render rules, content transforms, Scribble modules, relations,
  and asset pipelines. They use ordinary Scheme procedures and lists unless a
  typed value adds validation or meaning.
- **Domain modules** expose the useful pieces of a subsystem independently.
  Math Formula is its writing facade; rule, proof, audit, and pass modules are
  lower-level interfaces for projects building their own workflows.

Modules whose name includes `private` are internal. Public domain modules
remain available for direct composition, but their presence does not make every
low-level binding a recommended starting point. This API map is the supported
route from a user task to the narrowest useful layer.

Private bindings may change with their implementations. Public content
interpreters accept render rules and context; stock renderers use private
dispatch details for tree traversal and format-specific behavior.

For math, Formula is the writing-oriented facade and Semantic constructors
are the typed extension surface. `(grove math pipeline)` and the rule, proof,
and audit modules expose lower-level calculation machinery for code that needs
to inspect and compose those results; ordinary article authors should not need
them.

## Content values

Content text, layout, element, document, and style records are values. Their
constructors copy mutable strings and pair structure, and their public
accessors return independent copies of mutable data. Changing a string or list
returned by an accessor does not alter the node. Use `content-with-style` and
`content-with-property` to derive an element with an intentional change.
Opaque extension values embedded in properties remain shared and follow their
own type's contract.

`(grove ir)` defines these content values and their structural operations.
Text projection is an interpreter: use `(grove render plain-text)` for a
readable text form, including math-specific notation, and `(grove render text)`
for formatted text output.

Artifacts use the same ownership rule: `make-artifact` and
`make-byte-artifact` snapshot their path, content, and source paths. The public
`artifact-content` accessor returns a fresh string or bytevector.

HTML presentation, home-link, and navigation-link values apply the same rule
to their SXML, labels, IDs, and attribute lists. Their accessors return copies
of mutable data, so editing a returned fragment does not change the configured
presentation.
