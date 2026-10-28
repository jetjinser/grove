<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Design boundaries

Grove is a document tool, not a UI framework. It supplies the document model,
the HTML shell, and a small neutral stylesheet; projects supply their own visual
design.

## What Grove owns

- Semantic HTML: landmarks, skip link, headings, `lang`/`dir`, metadata, and
  the document shell.
- A minimal default stylesheet: reset, spacing, focus visibility, and the
  accessibility affordances that make generated markup usable without a theme.
- Content primitives: paragraphs, links, images, code, math, sections, and
  subtrees.
- Neutral user-preference handling: reduced motion, and only the contrast
  changes that do not impose a palette.

## What a project owns

- Palette, typography, spacing scale, and component styling.
- Navigation labels and layout.
- Project-specific command sets and their modules.
- Interactive widgets that are not part of the document model.

## Interactive widgets

Prefer native elements: `<details>` for disclosure, `<dialog>` for modals, and
the popover API for transient surfaces. Add small, focused JavaScript for the
behaviour the platform does not provide. Do not add a styled UI framework to
solve one widget.

If a widget is only a visual preview, keep it out of the accessibility tree,
remove interactive descendants, and close it when its trigger loses focus or
the user presses Escape. If it is interactive, it needs focus management, an
accessible name, and a way to close it.

## Renderer rules

HTML rendering can be extended with [renderer rules](renderer-rules.md): values
that map a content kind to its interpretation, composed explicitly rather than
registered as callbacks.

## Render context

[Render context](render-context.md) explains how project analysis and rendering
contexts are separated from build, reader, and math state.

## Site outputs

[Site outputs](site-outputs.md) defines the renderer projection layer for
sitemap, RSS, and graph.

## Composition model

[Composition model](composition.md) records the composition laws used by
content transforms, renderer rules, presentations, contexts, site outputs,
and asset pipelines.

## Resource identity

[Resource identity and RDF projection](resource-identity-rdf.md) explores
IRI-based identity and RDF projection for Grove's relations. It records the
tested Flora mapping, current design direction, and the remaining identity and
vocabulary decisions without committing to a public API.

## Testing

- `blue verify` runs `blue check`, `blue lint`, and `blue check-examples` in
  order; the last command builds the audited examples and runs axe.
- `blue a11y [HTML ...]` audits explicit pages.
- Axe does not test keyboard order, focus movement, or media queries; check
  those in a browser or with a dedicated interaction test.
