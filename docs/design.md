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

If a widget is only a visual preview, keep it out of the accessibility tree and
remove interactive descendants. If it is interactive, it needs focus
management, an accessible name, and a way to close it. The Flora link preview
follows the visual-only rule: it is `aria-hidden`, contains no focusable
descendants, and closes on Escape or focus leaving the link.

## Renderer rules

HTML rendering can be extended with [renderer rules](renderer-rules.md): values
that map a content kind to its interpretation, composed explicitly rather than
registered as callbacks.

## Render context

[Render context](render-context.md) defines the four context layers and their
boundaries.

## Derived artifacts

[Derived artifacts](derived-artifacts.md) defines the projection layer for
sitemap, RSS, and graph.

## Composition audit

[Composition audit](composition.md) maps the existing features onto this model
and lists the remaining gaps.

## Testing

- `blue verify` runs the test suite, REUSE, the example builds, and axe.
- `blue a11y [HTML ...]` audits explicit pages.
- Axe does not test keyboard order, focus movement, or media queries; check
  those in a browser or with a dedicated interaction test.
