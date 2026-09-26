<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove IR

Grove's input languages evaluate to content values. Renderers consume those
values; input languages do not choose an output format.

`(grove ir)` provides four content records:

| Record | Meaning |
| --- | --- |
| `content-text` | Ordinary text. |
| `content-layout` | Layout with its original text, such as a newline. |
| `content-element` | An open, named element with children and properties. |
| `content-document` | The root sequence of content nodes. |

Each record has an optional `source` field for future source locations. A
`content-element` has a symbolic `kind`, content children, and an association
list of properties. Renderers may support different kinds. The current shared
conventions are `link` with a logical page ID in `target`, `math` with an
optional `display?` property, and `title` for document titles. Strings passed
to element or document constructors become `content-text` nodes.

`(grove site)` provides site pages and a resolved site. A page has an ID,
title, and content document; the site has its own title. `make-site` checks
that IDs are unique and every logical link target exists, then records outgoing
links. `site-links` returns target IDs, and `site-backlinks` returns source
pages. The site IR contains no HTML paths or markup.

`(grove input scribble)` reads `.scrbl` files into this IR. It uses a top-level
`@title{...}` element as page metadata and removes that element from the page
body. `(grove render html)` turns the same IR into HTML; other renderers can
consume it without parsing Scribble or inspecting HTML.
