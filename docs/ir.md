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

`(grove collect)` lists source paths from a directory using a caller-supplied
filename predicate. `(grove input scribble)` provides a page reader for one
source path at a time. It uses a top-level `@title{...}` element as page
metadata and removes that element from the page body. The reader evaluates
Scribble in a Guile environment shared by pages read through the same reader.

`(grove build)` maps any page reader over a collection of sources, constructs
the site, and applies any list of renderer functions to that site. Renderers
consume the site IR and return `(grove output)` artifacts with relative paths
and text content. `(grove output)` can prefix those paths and write the
artifacts to a chosen directory; it rejects duplicate output paths before
writing. The linked-notes example collects `.scrbl` files, reads them once,
then renders HTML and text from the same site. The text artifacts are prefixed
with `text` before both sets are written.

The build stages can be composed directly:

```scheme
(define site
  (build-site (collect-directory source-directory include-source?)
              (make-scribble-page-reader)
              #:title "My Site"))

(write-artifacts output-directory
  (render-site site
    (list render-html
          (lambda (site)
            (prefix-artifacts "text" (render-text site))))))
```

`collect-directory` only chooses source paths. A different collection function
can supply the same list, and a different reader can turn each source into a
site page. A renderer is a function from a site to a list of artifacts, so
renderer-specific configuration can be captured in a closure. The output
writer only handles artifact paths and contents.

## HTML styles

`(grove style html)` holds stylesheet artifacts and optional additional text
assets. `(make-html-renderer #:style style)` returns a renderer function for
`render-site`. The HTML renderer emits each style artifact once and links the
stylesheets from every generated page. The default style has no assets, so
existing `render-html` and `site->html` calls keep their output format.

The linked-notes example reads its CSS file into an artifact, places that
artifact in an HTML style, and passes the configured renderer to `render-site`.
Text rendering consumes the same site without using the HTML style.
