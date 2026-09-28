<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Forester subtrees and transclusion

Forester has two related ways to compose trees. A `subtree` declares content
inside a tree and makes that content addressable. A `transclude` inserts a
tree's content at another point. Transclusion keeps the target tree as a
separate page as well.

## Declare an inline tree

Use `@subtree{...}` with exactly one direct title:

```scheme
@use-modules[(my-site commands)]

@title{A reusable calculation}

@subtree{
  @title{Difference of squares}
  @heading{Identity}
  @display-math{a^2-b^2=(a-b)(a+b)}
  @paragraph{See @link["algebra#examples"]{the algebra examples}.}
}
```

The subtree stays visible at its definition and gets its own HTML and text
routes. Its title becomes the standalone page title; the direct `@title` is
removed from the body. Headings and links in its body belong to the subtree's
tree, including when that body is rendered inside another tree.

## Generated IDs

The analyzer assigns IDs in source-page order and document preorder, counting
from `unstable-0` for each forest analysis. An outer subtree is numbered
before subtrees nested inside it; siblings follow their content order. The
number is deterministic for the same ordered inputs, but adding or moving a
subtree can change later numbers.

These IDs are internal and intentionally unstable. Treat them as build-time
addresses for composition, not durable names: do not publish them as
bookmarks, external links, or user-facing identifiers. File-backed tree IDs
remain the appropriate choice for stable references.

## Embed a tree

`@transclude["tree-id"]` inserts the target tree's body at that point:

```scheme
@title{Two uses}
@paragraph{First context:}
@transclude["unstable-0"]

@paragraph{Another context:}
@transclude["unstable-0"]
```

This example assumes the desired subtree received `unstable-0` in the
current source order. If the order changes, update the internal transclusion
target accordingly. The target still has a standalone page. Unresolved targets
and cycles across subtree/transclusion composition fail during analysis.

## Links inside embedded content

Transclusion changes where content appears; it does not change which tree
owns that content. An ordinary `@link` inside the target is indexed against
the target tree, checked there, and rendered to its resolved destination from
the host page. Its backlink/source identity remains the target tree. HTML
anchor IDs are prefixed per embedding occurrence so repeated content does not
produce duplicate IDs. Plain-text relative links are calculated from the
output route where the embedded content appears.

Host artifacts depend on the source files of trees they compose, so edits to a
transcluded tree invalidate the pages that embed it.

## Scheme API

The `(forester composition)` module is the shared interface for providing and
querying composition facts. Providers pass typed subtree definitions and
transclusion occurrences; consumers query those meanings directly:

```scheme
(define composition
  (make-forest-composition
    (list (make-subtree-definition "math-0003" "unstable-0"))
    (list (make-forest-transclusion
            "math-0004" "transclude-1" "unstable-0"))))

(forest-composition-subtree-definitions composition)
(forest-composition-outgoing-transclusions composition "math-0004")
(forest-composition-incoming-transclusions composition "unstable-0")
```

The API preserves fact and occurrence order and rejects duplicate definitions
or duplicate occurrence IDs within a source tree. Grove relation values, tuple
columns, and kind tags are internal to this module. A project can provide
composition facts from another source while using the same semantic queries.

## Output format policy

- HTML and text generate a standalone route for each inline subtree and render
  the subtree at its definition. Their main site indexes list file-backed
  trees only, keeping the index focused on authored pages.
- The sitemap includes inline subtree routes because they are independently
  addressable pages.
- RSS includes file-backed pages only; inline subtrees are fragments of their
  defining page and do not become feed entries.
- The graph contains every page/tree node, `subtrees` edges for definition
  containment, and `transclusions` edges for each embedding occurrence. These
  two edge types have different meanings and remain separate.

## Complete example

The runnable [linked-notes example](../examples/linked-notes/source/algebra/math-0003.grv)
defines a reusable identity, links from its body to an existing heading, and
transcludes it twice. A second [source file](../examples/linked-notes/source/algebra/math-0004.grv)
also transcludes that subtree. Run `guile -L modules examples/linked-notes/build.scm`; output
goes to `/tmp/grove-linked-notes` unless another output directory is passed.
