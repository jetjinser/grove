<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Forester subtrees and transclusion

Forester has two related ways to compose trees. A `subtree` declares content
inside a tree and makes that content addressable. A `transclude` inserts a
tree's content at another point. Transclusion keeps the target tree as a
separate page as well.

Document structure consists of trees and their composition. A title is a tree
property, not an independently indexed heading. Use nested `subtree` commands
for local organization too; an explicit ID is needed when its identity should
survive edits. HTML title levels follow display depth.

The contents directory follows subtree and transclusion occurrences in body
order. Repeated displays of one tree have distinct local anchors; links to its
ID open its standalone route. These local anchors are renderer output, not
authored fragment targets.

## Declare an inline tree

Use `@subtree{...}` with exactly one direct title. Give a subtree a durable
address with the `#:id` option:

```scheme
@use-modules[(my-site commands)]

@title{A reusable calculation}

@subtree[#:id "difference-of-squares"]{
  @title{Difference of squares}
  @tex-display-math{a^2-b^2=(a-b)(a+b)}
  @paragraph{See @link["algebra#examples"]{the algebra examples}.}
}
```

Ordinary text in a subtree is decoded into paragraphs using the same blank-line
rules as a root document. Direct `summary`, `published`, and `updated` commands
are extracted as frontmatter; `tag` and `taxon` remain queryable content nodes.
The subtree declaration's source location is retained in its page metadata.

Shared Scheme bindings belong at document top level, before the subtree that
uses them. `subtree` is an ordinary procedure; a Scheme `let` expression can
also construct content using local bindings.

The subtree stays visible at its definition and gets its own HTML and text
routes. Its title becomes the standalone page title; the direct `@title` is
removed from the body. Content and links in its body belong to the subtree's
tree, including when that body is rendered inside another tree.

## Subtree IDs

An explicitly named subtree keeps its ID across source edits:

```scheme
@subtree[#:id "difference-of-squares"]{
  @title{Difference of squares}
  @paragraph{A reusable identity.}
}
```

Use that ID in `@transclude["difference-of-squares"]` and in links. IDs share
the same namespace as file-backed page IDs and other subtrees; duplicates are
errors. IDs cannot contain `#`; authored links accept tree IDs only.

Subtrees without `#:id` keep their automatically assigned `unstable-N` IDs.
The analyzer numbers them in source-page order and document preorder, skipping
named subtrees. Adding or moving an unnamed subtree can change later generated
IDs, so use explicit IDs for references that should survive source edits.

## Embed a tree

`@transclude["tree-id"]` inserts the target tree's body at that point:

```scheme
@title{Two uses}
@paragraph{First context:}
@transclude["unstable-0"]

@paragraph{Another context:}
@transclude["unstable-0"]
```

The target still has a standalone page. Unresolved targets and cycles across
subtree/transclusion composition fail during analysis.

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

## Backmatter

HTML and text renderers can show incoming composition and link relationships
after a page's body. The default order is `context`, `references`,
`backlinks`, then `related`; empty sections are omitted. Context lists pages
that transclude the current tree. References lists pages in the configured
reference taxon linked from the current tree or its subtrees and transcluded
trees. Backlinks lists authored links to the current tree.
Related lists non-reference pages linked directly from the current page.
These sections remain separate because embedding, incoming links, citations,
and outgoing related links express different relationships.

Configure the sections on either renderer with `#:backmatter`. The list order
sets the display order, and an empty list disables generated backmatter:

```scheme
(make-html-renderer
  #:routes routes
  #:backmatter '(backlinks))

(make-text-renderer
  #:backmatter '())
```

Use `#:backmatter-labels` to customize headings for the selected sections;
omitted labels keep their defaults:

```scheme
(make-html-renderer
  #:routes routes
  #:backmatter '(context references backlinks related)
  #:backmatter-labels '((context . "Embedded in")
                       (references . "Citations")
                       (backlinks . "Linked from")
                       (related . "See also"))
  #:reference-taxon-id "Bibliography")
```

The default reference taxon ID is `Reference`, following JMS Forester's
reference classification. Mark a reference page with a matching taxon, for
example `@taxon["Reference"]{Reference}`. Projects with another taxonomy can
override the ID through `#:reference-taxon-id`.

## Scheme API

The `(forester composition)` module is the shared interface for providing and
querying composition facts. Providers pass typed subtree definitions and
transclusion occurrences; consumers query those meanings directly:

```scheme
(define composition
  (make-composition
    (list (make-subtree-definition "math-0003" "unstable-0"))
    (list (make-transclusion
            "math-0004" "transclude-1" "unstable-0"))))

(subtree-definitions composition)
(outgoing-transclusions composition "math-0004")
(incoming-transclusions composition "unstable-0")
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

The runnable [linked-notes example](../examples/linked-notes/plants/algebra/math-0003.plant)
defines a reusable identity, links from its body to another named tree, and
transcludes it twice. A second [source file](../examples/linked-notes/plants/algebra/math-0004.plant)
also transcludes that subtree. Run `grove build --project examples/linked-notes/project.scm`; output
goes to the example’s `build/` directory unless `--output PATH` is supplied.

## Styles

The renderer's `#:style` defaults to `default-html-style` from
`(forester style html)`. This is an ordinary `(grove style html)` value.
Passing a style uses it directly; filenames have no special meaning.

To extend the default CSS, compose styles in cascade order:

```scheme
(use-modules (forester style html) (grove style html) (grove output))

(define style
  (compose-html-styles
    default-html-style
    (make-html-style
      #:stylesheets (list (make-artifact "flora.css" "body { color: #333; }")))))

(make-html-renderer #:routes routes #:style style)
```

Pass the custom style alone to replace the default, or `(make-html-style)` to
omit stylesheets entirely. File-backed CSS artifacts should list their source
paths as dependencies, as in the showcase builder.

### Element styles

Rendered prose commands, `link`, `image`, `subtree`, `transclude`, and math
notation commands accept `#:style`. A symbol or string is a named content
style; HTML maps its name to a class:

```scheme
@paragraph[#:style 'notice]{This is a note.}
@link["lemma" #:style 'reference]{Read the lemma.}
@subtree[#:id "lemma" #:style 'callout]{
  @title{Lemma}
  This is the body.
}
@◇:math{@◇:frac[#:style 'ratio]{x}{y}}
```

For HTML-specific attributes, use the existing style property API:

```scheme
@use-modules[(grove style) (grove style html)]
@(define note-style
  (make-content-style 'notice
    (list (make-html-attributes
            '((id . "intro") (class . "important") (role . "note"))))))
@paragraph[#:style note-style]{This is an important note.}
```

`class` is appended to structural classes. HTML IDs and local `href` attributes
in styles are scoped when a tree is embedded, so repeated transclusions keep
separate anchors. A style cannot replace a renderer-owned navigation ID.
A subtree's `#:id` is its tree identity, not an HTML attribute.

`content-with-style` applies the same interface to an element returned by
Scheme code, preserving its content, properties, source, and flow kind.
Styling does not change query or mathematical semantics. Text output ignores
HTML presentation properties. Frontmatter commands extract data rather than
rendering styled elements; computation commands returning several elements
can be placed inside a styled subtree.
