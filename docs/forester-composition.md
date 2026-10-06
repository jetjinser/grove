<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Tree composition reference

Forester indexes trees, not headings. Each source file defines a root tree;
`subtree` defines an addressable nested tree and `transclude` displays an
existing tree. Titles are properties; HTML title levels follow display depth.

## Identity and ownership

A subtree requires exactly one direct title:

```scheme
@subtree[#:id "difference-of-squares" #:open? #f]{
  @title{Difference of squares}
  A reusable identity.
}
@transclude["difference-of-squares"]
```

Explicit IDs share the file-backed tree namespace and cannot contain `#`.
Duplicates, unresolved targets, and composition cycles fail during analysis.
Unnamed subtrees receive `unstable-N` IDs in source-page/document preorder;
edits can change them. Use explicit IDs for durable references. `#:open?`
controls initial disclosure state.

Occurrences carry author anchors for fragment links. An explicit `#:id` also
anchors that occurrence in its host page; `#:anchor` overrides the value or
anchors an unnamed subtree, and `transclude` stays unanchored unless the
author passes `#:anchor`. Author anchors are emitted as-is (never scoped by
embedding, unlike style IDs) and must be unique per page together with math
IDs; duplicates fail during analysis. The generated `tree-N` position anchors
are unchanged.

Root and subtree prose use the same paragraph decoder. Direct title, summary,
and dates become frontmatter. `tag` and `taxon` accept one value without a
separate label, normalize it to lowercase, and remain queryable content nodes.
Topic selectors and reference classification are case-insensitive. Repeated
tags differing only in case are duplicates. Declaration source locations are
retained. Shared Scheme definitions belong outside ordinary procedure bodies
such as `subtree`; use `let` to construct locally bound content.

A tree owns its body and references even when displayed elsewhere. Embedding
does not transfer backlink identity to the host. Repeated occurrences receive
separate local HTML anchors; links to tree IDs select standalone routes, not
those anchors. Text links resolve relative to the displayed output route.
Host artifacts depend on the sources of their composed trees.

The contents listing follows display occurrences in body order. HTML and text
provide standalone subtree routes. Sitemaps include them; RSS includes only
file-backed trees. Graph output keeps definition containment and transclusion
edges separate.

## Backmatter

Default sections are `context`, `references`, `backlinks`, and `related`, in
that order; empty sections are omitted:

| Section | Meaning |
| --- | --- |
| Context | Trees that transclude this tree. |
| References | Reference-taxon targets linked from this tree and its composition. |
| Backlinks | Authored links to this tree. |
| Related | Non-reference targets linked directly from this tree. |

HTML/text renderers accept `#:backmatter` to select/order sections (an empty
list disables them), `#:backmatter-labels` to override headings, and
`#:reference-taxon-id` to choose the reference classification. The default is
`reference`, written `@taxon["reference"]`.

## Composition API

`(forester composition)` accepts typed `make-subtree-definition` and
`make-transclusion` records through `make-composition`. Query via
`subtree-definitions`, `outgoing-transclusions`, or `incoming-transclusions`.
Occurrence order is preserved; duplicate definitions and duplicate occurrence
IDs within a source tree are rejected. Tuple layout and relation implementation
remain private, so other providers can supply the same semantic facts.

See [linked notes](../examples/linked-notes/project.scm) for an executable
composition example.

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
A subtree's `#:id` is its tree identity; explicit IDs and `#:anchor` values
double as unscoped occurrence anchors, while style IDs remain scoped.

### Reference fragments and equation IDs

A link target may carry a fragment after `#`. The page before the `#` is
validated like any other link; the fragment is the id of a node on that page and
is passed through to the output URL:

```scheme
@subtree[#:id "theorem-3"]{
  @title{Theorem 3}
  The bound holds while the flow stays smooth.
}
@tex-display-math[#:id "eq-2-12"]{a^2 + b^2 = c^2 \tag{2.12}}
@paragraph{See @link["cfd-0003#theorem-3"]{Theorem 3} and
@link["cfd-0003#eq-2-12"]{(2.12)}.}
```

`@tex-math` and `@tex-display-math` put `#:id` on their container
(`span.math-inline` / `div.math-display`), which is what the fragment scrolls
to. Explicit subtree IDs and occurrence `#:anchor` values are other anchor
sources. A fragment resolves inside the target page's own DOM, not inside
pages that transclude it. Links without a fragment are unchanged. The reader
splits the target into the page (`target`) and the `fragment`, so the
reference graph and backlinks stay page-level. A raw `#` in a link's `target`
property is rejected.

Fragments are existence-checked against the anchors the target page declares
in its own document: equation IDs, explicit subtree IDs, occurrence anchors,
and the page's `tree-root-<page-id>` root. Unknown fragments are reported as
build warnings naming the fragment, the target page, and the source file.
Generated position anchors (`tree-N`, `ref-N`) are implementation details,
not author contracts, and warn like any other unknown fragment. Links
returned by queries receive the same check.

`content-with-style` applies the same interface to an element returned by
Scheme code, preserving its content, properties, source, and flow kind.
Styling does not change query or mathematical semantics. Text output ignores
HTML presentation properties. Frontmatter commands extract data rather than
rendering styled elements; computation commands returning several elements
can be placed inside a styled subtree.
