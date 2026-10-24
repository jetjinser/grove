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
anchors an unnamed subtree. Beyond explicit declarations, every tree that
occurs exactly once in a page is anchored there with its tree ID, so a
transcluded tree can be linked through its host without extra syntax. An
occurrence carries exactly one anchor: the author anchor when present,
otherwise the generated `tree-N` position anchor, which now only backs
repeated occurrences. The contents listing and text output address
occurrences through the same anchors, and `tree-root-<page-id>` keeps
anchoring each page's root. Links aimed at an anchor retired by a lost
uniqueness warn through the fragment registry. Unnamed subtrees anchor under
their unstable-N IDs, which inherit those IDs' instability. Author anchors
are emitted as-is (never scoped by embedding, unlike style IDs) and must be
unique per page together with math IDs; duplicates fail during analysis.

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
| Context | Trees embedding this tree: the tree that defines it as a subtree and the trees that transclude it. |
| References | Reference-taxon targets linked from this tree and its composition. |
| Backlinks | Authored links to this tree. |
| Related | Non-reference targets linked directly from this tree. |

HTML/text renderers accept `#:backmatter` to select/order sections (an empty
list disables them), `#:backmatter-labels` to override headings, and
`#:reference-taxon-id` to choose the reference classification. The default is
`reference`, written `@taxon["reference"]`. HTML entries follow the forester
entry anatomy: the target's taxon when its document declares one, the title
link, and a dimmed `[tree-id]` slug; backlink entries show the link's text
(falling back to the source title) with the source tree's slug, linking to
the exact reference anchor.

## Metadata

Tree headers and backmatter entries end in a metadata line. The stock items
are the page's dates (`@published`/`@updated`); pages without dates render
nothing. Add project metadata as static SXML keyed by page ID; Grove appends it
after the built-in date items:

```scheme
(make-html-options
  #:presentation
  (make-html-presentation
      #:metadata-items
      '(("flora-page" . ((li (@ (class "meta-item")) "…author or source items…"))))))
```

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
(use-modules (forester render html)
             (forester style html)
             (grove render html presentation)
             (grove render html routes)
             (grove style html)
             (grove output))

(define style
  (compose-html-styles
    default-html-style
    (make-html-style
      #:stylesheets (list (make-artifact "flora.css" "body { color: #333; }")))))

(make-html-renderer
  #:options (make-html-options
              #:routes routes
              #:presentation (make-html-presentation #:style style)))
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

Display equations are numbered per page in document order: every display
equation counts, named or not. Equations without an author `\tag` render the
build number, prefixed with the page's declared chapter when the page
metadata carries one. Equations without an `#:id` receive a generated
`unstable-eq-N` ID, which joins the anchor registry like any other math ID.
Numbers may shift when equations are inserted; IDs stay author-owned, and
generated IDs carry the same instability as unnamed trees' `unstable-N`.

### Styles are values

A content style is an ordinary Scheme value: bind it, pass it through
custom commands, and combine styles with `merge-content-style`, which
joins names into one class list and keeps every property set (later
styles win where a renderer reads one property of a kind, such as the
tag):

```scheme
(define print-accent
  (merge-content-style accent
    (make-content-style #f
      (list (make-html-attributes '((data-print . "high")))))))
```

`subtree` and `transclude` attach their `#:style` to the occurrence
container, next to `forest-tree-block`: `@transclude["t" #:style
'aside-box]` renders `class="forest-tree-block transclusion aside-box"`,
so CSS descendant selectors scope any stylesheet to that embedding or to
every occurrence of the tree (whose container also carries
`data-tree-id`). A container style cannot set an id; the occurrence
anchor owns it. Styles never push down into the attached content -
descendant selectors cover that.

`content-with-style` applies the same interface to an element returned by
Scheme code, preserving its content, properties, source, and flow kind.
Styling does not change query or mathematical semantics. Text output ignores
HTML presentation properties. Frontmatter commands extract data rather than
rendering styled elements; computation commands returning several elements
can be placed inside a styled subtree.

## Custom elements

`(grove element)` defines element vocabularies without adding renderer
cases. `make-element` returns a command that renders its body inside the
tag of your choice; the renderer adds no attributes of its own, and text
output keeps the body:

```scheme
(use-modules (grove element))

(define wbr     (make-element #:tag "wbr" #:void? #t))
(define callout (make-element #:tag "aside" #:class "callout"
                              #:flow-kind 'block))
(define my:note (make-element #:tag "my:note"
                              #:attributes
                              '((xmlns:my . "https://example.com/ns"))))
```

`@wbr[]` renders `<wbr/>`; `#:void?` marks the element as empty, rejects
content, and emits a self-closing tag. Other elements close explicitly, so
unknown tags never swallow the rest of the document. `#:class` joins the
tag's class, `#:attributes` carries static attribute pairs such as namespace
declarations, and `#:flow-kind` selects inline or block. Tag names are
validated at definition time: they must start with a letter and contain
letters, digits, hyphens, underscores, or colons. The HTML is entirely the
vocabulary author's; Grove ships no element table of its own. `#:kind`
overrides the content element's kind (default `span`) for renderer
features that match on kinds - `#:anchor` and `#:side-lane` are the
current ones.

`#:anchor` declares the occurrence as a linkable anchor — a term
definition, a figure. It renders as the tag's `id` and registers the
element with the anchor registry (`page-anchor-facts`), so fragment links
against the page resolve to it and term queries can see it:

```scheme
(define defn (make-element #:tag "dfn" #:anchor "term-loom"))
```

## Term definitions

Grove collects defining occurrences of terms from anchored elements tagged
`dfn` — the HTML defining-instance semantic. An element like the `defn`
above contributes a `(term-id page-id anchor display-text)` fact to the
forest's term-definition relation, where the anchor is the canonical term
id (derive it from the term text, as the topic convention does) and the
display text is the element's plain-text body, so a defining element may
carry an original term and a translation together.

Three queries read the relations. The report projections name the **host
article** - the root page whose rendered DOM carries each occurrence -
next to the owning page, because an in-paragraph definition's owning id
(an anonymous `unstable-N` subtree) is unreadable in a report without it:

```scheme
(use-modules (forester site))

;; One term's occurrences as (page-id anchor display-text), feeding a
;; generated glossary listing or hover-preview text.
(term-definitions forest "term-loom")

;; Terms whose occurrences disagree - defined on more than one page,
;; or with differing display texts.  Each conflict is
;; (term-id (host-id host-title page-id anchor display-text) ...),
;; in collection order, so a drift report can deep-link to
;; host.html#anchor and name the article it happened in.
(term-definition-conflicts forest)

;; Fragment links aimed at an anchor no page defines, grouped as
;; (term-id (host-id host-title page-id) ...) - the corpus's
;; definition backlog.
(undefined-term-mentions forest)
```

A mention is any `link` or `query-link` whose `fragment` does not resolve
in its target page's anchor index - the same condition the fragment
warning reports, collected as data. The relation constructors
(`make-term-definition-relation`, `make-term-mention-relation`) and their
`term-definitiono` / `term-mentiono` goals are exported from
`(forester relations)` for queries beyond these three.

To mark a mention explicitly - keeping it distinguishable from ordinary
links even when the term is defined - give a link element a `term`
property naming the term id:

```scheme
;; a term command's expansion, at the content level:
(make-content-element 'link body
  #:properties `((target . ,defining-page)
                 (fragment . ,anchor)
                 (term . "term-loom")))
```

Resolution goes through the raw anchor registry - the definition
relation - never through rendered DOM ids, which carry host prefixes
inside subtrees. A resolved mention renders as an anchor to the defining
page's own route (its standalone rendering keeps unprefixed anchor ids)
with the definition's display text when the element has no body of its
own; a term nothing defines renders inline under `missing-term` and
lands in the backlog report instead of the href. The mention relation
records both kinds with their resolution state, so counts of defined and
undefined mentions need no guessing.
