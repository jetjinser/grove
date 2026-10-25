<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Relation reference

Grove's relation layer uses miniKanren goals to query finite facts and to
compose domain rules. faster-minikanren is the implementation used by Grove;
the public Grove module owns the relation API and its documented contracts.

## Relation values

A relation is an immutable, finite snapshot of ground fact rows. Each row is a
proper Scheme list containing one or more terms, and every row in one relation
has the same arity. A term can be an atomic Scheme value or a pair tree of
terms. Vectors are reserved by faster-minikanren for logic variables and are
not valid relation data. A relation cannot contain a live logic variable.
Fact rows and pair terms are ordinary Scheme data; callers must treat them as
read-only after relation construction, including values returned by
`relation-facts` and query answers.

Base facts have set semantics: structurally equal rows occur once. The
constructor retains the first occurrence order, and `relation-facts` exposes
that order. Relation arity is part of the relation value and is checked when a
goal runs. An empty relation defaults to arity two for convenience; pass
`#:arity` to `make-relation` to construct an empty relation with another arity.

Construct and query finite facts with:

```scheme
(define references
  (make-relation
    '(("intro" "algebra")
      ("limits" "algebra"))
    #:arity 2))

(run* (source)
  (relationo references source "algebra"))
```

Rows are tuples, so nested lists remain available as individual terms. The
relation goal accepts one logic argument per row column; callers use ordinary
miniKanren variables and compose relation goals with `conde`, `=/=`, and other
supported goals.

## Relation library DSL

The optional `(grove relation dsl)` module provides a small macro vocabulary
for libraries that define reusable rules. It re-exports the relation and
miniKanren APIs from `(grove relation)` and adds `define-relation`, `exists`,
`all`, `either`, `project`, and `query`:

```scheme
(use-modules (grove relation dsl))

(define-relation-schema make-reference-relation referenceo
  (source-page reference-id target-page label))

(define-relation (incoming-referenceo references target source)
  (referenceo references
              (source-page source)
              (target-page target)))

(query (source)
  (incoming-referenceo references "algebra" source))
```

These forms expand to faster-minikanren relations, `fresh`, `conde`, and
`run*`; `project` exposes miniKanren's host-language projection. Host operations
must handle the projected values, including any unbound logic variables;
projection does not make an arbitrary Scheme function relational. These forms
add no new evaluation engine.
`define-relation-schema` declares
the tuple columns once, generates a constructor with fixed arity, and creates a
named query macro. Query clauses name columns, so omitted columns become
independent anonymous logic variables. Unknown or repeated columns are
rejected during macro expansion. For example, a query can constrain
`(target-page "algebra")` without naming the other three columns. Terms are
ordinary Scheme expressions in lexical scope.
`either` clauses and `all` goals retain standard miniKanren search order, and
results retain its duplicate-answer behavior. Libraries pass relation values
explicitly and export their own named rules. The positional `relationo` and
core miniKanren forms remain available for lower-level code.

## Query results

Queries enumerate successful derivations in miniKanren search order. Two
different proofs may produce equal answer values, so query results are not
implicitly deduplicated. Callers that need set semantics must request it
explicitly. Domain APIs may impose stronger ordering or uniqueness contracts;
for example, Forester page queries return pages in site order and once per
page, regardless of how many references or derivations reach that page.

The order of conjuncts and disjuncts is part of query execution. Domain rules
should state ordering and duplicate behavior at their public boundary rather
than relying on incidental internal relation ordering.

## Extension boundary

Grove supplies the relation value, relation goal, and miniKanren query forms.
Domain libraries own their fact schemas, named predicates, and recursive
rules. They receive relation values explicitly; Grove does not keep an
implicit global fact database or import domain-specific relation modules.

Grove's relation implementation needs two operations that faster-minikanren
does not export: identifying a logic variable and projecting ground values
inside a goal. Their `@@` access is confined to `(grove private minikanren)`;
the public relation tests exercise indexing, unification, joins, and variable
projection through `(grove relation)`. If faster-minikanren changes these
internals, this adapter and those public behavior tests are the compatibility
boundary to update.

The Forester reference adapter lives in `(forester relations)` independently
of `(forester site)`. It stores each reference occurrence as a four-column
row: `(source-page-id local-reference-id target-page-id label)`. Its
exports provide incoming and outgoing reference queries, backlinks, common
backlinks, and transitive reachability. The site layer maps those query
results back to page and reference records and preserves site ordering.

A link fragment (`@link["page#node"]`) is not part of the reference row: the
reader splits it off, so `target-page-id` is the page and the relation stays
page-level. Two fragment links to the same page therefore produce two
occurrences but a single backlinked page, and fragments never affect
reachability.

## Forester content and page queries

`(forester site)` exposes two immutable relation snapshots on a Forest.
`(content-node-relation forest)` stores one row for each content element:
`(node-id page-id parent-id kind position properties text character-count codepoint-count plain-text?)`.
`(page-relation forest)` stores one row per page:
`(page-id title metadata)`. The `nodeo` and `pageo` goals query those rows;
`node-propertyo` and `page-metadatao` query the corresponding alists.

Only content elements receive node IDs. IDs are dense non-negative integers,
assigned in preorder across pages in Forest order. They resolve through
`(content-node forest id)` only for that Forest snapshot; another Forest or a
later analysis has no ID stability guarantee. The `position` column is the
child slot in the original content list, so text and layout leaves can create
gaps between element positions even though leaves have no rows or IDs.

The `text` column is the concatenation of descendant text and layout leaves,
without renderer-specific formatting. `character-count` counts Unicode
extended grapheme clusters, the default user-perceived text units;
`codepoint-count` is the Guile string length. UTF-8 byte length is a storage
measure and can be computed from `(string->utf8 text)` when needed.
`plain-text?` means that the element has no child content elements;
text and layout leaves are allowed. Thus `plain-text-paragrapho` selects
paragraphs without nested semantic elements, while `paragrapho` also includes
paragraphs containing links, math, or other inline elements. `contains-texto`
performs a case-sensitive substring search over the concatenated text.

Page title and metadata are a separate relation because the page reader
extracts `@title`, `@summary`, `@published`, and `@updated` into the Grove
page record. Tags and taxa stay in the content IR and are queried with
`topic-membero`. Topics have one lowercase value; ground topic selectors are
case-insensitive and enumerated values are canonical. Page facts and content
facts can be joined through their
shared page ID:

Forester also materializes `@describes` declarations as `(page-id uri)` rows
in `(described-entity-relation forest)`. Use `described-entityo` in relational
queries, or `(describing-pages forest uri)` to get matching page records in
site order. Multiple pages may describe the same external entity; URI strings
are compared exactly.

Because described entities and page data are separate relations, join them
through the page ID. This keeps entity matching composable with other page
facts and content queries:

```scheme
(let ((entities (described-entity-relation forest))
      (page-data (page-relation forest)))
  (query (page-id title)
    (described-entityo entities
      (uri "doi:10.1146/annurev.fluid.29.1.123")
      (page-id page-id))
    (pageo page-data
      (page-id page-id)
      (title title))))
```

```scheme
(let ((nodes (content-node-relation forest))
      (page-data (page-relation forest)))
  (query (node-id)
    (exists (page-id label summary)
      (pageo page-data (page-id page-id))
      (page-metadatao page-data page-id 'summary summary)
      (topic-membero nodes "algebra" page-id label)
      (nodeo nodes
        (node-id node-id)
        (page-id page-id)
        (kind 'paragraph))
      (shorter-than-o nodes node-id 120)
      (contains-texto nodes node-id "范畴论"))))
```

Project modules can export ordinary `define-relation` rules over these explicit
snapshots; no Grove changes are required. Composition queries use the typed API
in `(forester composition)` rather than depending on its private tuple layout.

### Deferred queries in articles

`@forest-query[procedure]` records a Forester query while a page is read. After
all authored pages and the Forest relation snapshots exist, Forester calls the
procedure with `(forest page-id)`. It must return a `content-document`. The
callback can use the page, content-node, and reference relations above, along
with ordinary Scheme to assemble the result. Place the command at the page's
top level; its result may contain ordinary blocks, including links and copied
content nodes.

Use the [linked-notes query example](../examples/linked-notes/plants/orphans.plant)
for a complete authored query.


The query result is inserted at the command's position for rendering. It does
not become source data: generated elements are absent from page, reference,
composition and content-node relations. Generated `@link` elements
are checked against the authored Forest and rendered as links, but do not
create backlinks or alter authored link IDs. Query output cannot recursively
request another Forest query or declare new subtrees. It may return
`transclude` elements to display existing trees: targets must exist and the
resolved display graph must be acyclic. Generated transclusions do not enter
the authored composition relation. Every query runs over one fixed snapshot.

Because a query may depend on any source page or module, rendered pages
containing a query (including a page that embeds such a page as a subtree or
transclusion) depend on all Forest sources for cache invalidation. This is
intentionally conservative.

For finite-set questions such as “pages with no incoming references,” use the
relation results to collect the referenced IDs and take the set difference in
Scheme. `=/=` is an inequality constraint, not general negation over a
relation. By default, incoming references mean authored links; transclusions
are represented separately by the composition API.

## Recursion

Finite facts do not guarantee termination of recursive goals. Grove does not
promise tabling. Forester reachability uses path-local cycle checking, excludes
the starting page, and deduplicates reachable IDs; the site API returns them
in site order. Multiple derivations remain possible in raw relation queries.
