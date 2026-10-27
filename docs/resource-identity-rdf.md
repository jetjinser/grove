<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Resource identity and RDF projection

This is a design study, not a public API commitment. It asks whether IRI can
provide one clear naming layer for Grove's local pages and external entities,
and whether RDF is a faithful projection of Grove's relations.

## The useful unification

An IRI is a name for a resource. An absolute IRI can name an external entity
such as a DOI, or a published Grove page. ASCII-only URIs are already valid
IRIs, so one value model can cover both current DOI and HTTPS identifiers and
future Unicode identifiers. When a protocol accepts only URIs, convert the
IRI at that protocol boundary using RFC 3987's mapping.

This is a unification of **resource identity**, not of every string that
locates or names something:

- A page ID is currently local to a forest. It can be used as a convenient
  local name, but it is not globally meaningful without a forest identity
  policy.
- A route is a retrieval location. Grove already combines `site-url` with the
  output route for canonical HTML links and sitemap locations, so this is a
  natural default page IRI for a published forest. Its durability follows the
  route policy: changing the route changes that default IRI.
- A filesystem path and an artifact path have filesystem/build semantics.
  They remain paths; a `file:` IRI is a conversion for a boundary that needs
  one, not a reason to treat paths as resource IRIs internally.
- A relative IRI reference has no global identity until resolved against a
  base. The base and resolution rule are part of its meaning.

Therefore “all resources use IRIs” is useful only when “resource” means a
globally named semantic resource. It should not erase the distinction between
identity, local address, retrieval route, and filesystem path.

## Candidate identity rules

1. External entity declarations such as `@describes` denote absolute IRIs.
   They identify entities; they do not imply that Grove can retrieve them.
2. Grove page IDs remain local keys for parsing, indexes, and intra-forest
   queries. For a published page, its absolute canonical URL is the default
   global IRI when `site-url` is configured; without it, no absolute page IRI
   is invented.
3. An addressable page with an unstable ID still has a current canonical URL
   and can be represented by that IRI in a snapshot export. Its IRI may change
   when generated IDs or routes change, so it is not a durable cross-forest
   name. Blank nodes are more appropriate for structural resources with no
   published address.
4. The default page IRI can reuse Grove's existing canonical URL rather than
   invent a second namespace. A project that needs identity to survive route
   changes would need a separate stable-identity policy; that is a distinct
   future choice, not a requirement for a useful local RDF view.
5. Preserve the authored IRI spelling for display and serialization. For
   lookup, simple string equality is a safe baseline but can miss equivalent
   spellings. Any stronger comparison is a comparison policy, not a universal
   rewrite: scheme-specific equivalences differ, and RFC 3987 advises keeping
   the original form when normalization is only for comparison.

The remaining design question is whether a stable page IRI should always
follow the public canonical URL, or whether projects that need identity to
survive route changes should be able to supply a separate stable identity.
Using the canonical URL is already coherent for published pages and is the
smaller default; only the stronger persistence guarantee needs another policy.

The current `@describes` implementation is narrower than this candidate model:
it stores the authored value as a string and validates it with Guile's
`(web uri)` parser, requiring a scheme and a host for HTTP(S). It preserves the
spelling and compares values exactly. This is not a complete IRI parser or a
normalization policy: the parser accepts Unicode path characters but rejects a
Unicode hostname. The authoring API therefore continues to describe these
values as URIs until Grove adopts one consistent IRI contract.

## RDF is a projection, not Grove's relation model

RDF gives a useful interchange vocabulary. A triple has an IRI or blank node
as subject, an IRI predicate, and an IRI, blank node, or literal as object.
This lets a page IRI point to an external entity IRI, and lets predicates
distinguish authored links from transclusions. RDF does not require every value
to be an IRI, nor does it define Grove's relation semantics.

A page-level view can use direct triples:

```turtle
<https://flora.purejs.icu/blazek2005-r10-1.html>
  <http://purl.org/dc/terms/subject>
  <doi:10.1146/annurev.fluid.29.1.123> .

<https://flora.purejs.icu/blazek2005-ch03.html>
  <http://purl.org/dc/terms/references>
  <https://flora.purejs.icu/blazek2005-r3-1.html#section-2> .
```

The first predicate uses DCMI's `subject`; the second uses `references`, whose
definition includes resources pointed to by the subject. This direct graph is
useful for exploration, but it collapses repeated links and omits their labels,
source anchors, and order.

For a faithful interchange, keep the direct page-level triple and attach an
occurrence record to the source page. A record carries the fragment-bearing
target, visible label, local source anchor, and position. A transclusion record
similarly carries its target tree, anchor, and position. Blank nodes suffice
when occurrence identity only needs to survive one export; mint occurrence
IRIs only if they must be addressed across exports.

The projection must also keep Grove's relation types distinct: a link is not a
transclusion, containment is not either of those, and `describes` is a claim
about an external entity. A single generic `relatedTo` predicate would erase
meaning. Conversely, the graph-view projection need not preserve every source
detail when that detail is irrelevant to the view.

## Candidate vocabulary mapping

The existing relations do not need a wholesale Grove vocabulary. A small
application profile can reuse established terms where their definitions fit,
and reserve Grove terms for the semantics that are specific to Forester:

| Grove fact | Candidate RDF expression | Assessment |
| --- | --- | --- |
| A tree `@describes` an external entity | `dcterms:subject` | Good general match: the external IRI is a topic of the tree. `schema:about` is an alternative for consumers that model pages as `CreativeWork`; Schema.org declares it equivalent to `dcterms:subject`. |
| Authored page link | `dcterms:references` | Good page-level projection: its definition includes a resource that is pointed to. It intentionally collapses repeated link occurrences. |
| Link occurrence details | Explicit blank-node occurrence record beside the direct `dcterms:references` triple | Carries `rdfs:label`, source anchor, and position per authored link, including repeated links. Include the authored fragment in the target IRI. This requires a small Grove vocabulary for `referenceOccurrence`, `target`, `sourceAnchor`, and `position`. |
| Declared nested subtree | `dcterms:hasPart` | Good match for a separately addressable tree logically included in its parent tree. |
| Transclusion directive | Grove-specific `transcludes` predicate plus an explicit occurrence record | No standard term examined expresses “insert this target tree's content here” without changing it into generic reference, part-whole, or provenance. Keep this operation distinct. The source-to-target edge is useful for graph exploration; the occurrence record preserves anchor and order. |

An explicit occurrence record is clearer here than RDF reification. Reification
describes an RDF statement token and does not itself assert the statement; the
direct triple must still be emitted. It also needs `rdf:type rdf:Statement`
and the three `rdf:subject`/`rdf:predicate`/`rdf:object` properties. An explicit
record needs only a source-to-occurrence edge and its target, source anchor,
position, and optional `rdfs:label`. In the prototype this is four triples per
occurrence plus an optional label, versus six plus an optional label for
reification, before counting the shared direct edge. It also avoids making an
RDF statement look like Grove's authored occurrence.

For example:

```turtle
<https://example.test/page.html>
  <http://purl.org/dc/terms/references> <https://example.test/target.html#section-2> .

<https://example.test/page.html>
  <urn:grove:vocab#referenceOccurrence> _:link-1 .

_:link-1
  <urn:grove:vocab#target> <https://example.test/target.html#section-2> ;
  <http://www.w3.org/2000/01/rdf-schema#label> "Visible link label" ;
  <urn:grove:vocab#sourceAnchor> "ref-1" ;
  <urn:grove:vocab#position> 0 .
```

`urn:grove:vocab#` here is only a placeholder, not a proposed Grove namespace.
The occurrence node is export-local; its blank-node identity need not remain
stable across exports. `position` is zero-based and scoped to the source page.

This mapping does not make `prov:wasDerivedFrom` a substitute for
transclusion. PROV describes transformations or constructions of entities;
Forester's transclusion is an authoring/rendering instruction. Provenance could
describe a generated HTML artifact's dependence on source trees in a separate
build graph, but that is a different relation and export layer.

This is a candidate profile, not a final vocabulary decision. A public
namespace and exact `position` datatype remain open. Page metadata such as
title and summary could use DCMI terms too, but belongs in a separate pass so
the relation mapping does not expand into a general metadata schema.

## What the Flora experiment establishes

A temporary Scheme prototype read Flora's analyzed model and joined each
indexed link occurrence to its source content node. On all 1,789 pages, all
4,600 references matched one-for-one in order; target IDs and visible labels
agreed. The join recovered 1,873 fragment-bearing links. All 8 transclusions
also preserved target tree, source anchor, and per-page order. This establishes
that the current analyzed model has enough information for a lossless
occurrence projection when both relations are joined; neither relation alone
is sufficient.

The candidate graph used current HTML routes as page IRIs and DCMI
`references`/`subject` for direct edges, with blank-node records for occurrence
details and a local `transcludes` predicate. Its 4,600 link occurrences
collapsed to 1,866 distinct direct reference triples, showing why an
occurrence layer is needed. Current routes work as snapshot page IRIs,
including generated `unstable-*` routes, but those names can change when routes
or generated IDs change.

The graph was emitted as N-Triples, serialized to Turtle with Serd from the
project's pinned Guix channel, parsed back to N-Triples, normalized, and
compared. All 24,907 distinct triples matched. The graph includes Chinese link
labels and Flora's `doi:` entity IRI. This validates syntax and lossless
round-tripping for this mapping, not a general consumer's query behavior or a
final public vocabulary. The transclusion and occurrence terms remain
illustrative. `dcterms:hasPart` for declared nested trees is a semantic
candidate; that relation was not included in this prototype pass.

## Current design direction

Treat IRI as the candidate common identity for globally named resources, while
keeping local page IDs and paths distinct. Keep Grove's current typed
relations as the source of truth. The tested mapping uses `dcterms:subject`
for `describes`, `dcterms:references` for authored links, explicit blank-node
records for occurrence details, and a distinct Grove `transcludes` predicate.
`dcterms:hasPart` remains a reasonable candidate for declared subtrees, pending
a separate model-level check. These are export choices, not replacements for
Grove relations or settled API commitments.

Before introducing an IRI record or changing `@describes`, settle the page
identity policy and the comparison contract. A string-only IRI convention may
be enough for authored identifiers; a parsed IRI value is justified when
Grove itself needs validation, relative-reference resolution, or URI
conversion. SRFI 275 is a useful Scheme design reference and reached final
status on 2026-10-15; its standardization does not by itself settle Grove's
resource-identity policy or commit Grove to its API.

## References

- [RFC 3987: Internationalized Resource Identifiers](https://www.rfc-editor.org/rfc/rfc3987.html)
- [RDF 1.2 Concepts and Abstract Data Model](https://www.w3.org/TR/rdf12-concepts/)
- [DCMI `subject`](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/subject/), [`references`](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/references/), and [`hasPart`](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/hasPart/)
- [Schema.org `about`](https://schema.org/about)
- [PROV-O](https://www.w3.org/TR/prov-o/)
- [SRFI 275: URIs, IRIs, and basic paths](https://srfi.schemers.org/srfi-275/srfi-275.html)
