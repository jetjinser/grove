<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Composition model

Grove aims to make extensions inspectable before they run. A useful extension
has a clear value representation, a named way to combine values, an explicit
order or precedence, and an interpreter that gives the value meaning.

This does not require every extension to use one universal composition
protocol. Grove uses ordinary Scheme values where their behavior is already
clear, and typed records where construction-time invariants or domain meaning
justify a distinct type.

## Composition laws in use

| Concept | Value | Combination | Interpretation |
| --- | --- | --- | --- |
| Content | constructors and records in `grove ir` | build a tree; there is no general merge | a renderer interprets the content algebra |
| Content transform | ordinary procedure: document → document | `compose-content-transforms` applies procedures left to right; `identity` is the unit | `transform-document` checks the input and result document |
| Render rules | typed map from content kind to procedure | `compose-render-rules` layers an overlay over a base; `next` explicitly reaches the fallback | HTML, text, and gemtext interpreters |
| HTML presentation | typed product of style, rules, head, navigation, and page fragments | `compose-html-presentations` combines each field according to its own law | HTML renderer |
| Derived artifacts | ordered list of typed artifact producers | ordinary list operations such as `append` | `render-derived-artifacts` |
| Render context | typed wrapper around an association list | `render-context-with` overlays keys; present `#f` remains distinct from absence | rules, renderers, and derived artifacts |
| Asset pipelines | typed producer with name, version, inputs, and procedure | project order; outputs retain declared ownership and dependencies | build pipeline with content-addressed caching |

An HTML presentation is a product of distinct composition laws: styles and
head/navigation nodes append in order, later rules override and may delegate,
page fragments append by page ID, and the rightmost declared home link wins.
Keeping these rules visible makes the combined value predictable without
pretending that all of its fields have identical semantics.

## Native Scheme interoperability

The public boundary should use ordinary procedures for transformations,
ordinary lists for ordered collections, association lists for small keyed
environments, and SRFI-9 records for values with invariants. That lets project
code use familiar `map`, `append`, `fold`, `filter`, and `assoc` operations,
and use Guile pattern matching when inspecting public records. Grove-specific
combinators remain where they clarify domain behavior or enforce a boundary;
they should not wrap a procedure or list without adding meaning.

## Keep different semantics distinct

- A content transform rewrites a document before interpretation; a render rule
  interprets one content kind during rendering. They are both procedures, but
  have different signatures, order, and call context.
- A rule overlay is not a right-biased map merge: `next` preserves the lower
  interpretation as an explicit fallback.
- A context is an environment, while a presentation is an output policy. Their
  records and composition should remain separate even though both contain
  associations internally.
- Asset pipelines and filesystem effects declare dependencies and produce
  artifacts. They belong to the build interpreter, not to a pure value
  composition protocol.
- The content algebra remains a free set of constructors; sequencing it as a
  monad would conflate content structure with rendering order.

## Design checks

When adding a customization point, ask:

1. Is an ordinary Scheme value sufficient, or does a record add useful
   validation, identity, or interpretation?
2. What is its identity, if an empty value is meaningful?
3. Is combination append, ordered function composition, override, or explicit
   fallback? State the order in the API and test associativity where it should
   hold.
4. What inputs determine its cache identity, and who evaluates it?
5. Can callers use SRFI and Guile tools directly, or is a Grove wrapper doing
   real semantic work?

Keep the render handler alist private unless a project needs to replace
dispatch itself; render rules already cover element-level extension. Consider
shared image-variant or encoder policy only when multiple consumers need the
same contract.
