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
| Content style | optional class name and renderer-specific properties | `merge-content-style` joins names and appends properties; the empty style is the unit | content elements and format-specific renderers |
| Content transform | ordinary procedure: document → document | `compose-content-transforms` applies procedures left to right; `identity` is the unit | composition checks the document at every stage; `transform-document` checks one transform's input and result |
| Render rules | typed map from content kind to procedure | `compose-render-rules` layers any number of sets left to right; the empty set is the unit, `next` explicitly reaches the fallback | HTML, text, and gemtext interpreters |
| HTML presentation | typed product of style, rules, head, navigation, and page fragments | `compose-html-presentations` combines any number of layers left to right; the empty presentation is the unit | HTML renderer |
| Site outputs | ordinary renderer procedures or cached renderer values | ordered renderer list | project build |
| Render context | typed wrapper around an association list | `render-context-with` overlays keys; present `#f` remains distinct from absence | rules and renderers |
| Asset pipelines | typed producer with name, version, inputs, and procedure | ordered independent producers; each producer may emit several artifacts | build pipeline with content-addressed caching |

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

## Shared values and mutation

Grove Content, Site, Relation, project-context, HTML-presentation, and Forester
composition values snapshot mutable strings and pair structure at construction;
their accessors return independent copies. This keeps records stable while
allowing callers to use ordinary Scheme lists and strings. Opaque records,
procedures, and other extension values remain shared and follow their own
type's contract.

A generic analyzer's model is different: Grove does not know its type, so the
same model value is passed to every renderer and browser callback. Treat that
model as read-only after analysis, or derive a new model in an earlier
transform. Mutating a shared model can make later renderers depend on renderer
order. Grove-owned Content and Site values remain snapshots; use constructors
such as `content-with-property` and `content-with-style` to derive intentional
changes.

Project configuration snapshots declared paths and list containers because
they define build identity. Callback procedures and opaque extension values
remain shared so projects can compose ordinary Guile code without a wrapper
protocol.

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
  composition protocol. Pipeline order is execution order, not a dependency
  graph: one pipeline cannot declare another pipeline's emitted artifact as an
  input. Pipeline artifacts take precedence over same-path renderer artifacts;
  duplicate paths emitted by two pipelines fail artifact validation. Compose
  dependent stages inside one producer procedure, or declare a real source
  file as the shared input to independent producers.
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
