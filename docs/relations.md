<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove relations

Grove's relation layer uses miniKanren goals to query finite facts and to
compose domain rules. faster-minikanren is the implementation used by Grove;
the public Grove module owns the relation API and its documented contracts.

## Relation values

A relation is an immutable, finite snapshot of ground fact rows. Each row is a
proper Scheme list containing one or more terms, and every row in one relation
has the same arity. A term can be an atomic Scheme value or a pair tree of
terms. Vectors are reserved by faster-minikanren for logic variables and are
not valid relation data. A relation cannot contain a live logic variable.

Base facts have set semantics: structurally equal rows occur once. The
constructor retains the first occurrence order, and `relation-facts` exposes
that order. Relation arity is part of the relation value and is checked when a
goal runs. An empty relation defaults to arity two for convenience; pass
`#:arity` to `make-relation` to construct an empty relation with another arity.

The public construction and query forms are intended to be:

```scheme
(define references
  (make-relation
    '(("intro" "algebra" #f)
      ("limits" "algebra" "continuity"))
    #:arity 3))

(run* (source fragment)
  (relationo references source "algebra" fragment))
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
  (source-page reference-id target-page fragment label))

(define-relation (incoming-referenceo references target source)
  (referenceo references
              (source-page source)
              (target-page target)))

(query (source)
  (incoming-referenceo references "algebra" source))
```

These forms expand to faster-minikanren relations, `fresh`, `conde`, and
`run*`; `project` exposes miniKanren's standard host-language projection and
requires its projected terms to be ground. They add no new evaluation engine.
`define-relation-schema` declares
the tuple columns once, generates a constructor with fixed arity, and creates a
named query macro. Query clauses name columns, so omitted columns become
independent anonymous logic variables. Unknown or repeated columns are
rejected during macro expansion. For example, a fragment query can constrain
`(target-page "algebra")` and `(fragment "continuity")` without naming the
other three columns. Terms are ordinary Scheme expressions in lexical scope.
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
of `(forester site)`. It stores each reference occurrence as a five-column
row: `(source-page-id local-reference-id target-page-id fragment label)`. Its
exports provide incoming and outgoing reference queries, backlinks, common
backlinks, and transitive reachability. The site layer maps those query
results back to page and reference records and preserves site ordering.

Forester tree composition has a separate semantic interface in
`(forester composition)`. That module accepts typed subtree-definition and
transclusion facts, then uses Grove's relation layer internally. Forester's
site analyzer and renderers do not depend on its tuple layout or miniKanren
queries. See [`forester-composition.md`](forester-composition.md) for the
composition contract.

Rules are ordinary Guile module definitions built from miniKanren goals. A
library can export its own relation queries without changing Grove. Scheme
remains available for I/O, model construction, and rendering; the relation
API defines the common logic-query model.

## Recursion and tabling

Recursive rules are ordinary miniKanren relations and must define their own
termination behavior. The base relation is finite, but this alone does not
make every recursive query terminate. Grove's core relation contract does not
promise tabling. A future tabling facility must define how it handles
constraints, duplicate answers, recursive cycles, and the lifetime of tables;
it can be added as an explicit extension without changing ordinary goal
semantics.

Forester reachability currently uses path-local cycle checking. A recursive
branch cannot visit the same page twice, so cyclic graphs terminate and a
page is never reported as reachable from itself, including when a cycle
returns to the starting page. Multiple paths can still derive the same page;
the public Forester query removes duplicate page IDs, and the site API returns
pages in site order. This is an explicit finite-graph rule, not general
tabling or a fixed-point facility. Grove keeps tabling out of core until an
extension needs broader recursive semantics; such an extension can define
table scope, answer deduplication, and constraint behavior without changing
this relation API.
