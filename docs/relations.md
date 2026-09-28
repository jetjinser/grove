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
      ("limits" "algebra" "continuity"))))

(run* (source fragment)
  (relationo references source "algebra" fragment))
```

Rows are tuples, so nested lists remain available as individual terms. The
relation goal accepts one logic argument per row column; callers use ordinary
miniKanren variables and compose relation goals with `conde`, `=/=`, and other
supported goals.

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
