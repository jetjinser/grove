<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Math reference

Grove math lets an article use one expression for display and computation. It
supports a focused set of symbolic rewriting and calculus operations; it is not
a general-purpose computer algebra system.

## Three interfaces

Grove separates formula text, mathematical meaning, and visual layout:

- **Formula DSL (`φ:`)** reads convenient text such as `x^3 + 2*x` into a
  semantic expression. It can also embed semantic values for constructs that
  are awkward to write as text.
- **Semantic DSL (`◆:`)** constructs expressions and declarations that have
  meaning to calculations.
- **Notation DSL (`◇:`)** constructs display-only layout, such as fractions,
  matrices, and aligned equations. Notation does not become solver input.

Article commands accept semantic expressions, and Formula helpers wrap the
same semantic operations for text input.

Formula helpers distinguish constructing an expression from calculating it:
`derivative` constructs a symbolic expression, while `differentiate` returns a
calculation report; `derive` also returns a structured report, while
`derive->content` adapts that report for Scheme-authored content. Article writers
use `@derive` for rendered derivations. `partial-expression` pairs with
`partial`, and `antiderivative-expression` pairs with `antiderivative`. Likewise,
`poisson-particular` constructs an expression while `solve-poisson` computes a
particular solution. The corresponding Semantic constructors build values
directly without parsing text.

`φ:calculate` is the narrow exact-arithmetic evaluator. `φ:compute` runs the
broader calculation pipeline, including component and spatial operations,
partial derivatives, and simplification. Both return reports; use `calculate`
for supported arithmetic evaluation and `compute` when the expression needs
the domain calculation stages.

Import `(grove math semantic)`, `(grove math formula)`, and
`(grove math notation)` with the respective prefixes. The
[math-derivations example](../examples/showcases/math-derivations/project.scm)
shows executable usage. For direct control, `(grove math pipeline)` runs the
same calculations on Semantic values and returns structured reports with
results, diagnostics, derivations, and audit evidence; its checkers return
structured verification results. Formula is the convenient text-reading and
article-writing facade; use the pipeline when code needs to inspect or compose
those results. Keep the Formula prefix: its `read` helper would otherwise
shadow Scheme's ordinary reader procedure in the importing module.

Scheme code can inspect those values through `(grove math report)` and
`(grove math diagnostic)`. `math-report-result` from `(grove math pipeline)`
returns a reusable Semantic value only for a complete calculation;
`math-report-final` exposes the final term of a partial derivation for
inspection. Use `math-verify-derivation` from `(grove math verification)` to
replay a certificate, or `math-audit-calculation` from `(grove math audit)` to
combine replay, the independent polynomial contract check, and proof evidence.

Formula readers and Semantic constructors share one value type: a `math-value`
contains a mathematical term together with its source origin, presentation
properties, declarations, and domain requirements. Formula and Semantic are
different ways to construct that value, not separate expression models. The
Semantic module re-exports its accessors. For a custom extension that needs to
construct a term Grove does not provide a named constructor for, use
`make-math-value` from `(grove math expression)` and attach any declarations or
requirements the calculation needs. Prefer the named Semantic constructors
when they express the intended concept; the lower-level constructor checks the
term's shape, but cannot establish that a custom operator has supported
calculation semantics.

Formula text accepts identifiers, integer numerals, function calls,
parentheses, unary minus, powers, equations, and the operators `+`, `-`, `*`,
`/` (also `−`, `×`). It follows conventional precedence. Numeric coefficients
may omit `*`: `6x`, `6x^2`, `6(x+1)`, `6sin(x)`. Other adjacent identifiers
remain one name (`xy`), and `f(x)` is a function call. Ambiguous `1/2x` is
rejected; write `(1/2)*x` or `1/(2x)`.

`φ:read` reads one expression. `φ:read-lines` reads a multiline sequence,
returning one Semantic value for each nonblank line. `@check-derive` uses this
form so each authored line is one step in the chain.

In Scheme, `φ:read` returns a Semantic value that can be composed with
constructors such as `φ:add` or passed to calculations. `φ:term` deliberately
unwraps that value to the underlying Scheme term for inspection or lower-level
relations. Expression constructors such as `φ:derivative` return Semantic
values; calculation helpers such as `φ:differentiate` and `φ:derive` return
structured reports, while `φ:derive->content` returns Grove content:

```scheme
(use-modules ((grove math formula) #:prefix φ:))

(define expression (φ:read "x^3 + 2*x"))
(define symbolic-derivative (φ:derivative expression "x"))
(define report
  (φ:differentiate expression "x" #:assumptions '("coordinate(x)")))
(φ:term symbolic-derivative) ; inspect the underlying Scheme term
```

## Meaning and assumptions

Expressions are checked against their declared types and computational
domain. Numbers are scalars; variables need an explicit role such as
`coordinate(x)`, `parameter(a)`, `scalar(f)` or `vector(E)`. A coordinate is an
independent variable; a parameter is held constant during differentiation. A
function declaration gives its input and result types, but does not define its
body or derivative. Unknown calls may still be displayed, but calculations
need supported semantics for them.

Use `@with-assumptions` to make facts available in a local article scope. Facts
can also be passed to individual commands with `#:assumptions`.

```scheme
@with-assumptions["coordinate(x); parameter(a); positive(x)"]{
  @differentiate["x"]{exp(a*x) + log(x)}
}
```

This computes `a*exp(a*x) + 1/x`. Real quotients require a nonzero denominator;
real logarithms require a positive argument. The system discharges simple
facts such as positivity implying nonzero, but does not infer general truths.
Assumptions are assertions by the article author, not facts proved by Grove.

## Available calculations

| Operation | What it supports |
| --- | --- |
| Calculate, simplify, normalize, collect, expand | Exact arithmetic and supported scalar expressions; expansion distributes products over sums. Normalization orders scalar sums and products and collects coefficients. |
| Differentiate | Real scalar arithmetic, natural powers, quotients, `sin`, `cos`, `exp`, `log`, and declared smooth unary scalar functions. |
| Partial derivative | The scalar differentiation rules, with explicit coordinate and parameter roles; unknown multivariate functions can remain as symbolic partials under supported regularity assumptions. |
| Vector calculus | Ordinary three-dimensional Cartesian `grad`, `div`, `curl`, and `laplacian`, with declared fields, ordered coordinates, and regularity assumptions. |
| Component computation | Three-component vectors and supported scalar/vector arithmetic and spatial operations in an explicit Cartesian space. |
| Antiderivative | Exact rational polynomial primitives in one coordinate, selected to be zero at coordinate zero. |
| Poisson particular solution | A polynomial particular solution of `Δu = source` in an active Cartesian space, selected along one axis and checked by its residual. |
| Equation check | Computes and simplifies a residual. A zero residual certifies the equation within supported rules; nonzero or unresolved residuals are not proven. |

Use `@compute` for component and spatial calculations, `@antiderivative` for a
polynomial primitive, `@solve-poisson` for the particular Poisson solution,
and `@check-equation` to check a candidate. The corresponding Formula API is
available under `φ:`; Semantic constructors are under `◆:`.

## Derivations and checks

`@derive{expression}` applies available rewrite rules and displays each step.
Transitions identify their rule and the location within the expression where
they applied. Supply assumptions in its argument when required:

```scheme
@derive[#:assumptions "vector(E); smooth(E); div(E)=0"]{curl(curl(E))}
```

`@search-derive` searches within configured bounds for a path to a target.
Scheme callers use `φ:search source target`; set `#:backward? #t` to traverse
from the target toward the source without changing the argument order.
`@check-derive` checks a hand-written sequence against the registered rules.
`@check-equation` instead checks an equation by reducing its residual. These
checks establish only what the supported rules and calculation domain justify;
they do not prove assumptions or validate an external mathematical model.

Write one expression per line inside `@check-derive`; blank lines are ignored:

```scribble
@check-derive[#:view 'full]{
  differentiate(log(x),x)
  differentiate(x,x)/x
  1/x
}
```

Derivation and step records snapshot their term trees, paths, assumptions, and
ordered collections. Accessors return independent copies, so changing a
displayed trace cannot alter a certificate before it is checked. Opaque source
provenance values remain shared according to their own contract.

Semantic values follow the same data rule: construction snapshots the term,
presentation properties, declarations, and domain requirements; their
accessors return independent copies. The source-origin record is shared as
opaque provenance data.

Calculation reports likewise snapshot their candidate, limit, pass, and
diagnostic collections. Diagnostic paths, premises, and messages, along with
source-location filenames and excerpts, are copied when constructed and when
read. The records inside these collections retain the contracts of their own
types; for example, derivations snapshot their terms and steps separately.
Verification, semantic-check, and proof reports apply the same rule to their
diagnostics, proof terms, premises, errors, and step collections. This keeps
the recorded outcome stable when callers inspect or reuse its evidence.

The registered rules are also available as relational data through
`(grove math catalog)`. Import `(grove relation dsl)` to query rules by group,
execution mode, input operator, required premise, or semantic-check status.
`math-rule->content` renders a rule's declaration and guards as ordinary Grove
content. The [inference-rules example](../examples/showcases/math-derivations/plants/inference-rules.plant)
builds a page from these queries, using the same registry as the rewrite
engine rather than maintaining a separate catalog.

## Custom relational rules

`(grove math rule)` also provides `math-rule` and `math-rules-stepo` for direct
relational extensions. `math-rule` declares a rule from a pattern, premises,
and relational goals; `make-math-rewrite-rule` accepts a relation procedure
for rules that need algorithmic code. Pass an explicit rule list to
`math-rules-stepo`:

```scheme
(use-modules (grove math rule) (grove relation))

(define double-rule
  (math-rule 'double "double(X) = X + X" "all values" 'custom
    (facts x) (double ,x) (+ ,x ,x) () ()))

(run* (output name)
  (math-rules-stepo (list double-rule) '()
    '(double (var x)) output name 'forward))
```

This is a lower-level relational interface. The high-level `math-*` operations,
Formula helpers, and derivation verifiers select rule names from Grove's
registered rule set; a locally constructed rule is not added to that registry
or its catalog, so it cannot participate in their report and verification
pipeline. This keeps report step names resolvable against the same rule
definitions used by the catalog and verifier. The built-in rule-pack modules
also expose rule lists for direct relational queries: `(grove math rewrite)`
provides the complete set, while `(grove math arithmetic)`,
`(grove math differentiation)`, `(grove math identities)`,
`(grove math integration)`, `(grove math normalization)`,
`(grove math partial)`, `(grove math components)`, `(grove math spatial)`, and
`(grove math vector)` expose domain-specific packs.
Loading a pack does not register or replace high-level rules. Use this API when
project code wants to run or compose a custom relation directly.

Rule declarations and RuleSpec values snapshot their string and pair-based
data at construction. Their accessors return independent copies, so inspection
cannot change the registered rule or its verification behavior. Relation and
matcher procedures remain shared extension code.

Derivation commands can show the result alone, a compact view, or the full
steps. Long step lists are presented in a collapsible subtree. If a rule cannot
apply or a required premise is missing, `#:on-error 'show` displays the
calculation diagnostic in the article. Authored Formula expressions retain
source locations through parsing and computation where possible, so diagnostics
can point back to the relevant source text.

## Independent scalar-polynomial checks

The ordinary derivation verifier replays Grove's registered rewrite relations.
For a deliberately smaller domain, the independent semantic checker verifies
polynomial equations using its own exact sparse-polynomial normalizer. The
checker can validate the registered scalar laws, check an individual rewrite
step and its recorded path, or compare a computed polynomial derivative with an
independently computed derivative. For example:

```scheme
@(φ:polynomial-check->content (φ:check-polynomial-rule 'add-commutative))
@(φ:polynomial-check->content
   (φ:check-polynomial-derivative "x^3+2*x" 'x "3*x^2+2"))
```

Its domain is scalar polynomials over the rationals, including division by a
nonzero rational constant. Expression-valued denominators, functions, vectors,
spatial rules and algorithm-wide correctness proofs are unsupported. The
normalizer also has explicit expansion limits: at most 256 distinct monomials,
total degree 64, and exponent 128. The checker relies on the correctness of its
own small Scheme implementation; it is independently implemented from the
rewrite relations, but is not itself formally verified. A rule check proves
the equation in the rule's existing rewrite schema, specializing declared
zero-valued operands to scalar zero. It does not add a second public rule-law
representation. The rule relation is generated from the same schema, but
guards and procedural details still need testing. Tests replay each supported
polynomial schema on variable and compound substitutions; those finite tests
do not establish implementation equivalence for every possible input. For an
individual rewrite step, the ordinary checker replays the registered
relation, while the polynomial checker checks the recorded path and whole-term
denotation independently. It reuses Grove's type inference to establish that
the supplied assumptions make both terms scalar. For polynomial derivative
certificates, the same derivation record retains the rule, path and endpoints;
the ordinary verifier replays those steps, then the independent checker
compares the final term with its own exact derivative. This checks a specific
certificate, not the derivative program for all inputs. Assumptions remain
author-supplied inputs.

## Display notation

Notation lowers to MathML and is not parsed back into a Semantic expression.
Compute with Semantic values, then lower the result for display. Display-only
fractions, matrices, and alignments carry no implied calculation semantics.

`@tex-math`, `@tex-display-math`, `@◇:math`, and `@◇:display-math` accept
`#:id`, which becomes the container's HTML anchor (`eq-2-12` in
`@tex-display-math[#:id "eq-2-12"]{…}`) so a `@link["page#eq-2-12"]` can scroll
to the equation. The ID must be non-empty and contain neither whitespace nor
`#`; a style cannot replace it. Injecting that anchor is a separate concern and
does not change the math semantics above.

## Current boundaries

Unsupported expressions, missing declarations, invalid types, and unmet domain
requirements produce diagnostics. Search and calculation passes are bounded.

Polynomial integration uses exact rational coefficients and natural powers.
The returned primitive fixes its value to zero at the selected coordinate's
origin; a general primitive may additionally contain any coordinate-independent
term. The Poisson operation returns one selected polynomial particular
solution, not the family of solutions, boundary-value solution, or homogeneous
part. General symbolic integration, PDE solving, and general matrix algebra are
outside the current system.
