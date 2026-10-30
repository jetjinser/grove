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
`derivative-expression` constructs a symbolic expression, while `differentiate`
returns a calculation report; `derive` also returns a structured report, while
`derive->content` adapts that report for Scheme-authored content. Article writers
use `@derive` for rendered derivations. `partial-expression` pairs with
`partial`, and `antiderivative-expression` pairs with `antiderivative`. Likewise,
`poisson-particular` constructs an expression while `solve-poisson` computes a
particular solution. The corresponding Semantic constructors build values
directly without parsing text.
Formula also accepts `zero(vector)` as the vector-valued zero; code constructing
Semantic values directly can use `vector-zero` from `(grove math semantic)`.

`φ:calculate` is the narrow exact-arithmetic evaluator. `φ:compute` runs the
broader calculation pipeline, including component and spatial operations,
partial derivatives, and simplification. Both return reports; use `calculate`
for supported arithmetic evaluation and `compute` when the expression needs
the domain calculation stages.

Import `(grove math semantic)`, `(grove math formula)`, and
`(grove math notation)` with the respective prefixes. The
[math-derivations example](../examples/showcases/math-derivations/project.scm)
shows executable usage. For direct control, `(grove math calculation)` runs the
same calculations on Semantic values and returns structured reports with
results, diagnostics, derivations, and audit evidence; its checkers return
structured verification results. Formula is the convenient text-reading and
article-writing facade; use the calculation API when code needs to inspect or
compose those results. The calculation module re-exports report, diagnostic,
derivation, verification, and audit accessors used to inspect those results.
Use `(grove math provenance)` to inspect source locations and the origins
attached to derivation steps and diagnostics. For direct presentation, import
`(grove math presentation)` to turn reports, derivations, and verification
results into Content IR. Import focused modules such as `(grove math report)`
when code needs only one layer. Keep the Formula prefix: its `read` helper
would otherwise shadow Scheme's ordinary reader procedure in the importing
module.

`math-finished-report->content` presents a derivation when one exists. A
finished search with no path has no derivation, so the same helper falls back
to the report summary (including its zero-candidate result). For an unfinished
report, it raises by default; pass `#:on-error 'show` to render its diagnostics
instead.

Scheme code can inspect those values through `(grove math report)` and
`(grove math diagnostic)`. `math-report-finished?` means calculation or search
ended without diagnostics; it does not say whether a derivation was found.
`math-report-status` is the primary outcome classification, while
`math-report-reason` gives the detailed stopping reason:

| Status | Meaning |
| --- | --- |
| `'complete` | A derivation was produced with no unresolved symbolic derivatives. This is a completed calculation, not a general proof of a mathematical claim. |
| `'symbolic` | The computation finished, but its reusable result still contains symbolic derivatives. |
| `'no-result` | The search finished without finding a derivation. |
| `'cutoff` | A resource limit stopped the computation; the available prefix is not an exhaustive result. |
| `'unproven` | An equation check could not establish the requested identity. |
| `'error` | The report contains a diagnostic or another unsuccessful outcome. |

`math-report-result` returns a reusable Semantic value when a finished report
has a derivation; that result may still contain symbolic derivatives. It
returns `#f` when no derivation is available or computation stopped early.
`math-report-final` exposes the last term of a derivation, including a partial
one, for inspection. Successful `@check-derive` output says that each step
replays with the selected rules and assumptions. Successful `@check-equation`
output says that the residual reduced to zero under supported rules and
assumptions. These checks do not validate the author's assumptions or an
external mathematical model. Reports from `compute`, `differentiate`,
`partial`, `antiderivative`, and `solve-poisson` also attach a calculation
audit; its replay, semantic-contract, and proof-tree outcomes remain separate.
`check-equation` reports its residual outcome through `math-report-status` and
also exposes an independent polynomial result through
`math-report-equation-check`. That result can remain `semantically-checked`
even if residual reduction hits a resource limit, or be `unsupported` for a
spatial equation. It is separate from generic derivation audit because that
audit checks rewrite steps and could be semantically checked even when the
requested equation is false.

Use
`math-verify-derivation` from `(grove math verification)` to
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

The Formula facade also adapts Semantic operations that are useful when
composing expressions in Scheme: `prime` accepts a function name and a text
operand, while `function-partial` accepts a text application and a one-based
argument index. These produce the same `math-value` terms as the corresponding
Semantic constructors.

`φ:read` reads one expression. `φ:read-lines` reads a multiline sequence,
returning one Semantic value for each nonblank line. `@check-derive` uses this
form so each authored line is one step in the chain.

In Scheme, `φ:read` returns a Semantic value that can be composed with
constructors such as `φ:add` or passed to calculations. `φ:term` deliberately
unwraps that value to the underlying Scheme term for inspection or lower-level
relations. Expression constructors such as `φ:derivative-expression` return Semantic
values; calculation helpers such as `φ:differentiate` and `φ:derive` return
structured reports, while `φ:derive->content` returns Grove content:

```scheme
(use-modules ((grove math formula) #:prefix φ:))

(define expression (φ:read "x^3 + 2*x"))
(define symbolic-derivative (φ:derivative-expression expression "x"))
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

Scheme code can establish the same kind of dynamic scope with
`with-math-assumptions` from `(grove math context)`. Nested scopes add unique
facts and restore the previous scope on exit; per-calculation `#:assumptions`
are combined with the active scope. Use `call-with-math-assumptions` when a
procedure value is more convenient, or pass `#:replace? #t` to start from an
empty scope. `math-assumptions` turns Semantic values and raw fact terms into
the normalized assumption list used by the calculation APIs.

For relational extensions, `(grove math types)` exposes `math-typeo`, which
relates an assumption set, a mathematical term, and its inferred type. Compose
it with other miniKanren goals when writing custom rules. `math-infer-type` is
the direct convenience for a ground term; built-in calculations already check
types, so ordinary Formula and Semantic code does not need to call either.

`(grove math layout)` bridges values and content: `math-value->layout` turns a
Semantic value into ordinary Grove Content IR that custom commands or
renderers can place in a document. `math-term->layout` is the lower-level
version for callers that already have a raw term and type facts.
`math-notation->datum` gives a readable Scheme datum for inspecting a Notation
IR tree while developing an extension.

`(grove math term)` exposes the symbolic term tree as ordinary Scheme lists.
Use `math-term?`, `math-term->source`, and `math-term-size` to inspect terms;
`math-term-paths`, `math-term-at-path`, and `math-term-replace-at-path` support
extensions that traverse or rewrite a subexpression. Paths are zero-based
operand indexes. Lookup returns `#f` for an invalid term or path, and
replacement returns `#f` when its path or replacement term is invalid. Path
enumeration and size reject invalid terms. Normal writing and calculations
should use Formula or Semantic values instead.

## Available calculations

| Operation | Supported scope | Conditions and meaning |
| --- | --- | --- |
| Calculate, simplify, normalize, collect, expand | Exact rational arithmetic and supported scalar expressions. Expansion distributes products over sums; normalization orders scalar sums and products and collects coefficients. | `calculate` is the narrow exact evaluator; these operations do not approximate unsupported functions. |
| Differentiate | Real scalar arithmetic, natural powers, quotients, `sin`, `cos`, `exp`, `log`, and declared smooth unary scalar functions. | Variable roles and domain facts matter: for example, real `log(x)` requires `positive(x)`, and a quotient requires a nonzero denominator. |
| Partial derivative | The scalar differentiation rules, with explicit coordinate and parameter roles. Unknown multivariate functions may remain as symbolic partials. | Repeated or mixed derivatives require matching regularity assumptions; a finished symbolic result can still contain unevaluated derivatives. |
| Vector calculus | Three-dimensional Cartesian `grad`, `div`, `curl`, and `laplacian`; three-component vector operations. | Declare the field types, ordered coordinates, and required regularity. This is not general-dimensional vector or matrix algebra. |
| Antiderivative | Exact rational-polynomial primitives in one coordinate. | Grove selects the primitive whose value is zero at the coordinate origin; it does not return the arbitrary integration term. |
| Poisson particular solution | One exact rational-polynomial particular solution of `Δu = source` in a declared three-dimensional Cartesian space. | Grove selects a solution by setting its value and first derivative to zero on one coordinate axis; this is not the general solution or a boundary-value solver. |
| Equation check | Computes and simplifies the residual between both sides. | A zero residual establishes the identity only within supported rules and supplied assumptions. A nonzero or unresolved residual is reported as unproven. |

Use `@compute` for component and spatial calculations, `@antiderivative` for a
polynomial primitive, `@solve-poisson` for the particular Poisson solution,
and `@check-equation` to check a candidate. The corresponding Formula API is
available under `φ:`; Semantic constructors are under `◆:`.

## Derivations and checks

`(grove math derivation)` represents a proposed sequence of terms and rule
steps. Construction records the proposal; it does not prove the steps.
`math-verify-derivation` from `(grove math verification)` replays each step
against its selected registered rule. `(grove math proof)` provides a separate
proof-tree API: proof nodes can refer to assumptions, apply registered rule
instances, or compose equalities by reflexivity, symmetry, transitivity, and
congruence. `math-proof-check` checks a proof tree under an ordered assumption
list. `math-proof-elaborate-derivation` tries to turn a derivation into such a
checked proof and returns a report when steps are unsupported or rejected.

For example, reflexivity is a complete proof of an expression being equal to
itself:

```scheme
(use-modules (grove math proof))

(math-proof-result-status
  (math-proof-check (math-proof-reflexivity '(var x)) '()))
;; => checked
```

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
Constructing a derivation records proposed evidence but does not certify it;
use `math-verify-derivation` before trusting a caller-supplied chain.

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
`math-rules-stepo`. Built-in `math-*-rules` packs are lists of rule records
and can be passed directly. `math-rewrite-answers` instead selects registered
rules by name:

```scheme
(use-modules (grove math rule) (grove relation))

(define double-rule
  (math-rule 'double "double(X) = X + X" "all values" 'custom
    (facts x) (double ,x) (+ ,x ,x) () ()))

(run* (output name)
  (math-rules-stepo (list double-rule) '()
    '(double (var x)) output name 'forward))
```

Each `math-rule` declaration also carries a `math-rule-spec`. Inspect its
left side, right side, premises, and guards with the `math-rule-spec-*`
accessors from `(grove math rule)`. `math-rule-spec-guard-kinds` identifies
guards Grove understands; `math-rule-spec-opaque-guards` returns the remaining
forms for custom tooling to interpret.

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
steps. Long step lists appear in a collapsed disclosure; Forester exposes that
disclosure as an unnamed subtree. If a rule cannot apply or a required premise
is missing, `#:on-error 'show` displays the calculation diagnostic in the
article. Authored Formula expressions retain
source locations through parsing and computation where possible, so diagnostics
can point back to the relevant source text.

## Independent polynomial checks

The ordinary derivation verifier replays Grove's registered rewrite relations.
For a deliberately smaller domain, the independent semantic checker verifies
polynomial equations using its own exact sparse-polynomial normalizer. The
checker can validate the registered scalar laws, check an individual rewrite
step and its recorded path, compare a computed polynomial derivative or
partial derivative with an independently computed derivative, or check
whether two scalar polynomial expressions have the same exact meaning. It can
also check polynomial antiderivatives and Poisson particular solutions. The
Poisson checker verifies `Δu = source` and Grove's selected-axis zero-value and
zero-first-derivative normalization. Import `(grove math semantic-check)` for
these checks and their typed evidence values. Evidence constructors and
accessors belong to this module, so extensions inspecting or validating a
checker result have one explicit import. `math-report-equation-check` carries
this result on reports from `check-equation`; the checker procedures and
evidence types remain in `(grove math semantic-check)`. Formula exposes these
helpers under the same `φ:` prefix for use in articles: `check-polynomial-step`,
`check-polynomial-derivative`, `check-polynomial-antiderivative`, and
`check-polynomial-poisson` parse
formula strings before calling the typed checker; `check-polynomial-rule` and
`check-polynomial-derivation` delegate values directly. The
`check-polynomial-equation` helper independently compares two polynomial
expressions without using the residual derivation. It returns
`semantically-checked` for equal polynomial meanings, `rejected` for unequal
meanings, and `unsupported` when either side lies outside the checker domain.
This result checks the equation itself, unlike a generic calculation audit,
which checks evidence about how a derivation proceeded.
`check-polynomial-cartesian` provides the same independent check for explicit
three-dimensional Cartesian components: `grad`, `div`, `curl`, and scalar or
componentwise vector `laplacian`. Its `#:axes` argument is an ordered list of
three distinct coordinate symbols (defaulting to `(x y z)`); vectors must be
written as `components(x,y,z)`. For example,
`(φ:check-polynomial-cartesian 'curl "components(x*y,y*z,z*x)"
"components(-y,-z,-x)")` checks the right-handed curl formula. The checker
only claims exact results for rational polynomials and returns `unsupported`
for other expressions; it does not infer geometric assumptions or handle
curvilinear coordinates.
`polynomial-check->content` adapts the typed result to Content IR through the
presentation layer. These are conveniences, not a second checker API. Import
`(grove math semantic-check)` when Scheme code needs typed evidence or works
directly with Semantic values, and `(grove math presentation)` when it only
needs report/check presentation. For example:

```scheme
@(φ:polynomial-check->content (φ:check-polynomial-rule 'add-commutative))
@(φ:polynomial-check->content
   (φ:check-polynomial-derivative "x^3+2*x" 'x "3*x^2+2"))
```

For a custom `RuleSpec` created with `make-math-rule-spec` from
`(grove math rule)`, call `math-polynomial-check-rule-spec` directly. It checks
the specification as a universal identity in the same scalar theory without
registering the rule. This is useful for validating authored rule data before
running it through a relational extension. Symbolic denominators require a
matching declared `nonzero` premise; opaque guards do not extend the checker's
proof domain.

Its domain is scalar polynomials over the rationals, including division by a
nonzero rational constant. The Poisson contract checker additionally takes the
three ordered axes from a declared Cartesian `space` assumption. Expression-
valued denominators, functions, general spatial rewrite rules and
algorithm-wide correctness proofs are unsupported. The
normalizer also has explicit expansion limits: at most 256 distinct monomials,
total degree 64, and exponent 128. The checker relies on the correctness of its
own small Scheme implementation; it is independently implemented from the
rewrite relations, but is not itself formally verified. A rule check proves
the equation in the rule's existing rewrite schema, specializing declared
zero-valued operands to scalar zero. It does not add a second public rule-law
representation. Tests compare each supported polynomial schema's generated
relation with the independent checker using variable and compound substitutions.
These finite cases provide regression coverage, but do not establish
implementation equivalence for every possible input. For each rewrite step,
the ordinary checker replays the registered relation, while the polynomial
checker checks the recorded path and whole-term
denotation independently. It reuses Grove's type inference to establish that
the supplied assumptions make both terms scalar. For polynomial derivative
certificates, the same derivation record retains the rule, path and endpoints;
the ordinary verifier replays those steps, then the independent checker
compares the final term with its own exact derivative. For a Poisson derivation,
the audit independently checks the final polynomial against the Laplacian
equation and selected-axis normalization. This checks a specific certificate,
not the underlying program for all inputs. Assumptions remain
author-supplied inputs.

These checks answer different questions, so read their results separately.
`math-verification-valid?` means every recorded transition passed replay under
the selected rules and assumptions. A semantic-check status of
`semantically-checked` means the checker established the contract within its
supported domain; `rejected` means it found a counterexample or malformed
evidence; `unsupported` means the evidence was outside that checker's domain or
capabilities and makes no claim either way. Proof elaboration reports use the
parallel statuses `checked`, `rejected`, and `unsupported`. A calculation audit
keeps replay, contract, and proof results side by side rather than collapsing
them into one boolean: callers can choose which evidence their application
requires, and an unsupported result does not contradict positive evidence from
another layer.

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
