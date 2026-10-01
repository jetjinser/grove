<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Math processing

Scalar zero is `(num "0")`; vector zero is the distinct term `(zero vector)`.
The source spelling `zero(vector)` produces vector zero and renders an accented
zero in MathML. Addition, subtraction and equations require matching types;
numeric `0` does not silently become a vector. Divergence-free rewriting now
produces vector zero, and derivation verification checks type preservation at
every step.

Automatic simplification removes a matching typed zero from either side of
addition and the right side of subtraction, cancels double negation, and removes
scalar one from either side of multiplication. These rules apply to scalar
and vector terms and strictly reduce term size. They are forward simplifications;
backward search does not insert zero/one decorations by default. Each transition
still records its rule and operand path.

Grove keeps mathematical meaning separate from mathematical presentation.
Semantic DSL in `(grove math semantic)` constructs reusable `math-value`
objects: mathematical terms with source origins and presentation preferences
kept alongside them. Notation DSL in `(grove math notation)` constructs
layout nodes for display-only notation. Both render as native MathML; only
semantic expressions enter computation. The document commands `@math` and
`@display-math` use Semantic DSL by default:

```text
text and embedded operands → syntax tree → mathematical value → passes
                                                           ↓
                                             Notation constructors → layout IR → MathML
```

`(grove math syntax)` tokenizes a deliberately small expression grammar and
parses it with source spans. `(grove math term)` maps that syntax tree to
Scheme s-expressions such as `(curl (curl (var E)))`; this is the term grammar
used by relational rules. `(grove math rewrite)` defines miniKanren relations
over those terms. `(grove math strategy)` selects rules and implements bounded
search and bounded normalization; `(grove math derivation)` stores their
certificates. `(grove math layout)` lowers terms through the pure Notation
constructors in `(grove math notation core)`. This layer depends only on the
content IR: it does not parse text or inspect mathematical meaning. The
author-facing `(grove math notation)` adapter decodes text and embeds semantic
values through the layout pass, then calls those same constructors. Thus both
entry points share one presentation vocabulary without a dependency cycle.
Mathematical terms remain the source of truth; notation is generated for
display and is never interpreted backwards as solver input.
`(grove math pipeline)` composes the computation passes.

There are three separate contracts: `math-term?` checks representable structure;
type inference checks scalar/vector/proposition meaning under explicit facts;
each computation checks its supported domain. An unknown application such as
`sin(x)` remains representable and displayable, but computation, search and
verification return `unsupported-expression`. Missing declarations and
incompatible operand types return `type-error`. Supported terms with no selected
rewrite remain normal forms; that does not mean all their operators were
numerically evaluated. Polynomial differentiation has a narrower domain and
identifies the unsupported subexpression, including its authored location.

`(grove math operator)` shares reserved arities, type signatures and infix display
symbols between structural validation, relational typing, diagnostics and
notation lowering. Adding a signature does not implement a calculation rule;
algorithms and special layout remain independent passes.

| Entry point | Accepted computation domain |
| --- | --- |
| `math-rewrite`, forward/backward search, chain verification | Typed terms in the registered expression vocabulary; selected rules determine possible transitions. |
| `math-calculate` | The same vocabulary; rewrites supported exact arithmetic subexpressions, retaining symbolic operands. |
| `math-collect` | The same vocabulary; collects supported scalar coefficients, retaining other subexpressions. |
| `math-simplify` | The same vocabulary; applies the selected typed identity, arithmetic and collection rules. |
| `math-differentiate` | Scalar polynomials over declared independent scalar coordinates, with exact constants and natural-number powers; differentiation is followed by simplification. |

The semantic parser accepts identifiers, integer numerals, function calls,
parenthesized groups, unary minus, power notation (`x^2`),
and `=`, `+`, `-`/`−`, `*`/`×`, and `/` with conventional precedence.
Embedded Semantic DSL objects are atomic operands, not strings to reparse.
Implicit multiplication and ambiguous subscripts are rejected. Subscript
layout and aligned tables belong to Notation DSL; an indexed mathematical
operation will need its own explicit semantic definition.

`@derive{...}` applies available relations repeatedly to the expression and
shows each intermediate term, joined with equality signs. Each transition is
annotated with the rule name and the operand path where it matched (`root`, or
one-based operand positions). Its bracket argument declares semicolon-separated
assumptions. For example,
`@derive[#:assumptions "vector(E); smooth(E); div(E)=0"]{curl(curl(E))}` declares `E` to be
a smooth vector field with zero divergence. The derivation applies the
curl-curl identity, uses the matching divergence-free fact, and simplifies
`0 - ΔE` to `-ΔE`. Assumptions are author-supplied facts, not inferred from a
physical model. The layout pass presents vector operators with `∇`, `×`, and
`·`, then the HTML renderer emits MathML Core elements such as `mo`, `mi`,
`mrow`, and `mtable`. This output markup is MathML Core; the source expression
syntax remains Grove's own small grammar. The rules and parser remain
intentionally limited: this is a small relational rewrite system, not a
general CAS or a replacement for existing `@math` authoring syntax.

`(grove math types)` provides relational type judgments for scalar and vector
terms. Variables need explicit `scalar(x)` or `vector(E)` declarations;
numeric literals are scalar, and current judgments cover arithmetic, gradient,
curl, divergence, and Laplacian. Type-inconsistent expressions and conflicting
declarations do not infer a type. The curl-curl rule additionally requires
`smooth(E)`. These declarations let the system check that rule preconditions
are present and compatible with the expression; their truth remains the
author's responsibility.

Premises may be shared in a local scope:

```scheme
@with-assumptions["scalar(x)"]{
  @differentiate["x"]{x^3 + 2*x}
  @check-derive{differentiate(x,x)
1}
}
```

The Scheme equivalent is `(with-math-assumptions '("scalar(x)") body ...)`.
Nested scopes and a command's `#:assumptions` add to the current facts;
contradictions are diagnosed rather than replaced. Normal and exceptional
exits restore the parent scope. Each certificate stores the effective facts,
so later verification does not depend on a live context. For an independent
context, `call-with-math-assumptions` accepts `#:replace? #t`.

The supported facts are `scalar(variable)`, `vector(variable)`,
`smooth(expression)` and equations such as `div(E)=0`. Other expression shapes
return `invalid-assumption`. Computation and certificate replay check the entire
fact set for conflicting scalar/vector declarations, including unused variables.
This checks fact structure and declaration consistency, not the truth or general
logical consistency of asserted equations and smoothness. Display preferences
never become premises, and the facts retained on a computed value for display
are not imported into a subsequent computation.

The current rules are deliberately few. `curl-curl-identity` encodes
`curl(curl(E)) = grad(div(E)) - laplacian(E)` and requires matching
`vector(E)` and `smooth(E)` declarations. `divergence-free` requires a vector
declaration and an explicit structurally matching assumption such as `div(E)=0`.
`div-curl-zero` encodes `div(curl(E)) = 0` and requires matching vector and
smoothness declarations. `subtract-zero` applies the typed algebraic
identity `0 - X = -X`.
Two scalar-field identities require matching `scalar(f)` and `smooth(f)` facts:
`curl-grad-zero` rewrites `curl(grad(f))` to vector zero;
`div-grad-laplacian` rewrites `div(grad(f))` to scalar `laplacian(f)`.
For example, `@derive[#:assumptions "scalar(f); smooth(f)"]{curl(grad(f)) + zero(vector)}`
combines a differential identity with typed zero elimination. Both identities
support bounded backward search; zero-producing inverse queries enumerate only
explicitly declared fields. Operators describe ordinary Euclidean three-dimensional
vector calculus with classical smooth fields.
The displayed path and rule make a derivation inspectable, but do not amount to
a formal proof that the author's declared facts hold.
The rewrite module exposes a rule registry with each law, its side condition,
group and supported query modes (`forward`, `backward`, `check`). The
curl-curl rule's type and regularity declarations are checked for presence and
consistency by the type judgment, though their truth is not proved.

Rule declarations live with their implementations: typed arithmetic identities,
vector identities, exact arithmetic and polynomial differentiation each own a
rule family. `math-rule` uses one quasiquoted left side and premise template
for both its miniKanren relation and diagnostic matcher. Computed arithmetic
rules derive their matchers from the same transform that computes the result.
Calculation, collection, simplification and default backward selections are
derived from the declarations, rather than maintained as parallel name lists.
Explicit selection of an unsupported query mode is an argument error.
Differentiation and numerical calculation support forward generation and
checking; they do not offer inverse integration or arbitrary number generation.

For target-directed queries, `(grove math pipeline)` exports `math-search`.
It performs breadth-first search over the miniKanren one-step relation and
returns a search report with the shortest candidates first. `#:max-depth`,
`#:max-results`, `#:max-states`, and `#:max-term-size` bound derivation length,
returned candidates, explored states, and nodes per generated term. `#:rules`
selects registered rule names; an empty list disables all rules and an unknown
name is an argument error. Limits return the candidates found so far, rather
than discarding them or claiming no solution exists. Each candidate retains
its term sequence and named rewrite
steps, and `math-derivation->content` lowers a candidate to displayable
MathML IR. For example:

```scheme
(define report
  (math-search "curl(curl(E))" "-laplacian(E)"
    #:assumptions '("vector(E); smooth(E); div(E)=0")
    #:rules '(curl-curl-identity divergence-free subtract-zero)
    #:max-depth 3))
(math-report-reason report)
(map math-derivation->content (math-report-candidates report))
```

Report accessors expose `candidates`, `reason`, `limits`, and `states`.
The shared `(grove math report)` record also exposes the operation `name`,
structured `diagnostics`, named `passes`, and the retained `final` term.
Search, rewriting, calculation, collection, simplification and differentiation
all use this record. `(grove math pass)` owns deterministic execution, bounds,
cycle detection and certificate composition; search retains its BFS scheduler.
Reasons are `exhausted`, `depth-limit`, `term-size-limit`, `state-limit`, or
`result-limit`. The `limits` list retains all encountered cutoffs, and
`math-report-complete?` is true only for exhaustive search. Exhaustion
is relative to the selected rules and supplied assumptions. Multiple distinct
paths to the same term remain distinct candidates.

`math-simplify` is a separate strategy: it takes a source expression and
normalizes using arithmetic and scalar collection rules, with optional `#:rules` and
`#:max-steps`. It returns one candidate in the same report format, with reason
`normal-form` or `step-limit`; a limited report retains the partial chain.
For example, `(math-simplify "1 * (x + 0)" #:assumptions '("scalar(x)"))`
normalizes to `x` without selecting a target or expanding differential identities.
`math-derive` also accepts Scheme-level `#:rules` for its displayed automatic
derivation.

`math-calculate` runs exact constant arithmetic alone; `math-collect` combines
structurally identical scalar summands and numeric coefficients in products.
For example, `1/3 + 1/6` becomes `1/2`, and `2*x + 3*x` becomes `5*x`.
These are separate passes with ordinary replayable certificates; `math-simplify`
combines them with zero/one elimination. Fractions retain the existing division
IR, and signs retain unary minus. No floating point, product distribution, or
implicit reordering of symbolic multiplication is introduced.

`math-differentiate` returns a report containing the differentiation and
simplification pass reports, plus their joined certificate. Each pass has its
own `#:max-steps` budget and shares `#:max-term-size`. A cutoff retains the
completed prefix and does not run subsequent passes. Unsupported polynomial
inputs, type errors and division by zero return diagnostics; invalid API
arguments and syntax remain errors. `math-rewrite` exposes the report used by
the display-oriented `math-derive`. `math-completed-report->content` renders
only completed computations, which prevents `.plant` commands from presenting
a partial derivative as a completed result.

Search results are candidates. `math-verify-step` checks one recorded edge
against the named miniKanren rule and its exact operand path. For an entire
candidate, `math-verify-derivation` returns a verification record; inspect it
with `math-verification-valid?`, `math-verification-errors`, and
`math-verification-derivation`:

```scheme
(let ((candidate (car (math-report-candidates report))))
  (math-verification-valid? (math-verify-derivation candidate)))
```

Verification reruns each named relation with the candidate's stored
assumptions, checks the term chain and type judgments, and rejects rules whose
registered conditions are not marked as checked. It checks structural use of
author-supplied assumptions; it does not prove those assumptions true.

Rule regression tests also compare every registered forward rule with an
independent exact sparse-polynomial interpreter in three coordinates. This
checks arithmetic, differentiation and vector identities without calling Grove's
rewriting or differentiation implementation. The divergence-free fixtures first
establish their premise independently. These finite tests supplement replay;
they are not a proof over all smooth fields. Search tests independently enumerate
a small graph, check reproducibility and all budget boundaries, and exercise
cycles with an independently supplied transition graph.

This searches forward from a known expression to a target. The inverse query
`math-search-backward` takes the target first and a required source expression
second; it returns candidates as ordinary forward derivations. For example,
the curl-curl example can be searched from `-laplacian(E)` back to
`curl(curl(E))` using the same bounds and assumptions.
Backward search defaults to differential identities and `subtract-zero`.
Other inverse arithmetic expansions are available through explicit `#:rules`,
for example `#:rules '(add-zero)` to recover `x + 0` from `x`. Field-generating
inverses enumerate explicit declarations, and term size plus depth bound their
expansion. A limit is not a proof that no derivation exists.


## One expression for display and computation

```scheme
@use-modules[((grove math semantic) #:prefix semantic:)
             ((grove math notation) #:prefix notation:)
             (grove math pipeline)]
@(define p @semantic:expression{@semantic:power{x}{3} + 2*x})
@math[p]
@with-assumptions["scalar(x)"]{
  @differentiate["x" p]
  @search-derive[p]{@semantic:add[p "0"]}
  @(math-verification->content
     (math-check-chain (list (semantic:add p "0") p)))
}
@notation:math{@notation:parens[p]}
```

`expression` decodes text, embedded values, and lists of fragments. Explicit
constructors include `variable`, exact rational `constant`, `add`, `subtract`,
`negate`, `multiply`, `divide`, `power`, `grad`, `divergence`, `curl`, `laplacian`,
`derivative`, and `vector-zero`. `group` preserves explicit parentheses;
`styled` accepts `#:variant 'normal`, `'bold`, `'italic`, or `'bold-italic`.
Neither grouping nor style changes the underlying mathematical term.
Adjacent brace bodies form separate arguments for any at-exp command,
including prefixed commands; each compound argument remains a fragment list.

Construction and direct display never simplify expressions. The computation
APIs accept a `math-value`, source text, or a fragment sequence through the same
decoder. `math-report-result` returns the final term as a reusable mathematical
value, with the certificate's assumptions retained for display. This does not
install those assumptions into subsequent computations: use an explicit
assumption scope for calculations involving free variables.

Notation DSL provides `math`, `display-math`, `frac`, `sup`, `sub`, `vec`,
`parens`, and `align`. It can embed a semantic value for display. Notation
nodes cannot become mathematical operands, and decorating a variable with
an arrow does not declare its type. A semantic function application can be
rendered without a rule for calculating it; unsupported differentiation,
missing type/premise information, and absent computational semantics are
different failures. Decode errors raise before pass-level `#:on-error 'show`
can render a diagnostic.

## Authored derivations

`math-check-chain` checks the author's exact sequence of source expressions.
Each adjacent pair must be related by one selected rule at one operand path;
it does not silently search for and insert intermediate expressions.
Search candidates and authored chains use the same `math-derivation` certificate
with `terms`, `steps`, and the original `assumptions`. Independent replay lives
in `(grove math verification)`; it does not trust the strategy that produced a
candidate.

```scheme
(define checked
  (math-check-chain
    '("curl(curl(E))"
      "grad(div(E)) - laplacian(E)"
      "zero(vector) - laplacian(E)"
      "-laplacian(E)")
    #:assumptions '("vector(E); smooth(E); div(E)=0")))
(math-verification-valid? checked)
(math-verification->content checked)
```

The default infers each step's rule and path. Optional `#:steps` supplies one
annotation per transition: a rule symbol infers the matching path;
`(rule path)` fixes both; `#f` leaves that transition to inference.
`#:rules` restricts allowed rules, including explicit annotations. For example:

```scheme
(math-check-chain '("x + (y + 0)" "x + y")
  #:assumptions '("scalar(x); scalar(y)")
  #:steps '((add-zero (1))))
```

Operand paths are zero-based lists, so `(1)` selects the second operand;
the empty path `()` selects the root. Diagnostics display one-based positions.
`math-verification-diagnostics` returns structured diagnostics containing
`step`, `rule`, `path`, `code`, `premises`, and `message` accessors under the
`math-diagnostic-` prefix. `math-verification-errors` returns readable messages.
Errors distinguish missing premises, invalid paths, rule-shape and target
mismatches, unknown/disabled rules, and type failures. Checking continues after
a failed step. Missing-premise inference is diagnostic only: it never modifies
the certificate's assumptions or turns an unproved premise into a verified fact.
`math-verification->content` raises for invalid verification by default;
`#:on-error 'show` displays its diagnostics.

In a `.plant` document, `@check-derive` takes the assumption string in brackets
and one complete expression per nonempty body line:

```scheme
@check-derive[#:assumptions "vector(E); smooth(E); div(E)=0"]{
  curl(curl(E))
  grad(div(E)) - laplacian(E)
  zero(vector) - laplacian(E)
  -laplacian(E)
}
```

Successful chains render the authored lines as annotated MathML. Failed chains
stop document evaluation with the step diagnostics. Scheme-level
`math-check-chain` provides explicit annotations and programmatic error handling.

## Article presentation and source diagnostics

The presentation pass in `(grove math presentation)` is independent of
computation and certificate replay. The default `#:view 'compact` shows the
starting expression and endpoint with a native HTML disclosure for all steps.
`#:view 'full` displays every step immediately; `#:view 'result` shows only the
two endpoints. Differentiation details preserve the separate differentiation
and simplification passes. Text output retains all steps of a compact view.
Rule labels are readable descriptions; HTML retains rule identifiers and
operand paths as `data-rule` and `data-path` attributes. Located step annotations
also retain the rewritten operand's source as `data-source` and a hover title.

```scheme
@with-assumptions["scalar(x)"]{
  @differentiate["x"]{x^3 + 2*x}
  @differentiate["x" #:view 'result]{x^3 + 2*x}
  @check-derive[#:view 'full]{differentiate(x,x)
1}
}
@check-derive[#:assumptions "scalar(x)" #:on-error 'show]{
  differentiate(x,x)
  2
}
```

Failed verification or incomplete differentiation raises by default.
`#:on-error 'show` renders diagnostics instead of claiming a completed equality.
`@differentiate` also accepts `#:max-steps` and `#:max-term-size`;
`@search-derive` accepts `#:max-depth` and `#:max-states`. Search reports always
retain their stopping reason and distinguish exhaustive results from cutoffs.

Located `.plant` inputs carry source metadata separately from mathematical
terms. `math-diagnostic-source` returns a `math-location` or `#f`; its accessors
provide `file`, one-based `line` and `column`, and a source `excerpt`.
Article diagnostics expose the filename and position in an expandable source
snippet; raised errors include the full source path. Authored certificates
retain origins for independent replay. `(grove math provenance)` propagates an
origin tree through each rewrite and across pass boundaries. Unchanged operands
keep their locations; structurally copied operands keep their matching input
origins; new nodes record the producing rule and rewritten input. If identical
input occurrences cannot be distinguished, `math-origin-locations` retains all
matching locations instead of inventing a unique copy correspondence.
`math-origin-generated?` and `math-origin-rule` identify generated nodes.
`math-diagnostic-origin` exposes the relevant origin; generated diagnostic
snippets explain which rule produced the term. `math-report-result` carries its
final origin into further computations. Computed strings without reader metadata
remain unlocated, including unlocated siblings of authored operands.
