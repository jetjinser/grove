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
`@display-math` use Semantic DSL by default. Examples use `◆:` for Semantic DSL
and `◇:` for Notation DSL; these are author-chosen module import prefixes.

Coordinates, constant parameters and function references may be defined as
reusable values. Their declarations are separate from source origins and display
preferences, and enter the context only through an explicit scope:

```scheme
@(define x (◆:coordinate 'x))
@(define a (◆:parameter 'a))
@(define f (◆:function 'f #:inputs '(scalar) #:result 'scalar))
@with-assumptions[(list x a f)]{
  @math{@◆:call[f]{x^2}}
  @differentiate[x]{@◆:multiply[a x]}
}
```

The equivalent facts are `coordinate(x)`, `parameter(a)` and
`function(f,scalar,scalar)`; a function declaration lists input types followed
by its result type. Signatures may have several scalar/vector inputs or no
inputs. Calls use either `f(x)` or `◆:call`; typing checks their arity and operand
types. A function reference is not a scalar variable. Plain `scalar(x)` denotes
an independent scalar variable; `parameter(a)` explicitly holds `a` constant
and rejects differentiation with respect to it. Declaring `f` does not supply
its definition or a differentiation rule. Unknown undeclared calls remain
displayable, while calculations diagnose the missing computational semantics.

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
`opaque(x)` remains representable and displayable, but computation, search and
verification return `unsupported-expression`. Missing declarations and
incompatible operand types return `type-error`. Supported terms with no selected
rewrite remain normal forms; that does not mean all their operators were
numerically evaluated. Scalar differentiation has a narrower domain and
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
| `math-normalize` | Real scalar sums and products: stable ordering, coefficient collection, factor powers and nested natural powers. Vector operands are not reordered. |
| `math-expand` | Explicit distribution of scalar products over sums and differences, followed by simplification. |
| `math-differentiate` | Real scalar arithmetic with natural-number powers, quotients, `sin`, `cos`, `exp`, `log`, and declared smooth unary scalar functions; differentiation is followed by simplification. |

Real quotients require `nonzero(denominator)` and logarithms require
`positive(argument)`. Exact constants, positivity implying nonzero, positive
exponentials and nonzero products/powers discharge these small domain obligations
without additional assertions. Premises apply to the whole source expression,
including factors which a later simplification would discard. Contradictory
constant sign premises and sign premises on vector expressions are rejected.

For example, with `coordinate(x); parameter(a); positive(x)`, derivatives of
`sin(x^2)`, `exp(a*x)` and `log(x)` simplify to `2*x*cos(x^2)`, `a*exp(a*x)`
and `1/x`. The constructors `◆:sin`, `◆:cos`, `◆:exp` and `◆:log` accept
the same embedded expressions as other Semantic commands.

A declared `function(f,scalar,scalar)` additionally needs `smooth(f)` for
differentiation. Its chain rule produces `prime(f,u)`, rendered as f′(u);
`◆:prime` is its Semantic constructor. This is a formal derivative of the
declared function, not an evaluation of its unknown body. Higher derivatives
and multivariate function differentiation are currently unsupported. Natural
power computation uses the polynomial convention u^0 = 1; negative and symbolic
exponents remain displayable but are outside this computation domain.

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

The supported facts include variable roles and function signatures, Cartesian
spaces and fields, `smooth(expression)`, `differentiable(expression)`, scalar
sign/domain premises and equations such as `div(E)=0`. Other expression shapes
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
Two scalar-field identities require a scalar operand and its smoothness:
`curl-grad-zero` rewrites `curl(grad(f))` to vector zero;
`div-grad-laplacian` rewrites `div(grad(f))` to scalar `laplacian(f)`.
For example, `@derive[#:assumptions "scalar(f); smooth(f)"]{curl(grad(f)) + zero(vector)}`
combines a differential identity with typed zero elimination. Both identities
support bounded backward search; zero-producing inverse queries enumerate only
explicitly declared fields. Operators describe ordinary Euclidean three-dimensional
vector calculus with classical smooth fields.

Explicit fields use an ordered Cartesian space with three independent coordinate
axes. A field's declaration records that coordinate order and its scalar or
vector result. This is the standard oriented Euclidean R3 model, not arbitrary
dimension, a curved coordinate chart or a manifold.

```scheme
@(define r3 (◆:space 'x 'y 'z))
@(define f (◆:field 'f #:space r3))
@(define e (◆:field 'E #:space r3 #:result 'vector))
@with-assumptions[(list r3 f e "differentiable(f); differentiable(E)")]{
  @derive{div(f(x,y,z)*E(x,y,z))}
}
```

The corresponding facts are `space(x,y,z)`, `field(f,scalar,x,y,z)` and
`field(E,vector,x,y,z)`. A space value carries the coordinate declarations;
a field value carries its field declaration. Explicit scope activation remains
necessary. Field references have function type; applications take three scalar
arguments and return their declared result. `◆:call` constructs applications,
and textual calls may compose the arguments, such as `f(y,x,z)` or `f(x^2,y,z)`.
Constant parameters remain distinct from coordinates and fields. Conflicting
spaces, duplicate axes, parameter axes, mismatched signatures and use of a bare
field reference as a scalar/vector value are diagnosed. Field computations and
reused computed values retain their declared spatial context and the premises
actually used by their derivation. A result of a C1 product rule cannot silently
be reused in a scope lacking that regularity. Each search candidate retains only
its own rule premises; alternative search paths do not impose extra requirements.

`differentiable` asserts C1 regularity. `smooth` asserts C-infinity regularity,
which implies C1. Regularity propagates from a declared function reference to
its application when its arguments have the required regularity, and through
supported algebraic/elementary compositions. A C1 declaration does not license
the second derivative identities. Smooth spatial derivatives remain smooth;
C1 spatial derivatives are not automatically C1 themselves.

The first-order product rules are:

```text
grad(f*g) -> g*grad(f) + f*grad(g)
div(f*E)  -> dot(grad(f),E) + f*div(E)
curl(f*E) -> cross(grad(f),E) + f*curl(E)
```

They require typed C1 operands, and support forward rewriting, target search
and step checking. Missing assumptions are diagnostic suggestions only; they
never enable a transition. `dot` maps two vectors to a scalar, and `cross` maps
two vectors to a vector with the declared orientation. `◆:dot` and `◆:cross`
render through Notation constructors, with grouped operands as needed.
Ordinary `*` retains scalar multiplication semantics; multiplying two vectors
is a type error. There is no vector component expansion, coordinate partial
derivative solver, PDE solving or physical-model inference in this stage.
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
IR, and signs retain unary minus. Arithmetic remains exact.

`math-normalize` is an independent pass. It gives scalar sums and products a
stable left-associated order, combines equal monomials and repeated factors,
and collapses nested natural powers. Variables precede opaque function factors;
summands are sorted by decreasing coordinate degree, then by their source
representation, with constants last. These choices make repeated normalization
idempotent; they do not identify every mathematically equivalent expression.
The same rule is included in `math-simplify` and derivative simplification.
For example, `x+2*x+x` becomes `4*x`, and `2*x*3*x^2` becomes `6*x^3`.

Scalar factor cancellation requires the original denominator to be nonzero.
Under `coordinate(x); nonzero(x)`, `x/x` becomes `1`, and differentiation of
`x/x^2` simplifies to `-1/x^2`. Cancellation handles exact coefficients and
structurally equal factors with natural powers; it does not factor arbitrary
polynomials. Factored structure remains the default. `math-expand` explicitly
distributes products over sums/differences in a separate pass and then simplifies;
it is never enabled by default, nor does it expand arbitrary binomial powers.

Computed values and derivation certificates retain domain requirements separately
from declarations and display properties. `math-value-requirements` and
`math-derivation-requirements` expose them. A value obtained from `x/x` retains
`nonzero(x)` after its visible term becomes `1`, including when embedded in another
expression, styled, differentiated, searched or used in an authored chain.
Reusing it requires the premise again; carrying a requirement never authorizes
that premise. Certificate replay checks the retained requirements as well.

`math-differentiate` returns a report containing the differentiation and
simplification pass reports, plus their joined certificate. Each pass has its
own `#:max-steps` budget and shares `#:max-term-size`. A cutoff retains the
completed prefix and does not run subsequent passes. Unsupported differentiation
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
@use-modules[((grove math semantic) #:prefix ◆:)
             ((grove math notation) #:prefix ◇:)
             (grove math pipeline)]
@(define p @◆:expression{@◆:power{x}{3} + 2*x})
@math[p]
@with-assumptions["scalar(x)"]{
  @differentiate["x" p]
  @search-derive[p]{@◆:add[p "0"]}
  @(math-verification->content
     (math-check-chain (list (◆:add p "0") p)))
}
@◇:math{@◇:parens[p]}
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
`@derive` and `@differentiate` accept `#:max-steps` and `#:max-term-size`.
`@derive` also accepts `#:rules`; it requests at least one transition and reports
`no-rule` if none applies under the supplied premises. Use `@math` to display an
expression without requesting a derivation.
`@check-derive` accepts `#:rules` and optional `#:steps`, one rule or
`(rule operand-path)` per adjacent pair.
`@search-derive` accepts `#:rules`, `#:max-depth`, `#:max-states`,
`#:max-term-size`, and `#:max-results` (default 1). `#:backward? #t` runs inverse
search using the same target/body order and displays a forward certificate:

```scheme
@search-derive["x" #:backward? #t #:rules '(add-zero)
              #:assumptions "scalar(x)" #:max-depth 1]{x+0}
```

Search reports always
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

The `(grove math catalog)` module exposes `(ruleo name group modes documentation)` for
finite relational queries over the complete rule registry. For example:

```scheme
(use-modules (grove math catalog) (grove relation dsl))
(map (lambda (row) (math-rule->content (car row)))
     (query (name group modes documentation) (ruleo name group modes documentation)))
```

`math-rule->content` renders an inference rule in native MathML. Declarative
rules retain their left/right templates, premises, and executable guards from
`math-rule`; metavariables are display placeholders, not solver inputs. Extra
Scheme guards appear in an expandable section. Rules implemented by algorithms
are explicitly labeled and show their description rather than an invented
fixed rewrite schema. This catalog describes the implementation, not a formal
proof of the rules. Types, domain checks, and scoped declarations still apply
when executing a rule.

Rules may supply `#:documentation` to `math-rule` or
`make-math-rewrite-rule`. The string becomes the execution procedure's native
Guile docstring; existing lambda docstrings also work without this keyword.
`math-rewrite-rule-documentation` reads it through `procedure-documentation`.
The fourth field of `ruleo` queries documentation in either direction, and
`math-rule->content` displays the explanation. Undocumented rules return `#f`;
add `(=/= documentation #f)` to select only documented rules. Documentation is prose only;
execution continues to use the structured schema and executable relation.

Numeric coefficients may omit `*`: `6x`, `6x^2`, `6(x+1)`, `6sin(x)` and
`-6x` lower to ordinary multiplication terms. Juxtaposition has the same
precedence as explicit multiplication; powers bind more tightly. Whitespace
is allowed after a coefficient. Multi-letter identifiers such as `xy` remain
single variable names, and `f(x)` remains a function call. Ambiguous forms such
as `1/2x` are rejected: write `(1/2)*x` or `1/(2x)` explicitly.
Semantic rendering prefers coefficient juxtaposition, including for explicitly
written products. Numeric products and numeric-base powers retain the
multiplication sign; dot and cross products retain their own symbols.

## Cartesian components and partial derivatives

`◆:components` constructs a vector with exactly three scalar entries in an
explicit ordered Cartesian space. Its IR is `(components x-term y-term z-term)`;
the chosen space travels as a requirement of the mathematical value, independently
of its component terms and display properties. Construction does not activate
that space. A raw textual `components(x,y,z)` uses the active space, and its
computed result retains that basis too. Mixing bases or using a vector as an
entry is diagnosed before computation. Scalar passes may simplify the entries; `math-compute` additionally lowers
component-wise vector operations and Cartesian spatial operators.

```scheme
@(define r3 (◆:space 'x 'y 'z))
@(define v (◆:components "x^2" "y" "sin(z)" #:space r3))
@with-assumptions[r3]{@math[v]}
```

Semantic components lower through the Notation DSL's column constructor to
native MathML `mtable` rows enclosed in parentheses. `◇:column{a}{b}{c}` is
available for layout only, with no vector semantics. Text output separates
component entries with commas.

`◆:partial` constructs `(partial expression coordinate)` without evaluating it.
`math-partial` computes the scalar derivative in a dedicated pass and runs
scalar simplification as a second pass. The article command is
`@partial["x"]{x^3+2x*y}`. Both use explicit `coordinate(x)` declarations;
a generic `scalar(x)` declaration does not establish an independent coordinate.
Other declared coordinates and constant parameters differentiate to zero.
A bare scalar field without either role remains a symbolic partial, rather
than being silently treated as an independent coordinate.

```scheme
(math-partial "x^3+2x*y+a" "x"
  #:assumptions '("coordinate(x); coordinate(y); parameter(a)"))
;; final term: 3*x^2 + 2*y
```

Partial rules adapt the same scalar calculus relations used for ordinary
polynomials, natural-number powers, quotients, elementary functions and smooth
unary function composition. They are registered as `partial-*`, with forward
and check modes. Search, authored checking, inference-rule rendering and
certificate replay use these registered relations. Each pass has its own step
budget; term-size limits and source provenance follow the existing pipeline.

Declared unknown multivariate scalar functions remain differentiable symbolic
terms, provided their C1 premise is supplied. For example, under
`function(f,scalar,scalar,scalar); differentiable(f)` and independent `x,y`,
`math-partial "x*f(x,y)" "x"` produces
`x*partial(f(x,y),x)+f(x,y)`. This is a symbolic normal form, not a calculated
component of the unknown function. No multivariate function chain rule or
argument-index derivative is inferred. C1 alone does not authorize second
unknown derivatives. Undeclared operators, missing regularity, invalid types
and missing domains produce diagnostics instead of symbolic success.

Coordinate and parameter roles, field/component spaces, regularity and real
domain requirements remain attached to computed values and certificates even
when the visible term reduces to zero or one. Reuse must satisfy those
requirements again; documentation and retained requirements never authorize
new assumptions.

Cartesian computation uses `math-compute` and `@compute`, with an explicit
ordered `space` premise. It lowers component addition, subtraction, negation,
scalar multiplication/division, dot/cross products and grad/div/curl/Laplacian
in a `components` pass, evaluates partials with local simplification in a
`partial` pass, and finishes with `simplify`. Each pass has a separate budget;
all edges are registered rules and replayable. First-order operators require
C1 regularity; Laplacian currently requires smoothness. Unspecified vector
components produce a diagnostic. Unknown scalar derivatives remain explicit.
These are Euclidean Cartesian operations, not curvilinear-coordinate formulas.
Results retain their ordered basis and the coordinate/parameter roles used.
Independent sparse-polynomial tests check every Cartesian rule and every
recorded computation edge without invoking Grove's calculus implementation.
