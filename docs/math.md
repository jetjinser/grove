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
scalar one from either side of multiplication. These six rules apply to scalar
and vector terms and strictly reduce term size. They are forward simplifications;
backward search does not insert zero/one decorations by default. Each transition
still records its rule and operand path.

Grove keeps mathematical meaning separate from mathematical presentation.
The current content IR nodes (`math-frac`, `math-sup`, `math-align`, and
`math-token`) describe layout and lower to MathML. The new `(grove math ...)`
modules provide an independent path for expressions that need structural
processing:

```text
source → tokens → syntax tree → symbolic term → relation rewrite → layout IR → MathML
```

`(grove math syntax)` tokenizes a deliberately small expression grammar and
parses it with source spans. `(grove math term)` maps that syntax tree to
Scheme s-expressions such as `(curl (curl (var E)))`; this is the term grammar
used by relational rules. `(grove math rewrite)` defines miniKanren relations
over those terms. `(grove math strategy)` selects rules and implements bounded
search and bounded normalization; `(grove math derivation)` stores their
certificates. `(grove math layout)` lowers terms to the existing math
layout nodes. `(grove math pipeline)` composes these passes.

The initial parser accepts identifiers, integer numerals, function calls,
parenthesized groups, unary minus, superscripts (`x^2`), subscripts (`x_i`),
and the operators `=`, `+`, `-`, `*`/`×`, and `/` with conventional
precedence. Scripts attach in source order; parentheses group compound script
expressions. It does not infer implicit multiplication or consume general
`@math` commands. Matrices and aligned derivations remain explicit structures
in `@math`, because their row, cell, and alignment markers carry layout
meaning that cannot be reliably guessed from plain text.

`@derive{...}` applies available relations repeatedly to the expression and
shows each intermediate term, joined with equality signs. Each transition is
annotated with the rule name and the operand path where it matched (`root`, or
one-based operand positions). Its bracket argument declares semicolon-separated
assumptions. For example,
`@derive["vector(E); smooth(E); div(E)=0"]{curl(curl(E))}` declares `E` to be
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
For example, `@derive["scalar(f); smooth(f)"]{curl(grad(f)) + zero(vector)}`
combines a differential identity with typed zero elimination. Both identities
support bounded backward search; zero-producing inverse queries enumerate only
explicitly declared fields. Operators describe ordinary Euclidean three-dimensional
vector calculus with classical smooth fields.
The displayed path and rule make a derivation inspectable, but do not amount to
a formal proof that the author's declared facts hold.
The rewrite module exposes a rule registry with each law, its side condition,
and whether the current implementation checks that condition. The
curl-curl rule's type and regularity declarations are checked for presence and
consistency by the type judgment, though their truth is not proved.

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
`math-verification->content` accepts only a valid verification.

In a `.plant` document, `@check-derive` takes the assumption string in brackets
and one complete expression per nonempty body line:

```scheme
@check-derive["vector(E); smooth(E); div(E)=0"]{
  curl(curl(E))
  grad(div(E)) - laplacian(E)
  zero(vector) - laplacian(E)
  -laplacian(E)
}
```

Successful chains render the authored lines as annotated MathML. Failed chains
stop document evaluation with the step diagnostics. Scheme-level
`math-check-chain` provides explicit annotations and programmatic error handling.
