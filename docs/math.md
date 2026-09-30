<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Math processing

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
over those terms. `(grove math layout)` lowers terms to the existing math
layout nodes. `(grove math pipeline)` composes these passes.

The initial parser accepts identifiers, integer numerals, function calls,
parenthesized groups, unary minus, and the binary operators `=`, `+`, `-`,
`*`/`×`, `/`, and `^` with conventional precedence. It does not infer implicit
multiplication or consume general `@math` commands yet.

`@derive{...}` applies available relations repeatedly to the expression and
shows each intermediate term, joined with equality signs. Each transition is
annotated with the rule name and the operand path where it matched (`root`, or
one-based operand positions). Optional bracket arguments declare assumptions, for example
`@derive["div(E)=0"]{curl(curl(E))}`. This derives the curl-curl identity,
uses the matching divergence-free assumption, and simplifies `0 - ΔE` to
`-ΔE`. Assumptions are matched structurally; they are not inferred from a
physical model. The layout pass presents vector operators with `∇`, `×`, and
`·`, then the HTML renderer emits MathML Core elements such as `mo`, `mi`,
`mrow`, and `mtable`. This output markup is MathML Core; the source expression
syntax remains Grove's own small grammar. The rules and parser remain
intentionally limited: this is a small relational rewrite system, not a
general CAS or a replacement for existing `@math` authoring syntax.

The current rules are deliberately few. `curl-curl-identity` encodes
`curl(curl(E)) = grad(div(E)) - laplacian(E)` and assumes `E` is a vector
field; the current term IR does not verify that type. `divergence-free`
requires an explicit structurally matching assumption such as `div(E)=0`.
`subtract-zero` applies the unconditional algebraic identity `0 - X = -X`.
The displayed path and rule make a derivation inspectable, but do not amount to
a formal proof that the rule's mathematical side conditions hold.
