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
smoothness declarations. `subtract-zero` applies the unconditional algebraic
identity `0 - X = -X`.
The displayed path and rule make a derivation inspectable, but do not amount to
a formal proof that the author's declared facts hold.
The rewrite module exposes a rule registry with each law, its side condition,
and whether the current implementation checks that condition. The
curl-curl rule's type and regularity declarations are checked for presence and
consistency by the type judgment, though their truth is not proved.

For target-directed queries, `(grove math pipeline)` exports `math-search`.
It performs breadth-first search over the miniKanren one-step relation and
returns the shortest candidates first. `#:max-depth`, `#:max-results`, and
`#:max-states` bound derivation length, returned candidates, and explored
states; hitting the state limit reports an error rather than claiming that no
solution exists. Each result retains its term sequence and named rewrite
steps, and `math-search-result->content` lowers a candidate to displayable
MathML IR. For example:

```scheme
(map math-search-result->content
     (math-search "curl(curl(E))" "-laplacian(E)"
       #:assumptions '("vector(E); smooth(E); div(E)=0")
       #:max-depth 3))
```

Search results are candidates. `math-verify-step` checks one recorded edge
against the named miniKanren rule and its exact operand path. For an entire
candidate, `math-verify-derivation` returns a verification record; inspect it
with `math-verification-valid?`, `math-verification-errors`, and
`math-verification-derivation`:

```scheme
(let ((candidate
        (car (math-search "curl(curl(E))" "-laplacian(E)"
               #:assumptions '("vector(E); smooth(E); div(E)=0")
               #:max-depth 3))))
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
