<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Mathematics in Grove

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

```scheme
@use-modules[((grove math semantic) #:prefix ◆:)
             ((grove math formula) #:prefix φ:)]

@(define x (◆:coordinate 'x))
@with-assumptions[x]{
  @math{x^3 + 2*x}
  @differentiate[x]{x^3 + 2*x}
}
```

`@math` displays the expression. `@differentiate` computes and displays its
derivative. Here `x` is explicitly declared as an independent coordinate.

Use the Formula reader when building a value in Scheme:

```scheme
@(define p (φ:read{x^3 + 2*x}))
@(φ:differentiate p "x" #:assumptions '("coordinate(x)"))
```

Use Semantic constructors when an expression is easier to build directly:

```scheme
@(define p (◆:add (◆:power x (◆:constant 3))
                   (◆:multiply (◆:constant 2) x)))
@math[p]
```

Formula text accepts identifiers, integer numerals, function calls,
parentheses, unary minus, powers, equations, and the operators `+`, `-`, `*`,
`/` (also `−`, `×`). It follows conventional precedence. Numeric coefficients
may omit `*`: `6x`, `6x^2`, `6(x+1)`, `6sin(x)`. Other adjacent identifiers
remain one name (`xy`), and `f(x)` is a function call. Ambiguous `1/2x` is
rejected; write `(1/2)*x` or `1/(2x)`.

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

For example, a three-dimensional space and vector field can be declared and
used as follows:

```scheme
@(define r3 (◆:space 'x 'y 'z))
@(define e (◆:field 'E #:space r3 #:result 'vector))
@with-assumptions[(list r3 e "smooth(E)")] {
  @derive{curl(curl(E))}
}
```

The curl-curl identity rewrites this to `grad(div(E)) - laplacian(E)`. If the
article also supplies `div(E)=0`, the result simplifies further to
`-laplacian(E)`. Grove uses the explicitly ordered Cartesian axes; it does not
implement curvilinear coordinates, arbitrary dimensions, or general PDE
solving.

## Derivations and checks

`@derive{expression}` applies available rewrite rules and displays each step.
Transitions identify their rule and the location within the expression where
they applied. Supply assumptions in its argument when required:

```scheme
@derive[#:assumptions "vector(E); smooth(E); div(E)=0"]{curl(curl(E))}
```

`@search-derive` searches within configured bounds for a path to a target.
`@check-derive` checks a hand-written sequence against the registered rules.
`@check-equation` instead checks an equation by reducing its residual. These
checks establish only what the supported rules and calculation domain justify;
they do not prove assumptions or validate an external mathematical model.

Derivation commands can show the result alone, a compact view, or the full
steps. Long step lists are presented in a collapsible subtree. If a rule cannot
apply or a required premise is missing, `#:on-error 'show` displays the
calculation diagnostic in the article. Authored Formula expressions retain
source locations through parsing and computation where possible, so diagnostics
can point back to the relevant source text.

## Display notation

Use Formula text for ordinary expressions. Use Semantic constructors to supply
meaningful operations and values. Use Notation DSL for layout that has no
calculation semantics, for example a fraction, matrix, or aligned derivation:

```scheme
@use-modules[((grove math notation) #:prefix ◇:)]

@◇:math{
  @◇:frac{a+b}{c}
}
```

Notation is lowered to MathML for output and is not parsed back into a semantic
expression. When an article needs both a calculation and a special layout,
compute with Semantic values, then present the result with notation.

## Current boundaries

The calculation engine is a small, typed relational rewrite system with bounded
passes. It handles a deliberately limited set of scalar algebra, differentiation,
Cartesian vector calculus, exact polynomial integration, and a restricted
Poisson problem. Unsupported expressions, missing declarations, invalid types,
and unmet domain requirements produce diagnostics rather than silently
claiming a result.

Polynomial integration uses exact rational coefficients and natural powers.
The returned primitive fixes its value to zero at the selected coordinate's
origin; a general primitive may additionally contain any coordinate-independent
term. The Poisson operation returns one selected polynomial particular
solution, not the family of solutions, boundary-value solution, or homogeneous
part. General symbolic integration, PDE solving, and general matrix algebra are
outside the current system.
