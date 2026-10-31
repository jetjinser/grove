<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Mathematical system: purpose and capability map

Grove's math system is for writing mathematics that can be read, transformed,
and checked inside an ordinary Scheme document project. Its goal is dependable,
explainable symbolic work over declared domains, with Scheme as an extension
language. It is not intended to cover every operation offered by a general
computer algebra system.

The [Math reference](math.md) lists the current syntax, APIs, and supported
calculation domains. This page describes how those capabilities fit together
and how to judge future additions.

## User tasks and guarantees

| What the author wants to do | Main interface | What Grove can provide | What the result does not claim |
| --- | --- | --- | --- |
| Write a formula and display it | Formula or Semantic, then math presentation | A shared semantic expression that can be rendered as mathematics | Display notation by itself does not acquire calculation semantics |
| Calculate an expression | `calculate` for exact arithmetic; `compute` for composed domain passes | A result, the executed steps, diagnostics, and separate audit evidence | A completed calculation is not automatically a proof of an external claim |
| Derive a result from rules | `derive`, `search`, or an authored derivation | Rule-labelled transitions that can be replayed under supplied assumptions | Finding or replaying a path does not establish that its assumptions are true |
| Check a proposed equality or chain | `check-equation`, `check-chain`, and semantic checkers | A residual or a check status within the selected rules and checker domain | `unsupported` is not evidence for or against the claim; assumptions are author-supplied |
| Add domain-specific mathematics | Guile/Scheme constructors, relations, rules, and passes | An extension can compose with the ordinary expression and report model | A custom operator has no built-in semantics merely because it can be displayed |

These interfaces answer different questions. Calculation chooses and runs an
algorithm; derivation search explores registered rules; verification replays
recorded steps; semantic checking independently evaluates a supported claim.
Reports should preserve those distinctions instead of compressing them to one
success boolean.

## Current capability envelope

The current system has a useful, deliberately bounded core:

- Formula, Semantic, and Notation provide text input, typed mathematical
  values, and display layout over a shared expression representation.
- Exact arithmetic and symbolic rewriting handle supported scalar
  expressions. Differentiation covers real elementary functions and declared
  smooth functions; partial derivatives can remain symbolic when a function
  body is unavailable.
- Exact polynomial algorithms provide scalar primitives and a selected
  particular solution for a three-dimensional Cartesian Poisson equation.
- Cartesian passes operate on explicit three-component vectors and an ordered
  basis, including gradient, divergence, curl, Laplacian, dot, and cross
  operations.
- Derivations, proof trees, replay verification, semantic checkers, and
  calculation audits expose different kinds of evidence. Each checker states
  a narrower domain than the language of displayable expressions.

These evidence layers have intentionally different scopes. Replay checks
whether recorded steps follow their selected rewrite rules. The independent
semantic checker covers exact scalar-polynomial and explicit Cartesian
polynomial contracts. Proof-tree elaboration can check supported fixed rule
specifications and equality composition, but does not prove algorithm-wide
correctness and can report `unsupported` for otherwise successful calculus or
Cartesian calculations. A positive result in one layer remains useful when
another layer is unsupported; the report keeps them separate.

For example, a completed `curl(grad(x*y))` calculation can have a replay-valid
trace and a semantically checked Cartesian contract while proof-tree
elaboration remains unsupported. That says the calculation passed the
evidence Grove currently provides; it does not say a general proof tree exists
for every internal calculation step.

The [Math reference](math.md#available-calculations) is authoritative for
operation-by-operation conditions, limits, and examples. General symbolic
integration, general ODE/PDE solving, and general matrix algebra are outside
the current envelope.

## Design criteria for additions

A new capability should improve a recognizable writing or calculation task
and satisfy these criteria:

1. **Explicit meaning.** The expression's type, coordinate roles, domain, and
   regularity requirements are represented as values or assumptions. Names
   alone do not silently imply mathematical roles.
2. **Composable results.** A result remains an ordinary mathematical value
   that can be passed to another calculation, displayed, or inspected from
   Scheme. Its origin and requirements survive the composition where they
   affect meaning.
3. **Inspectable execution.** The report identifies the outcome, result,
   steps, diagnostics, and available audit evidence. Bounded search or
   calculation does not present a partial prefix as exhaustive.
4. **Honest checking.** A checker distinguishes established, rejected, and
   unsupported claims. It checks only its documented semantics and does not
   certify author-supplied assumptions.
5. **A practical extension seam.** Users can add domain-specific expressions
   and algorithms in Scheme without making every such domain a built-in Grove
   feature. Where Grove claims independent checking, that checker needs its
   own stated domain and meaningful cross-checks.

The common public shape to preserve is:

```text
mathematical value + explicit context
    -> structured report (outcome, result, trace, diagnostics, audit evidence)
```

This is a design criterion, not a requirement that every operation use one
universal algorithm or one opaque report status. Specialized algorithms remain
appropriate when their input domain, result convention, and verification
conditions are visible to the author.

## Direction of work

The next useful work is to keep the public contract coherent across existing
capabilities: make it easy to tell which operations calculate, which produce
derivations, and which independently check results; ensure their assumptions,
limits, and outcomes compose predictably. This should precede adding a broad
new family of operators.

Future expansion should happen one mathematical domain at a time. A candidate
domain should have a concrete author task, an explicit semantic model, a
bounded implementation strategy where needed, and tests that compare results
against independently stated identities or an independent evaluator. If that
domain benefits from verification, its checker should be introduced with a
clear unsupported boundary. This keeps the system hackable while preserving
the ability to explain what any built-in result means.

## Decision: keep proof-tree scope bounded

A public calculation audit currently combines three kinds of evidence: rule
replay, an operation-specific semantic contract when available, and proof-tree
elaboration when its fixed rule theory supports the steps. A tested Cartesian
calculation can complete with valid replay and a `semantically-checked`
Cartesian contract while proof-tree elaboration reports `unsupported`. The
calculation and its semantic contract still have positive evidence; the
general proof-tree layer simply does not cover every internal algorithm step.

Keep these layers separate and keep proof-tree coverage bounded to its explicit
RuleSpec theory. Reconsider Cartesian proof trees when an author task needs to
compose vector identities as proof objects, rather than calculate a result and
check its Cartesian contract. That extension would need an explicit typed
vector proof theory and its own semantic checker, not a status-label
adjustment. The scalar-polynomial step checker classifies a well-typed vector
context as `unsupported` and reserves `rejected` for malformed, ill-typed, or
contradicted evidence within its contract.
