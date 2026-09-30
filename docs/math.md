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
parenthesized groups, unary minus, and the binary operators `+`, `-`, `*`/`×`,
`/`, and `^` with conventional precedence. It does not infer implicit
multiplication or consume general `@math` commands yet.

The initial `@derive{...}` command exercises one complete path: it parses
`curl(curl(E))`, applies the curl-curl identity relation, and displays both
terms as a MathML alignment. The parser and layout lowering are intentionally
limited; this is a foundation for focused passes, not a general CAS or a
replacement for the existing `@math` authoring syntax.
