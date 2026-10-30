<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# Hackable math experiment

This project-side experiment checks whether Grove's extension points can carry
Typst, LaTeX, and SMathML source in `.plant` articles. It is not a Grove core
API. The sample is [math.plant](math.plant); the implementation is
[extension.scm](extension.scm); executable checks are in
[`tests/unit/forester/hackable-math.scm`](../../tests/unit/forester/hackable-math.scm).

The unit test uses stand-in executables and does not need the external tools.
To run all three real adapters, enter Grove's optional Guix environment with
`guix shell -m manifest.scm -m integration-manifest.scm` from the repository
root. That manifest provides Typst, Racket with the SMathML collection, and a
minimal LaTeX kernel plus `amsmath` and `dvisvgm`.

`nix develop .#integrations` provides Typst, `racket-minimal`, and the LaTeX
tools, but does not package SMathML. The Nix environment can run the Typst and
LaTeX adapters; use the Guix integration manifest for `@smathml`.

The built-in Typst and LaTeX asset adapters include their executable versions
in the renderer cache identity. A custom compiler passed to
`make-math-asset-renderer` must provide a stable `#:compiler-id`; include any
external tool versions or options that can change its output.
